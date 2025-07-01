require(magick)
require(pdftools)
require(dplyr)
require(cli)
require(tidyverse)
require(stringr)
require(tesseract)
require(imager)
require(grid)

Sys.setenv(MAGICK_MEMORY_LIMIT = "2GB")
Sys.setenv(MAGICK_MAP_LIMIT = "4GB")
Sys.setenv(MAGICK_DISK_LIMIT = "8GB")
# system("convert -list resource")



find_region <- function(pdf_path,
                        page_num = 1,
                        test_regions = data.frame(
                          x = c(390, 1290),
                          y = c(425, 425),
                          width = c(700, 500),
                          height = c(150, 150),
                          label = c("Name Region", "ID Region")
                        ),
                        density = 400) {
  # Read PDF at high density for better visualization
  img <-
    image_read_pdf(pdf_path, pages = page_num, density = density)

  # Get image dimensions
  info <- image_info(img)
  width <- info$width
  height <- info$height

  # Create a copy for drawing
  img_with_boxes <- img

  # Draw rectangles on the image
  img_with_boxes <- image_draw(img_with_boxes)

  # Set up colors for name (blue) and ID (red) regions
  colors <- c("blue", "red")

  for (i in 1:nrow(test_regions)) {
    rect(
      test_regions$x[i],
      test_regions$y[i],
      test_regions$x[i] + test_regions$width[i],
      test_regions$y[i] + test_regions$height[i],
      border = colors[i],
      lwd = 2,
      lty = 1,
      col = NA
    )

    # Add labels to each box
    text(
      test_regions$x[i] + 10,
      test_regions$y[i] + 30,
      test_regions$label[i],
      col = colors[i],
      cex = 2,
      font = 2
    )
  }

  # Finish drawing
  dev.off()

  # Test OCR on each region
  cat("Testing OCR on each region:\n")
  for (i in 1:nrow(test_regions)) {
    geometry <-
      paste0(
        test_regions$width[i],
        "x",
        test_regions$height[i],
        "+",
        test_regions$x[i],
        "+",
        test_regions$y[i]
      )

    region <- image_crop(img, geometry)

    # Process for OCR
    processed <- region %>%
      image_convert(colorspace = "gray") %>%
      image_modulate(brightness = 120) %>%
      image_contrast()

    # Try OCR
    text <- ocr(processed)
    cat("\n", test_regions$label[i], " (", geometry, "):\n", sep = "")
    cat(substr(text, 1, 100), "...\n")
  }

  # Display the image with boxes
  plot(img_with_boxes)

  invisible(list(
    image = img_with_boxes,
    regions = test_regions,
    dimensions = c(width = width, height = height)
  ))
}

extract_student_info <-
  function(pdf_path_or_img,
           page_num = 1,
           debug = FALSE,
           name_region = c(
             x = 390,
             y = 425,
             width = 700,
             height = 150
           ),
           id_region = c(
             x = 1290,
             y = 425,
             width = 500,
             height = 150
           ),
           density = 400,
           show_regions = FALSE) {
    # Handle both file paths and pre-loaded magick image objects
    if (is.character(pdf_path_or_img)) {
      # If it's a path, read the PDF
      img <-
        image_read_pdf(pdf_path_or_img, pages = page_num, density = density)
    } else {
      # Otherwise, assume it's already a magick-image object
      img <- pdf_path_or_img
    }

    # Show annotated regions if requested
    if (show_regions) {
      img_with_boxes <- img
      img_with_boxes <- image_draw(img_with_boxes)

      # Draw name region (blue)
      rect(
        name_region["x"],
        name_region["y"],
        name_region["x"] + name_region["width"],
        name_region["y"] + name_region["height"],
        border = "blue",
        lwd = 2,
        lty = 1,
        col = NA
      )

      # Draw ID region (red)
      rect(
        id_region["x"],
        id_region["y"],
        id_region["x"] + id_region["width"],
        id_region["y"] + id_region["height"],
        border = "red",
        lwd = 2,
        lty = 1,
        col = NA
      )

      # Add labels
      text(
        name_region["x"] + 10,
        name_region["y"] + 30,
        "Name",
        col = "blue",
        cex = 2,
        font = 2
      )
      text(
        id_region["x"] + 10,
        id_region["y"] + 30,
        "ID",
        col = "red",
        cex = 2,
        font = 2
      )

      # Finish drawing
      dev.off()

      # Display the image with boxes
      plot(img_with_boxes)
    }



    # Extract name region
    name_geometry <-
      paste0(
        name_region["width"],
        "x",
        name_region["height"],
        "+",
        name_region["x"],
        "+",
        name_region["y"]
      )
    name_crop <- image_crop(img, name_geometry)

    # Extract ID region
    id_geometry <-
      paste0(id_region["width"], "x", id_region["height"], "+", id_region["x"], "+", id_region["y"])
    id_crop <- image_crop(img, id_geometry)


    if (debug) {
      image_write(name_crop, "debug_name_region.png")
      image_write(id_crop, "debug_id_region.png")
    }

    # Process name region for OCR
    # name_processed <- name_crop %>%
    #   image_convert(colorspace = "gray") %>%
    #   image_modulate(brightness = 120) %>%
    #   image_contrast() %>%
    #   image_despeckle()
    # Better preprocessing for name region
    name_processed <- name_crop %>%
      image_convert(colorspace = "gray") %>%
      # image_resize("150%") %>% # Scale up first
      image_normalize() %>% # Normalize contrast
      image_threshold(threshold = "40%", type = "black") %>% # Binarize
      image_morphology("close", "rectangle:2x2") %>% # Close gaps
      image_despeckle()


    # Process ID region - focus on making zeros look like zeros
    # id_processed <- id_crop %>%
    #   image_convert(colorspace = "gray") %>%
    #   image_modulate(brightness = 110) %>%
    #   image_contrast() %>%
    #   image_enhance()
    # Better preprocessing for ID region
    id_processed <- id_crop %>%
      image_convert(colorspace = "gray") %>%
      image_resize("200%") %>% # Scale up more for small text
      image_normalize() %>%
      image_threshold(threshold = "35%", type = "black") %>%
      image_morphology("open", "rectangle:1x1") %>% # Remove noise
      image_morphology("close", "rectangle:2x2") %>% # Fill gaps
      image_blur(radius = 0.3) # Slight blur to smooth edges


    if (debug) {
      image_write(id_processed, "debug_id_processed.png")
    }

    # Extract text using OCR
    # name_text <- ocr(name_processed)
    name_text <-
      ocr(name_processed, engine = tesseract(
        options = list(
          tessedit_char_whitelist = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ='`.",
          tessedit_pageseg_mode = 6
        ),
        cache = TRUE
      ))

    # Clean name
    student_name <-
      toupper(str_replace_all(name_text, "[^A-Za-z\\s\\-'\\.]", ""))
    student_name <-
      str_squish(student_name) # This should preserve internal spaces

    # id_text <- ocr(id_processed)
    id_text <- ocr(id_processed, engine = tesseract(
      options = list(
        tessedit_char_whitelist = "0123456789",
        tessedit_pageseg_mode = 8
      ),
      cache = TRUE
    ))

    # For ID processing, add character replacement just for common zero OCR errors
    # Ensure it's a single line and stripped of all whitespace
    student_id <- str_squish(gsub("\\s", "", id_text)) %>%
      str_replace_all("O", "0") %>%
      str_replace_all("D", "0") %>%
      str_replace_all("C", "0") %>%
      str_replace_all("c", "0") %>%
      str_replace_all("&", "0") %>%
      str_replace_all("o", "0") %>%
      str_replace_all("e", "0")

    if (is.na(student_id)) {
      student_id <- "NOT FOUND"
    }

    if (debug) {
      cat(
        "Raw name OCR text:\n",
        name_text,
        "\nCleaned:\n",
        student_name
      )
      cat("\nRaw ID OCR text:\n", id_text, "\nCleaned:\n", student_id)
    }

    result <- list(
      student_name = student_name,
      student_id = student_id,
      raw_name_text = if (debug) {
        name_text
      } else {
        NULL
      },
      raw_id_text = if (debug) {
        id_text
      } else {
        NULL
      }
    )

    if (debug) {
      image_write(name_crop, "debug_name_region.png")
      image_write(id_crop, "debug_id_region.png")
      image_write(name_processed, "debug_name_processed.png") # Add this line
    }


    return(result)
  }
