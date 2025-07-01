require(magick)
require(pdftools)
require(dplyr)
require(tidyverse)
require(stringr)
require(reticulate)
require(progress)
reticulate::py_require("opencv-python")
reticulate::py_require("numpy")

Sys.setenv(MAGICK_MEMORY_LIMIT = "2GB")
Sys.setenv(MAGICK_MAP_LIMIT = "4GB")
Sys.setenv(MAGICK_DISK_LIMIT = "8GB")
# system("convert -list resource")

detect_and_correct_orientation <-
  function(student_img, template_img, debug = FALSE) {
    # This function checks if the page is flipped 180 degrees and corrects it.

    # 1. Define an anchor from the top of the template image.
    #    This should be a unique feature not present at the bottom (e.g., a header).
    template_info <- image_info(template_img)
    anchor_height <- round(template_info$height * 0.10)
    anchor_width <- round(template_info$width * 0.40)
    anchor_x_offset <- round((template_info$width - anchor_width) / 2) # Centered
    anchor_y_offset <- 5 # A few pixels from the top edge

    anchor_geom <-
      sprintf(
        "%dx%d+%d+%d",
        anchor_width,
        anchor_height,
        anchor_x_offset,
        anchor_y_offset
      )
    template_anchor <- image_crop(template_img, anchor_geom)

    # if (debug) {
    #   cat("Created orientation anchor from template.\n")
    #   image_write(template_anchor, "debug_orientation_anchor.png")
    # }

    # 2. Use template matching to find this anchor in the student image.
    student_path <- tempfile(fileext = ".png")
    anchor_path <- tempfile(fileext = ".png")
    image_write(student_img, path = student_path, format = "png")
    image_write(template_anchor, path = anchor_path, format = "png")

    student_path_py <- gsub("\\\\", "/", student_path)
    anchor_path_py <- gsub("\\\\", "/", anchor_path)

    python_script <- sprintf(
      "
import cv2
import numpy as np

student_img_py = cv2.imread('%s', 0)
template_anchor_py = cv2.imread('%s', 0)

if student_img_py is None:
    raise Exception('Orientation check: Failed to load student image in Python.')
if template_anchor_py is None:
    raise Exception('Orientation check: Failed to load anchor image in Python.')

res = cv2.matchTemplate(student_img_py, template_anchor_py, cv2.TM_CCOEFF_NORMED)
min_val, max_val, min_loc, top_left = cv2.minMaxLoc(res)
    ",
      student_path_py,
      anchor_path_py
    )

    tryCatch(
      {
        reticulate::py_run_string(python_script)
        top_left_y <- reticulate::py$top_left[[2]]
        match_confidence <- reticulate::py$max_val
      },
      error = function(e) {
        if (debug) {
          cat("Python error during orientation check:", e$message, "\n")
        }
        top_left_y <<- 0 # Default to no-flip
        match_confidence <<- 0
      }
    )

    file.remove(student_path, anchor_path)

    # 3. Decide if rotation is needed based on the vertical position of the match.
    student_info <- image_info(student_img)
    page_midpoint_y <- student_info$height / 2

    # Only act if the match is reasonably confident.
    if (match_confidence < 0.5) {
      if (debug) {
        cat(
          sprintf(
            "Orientation anchor match confidence (%.2f) too low. Skipping rotation check.\n",
            match_confidence
          )
        )
      }
      return(student_img)
    }

    if (top_left_y > page_midpoint_y) {
      # Anchor found in the bottom half of the page; it's flipped.
      if (debug) {
        cat(
          sprintf(
            "Anchor found at y=%d (page height=%d). Page appears flipped. Rotating...\n",
            round(top_left_y),
            student_info$height
          )
        )
      }
      return(image_rotate(student_img, 180))
    } else {
      # Anchor found in the top half; orientation is correct.
      if (debug) {
        cat(
          sprintf(
            "Anchor found at y=%d (page height=%d). Orientation appears correct.\n",
            round(top_left_y),
            student_info$height
          )
        )
      }
      return(student_img)
    }
  }


correct_skew <- function(img, debug = FALSE) {
  gray_img <- image_convert(img, colorspace = "gray")

  # Apply binary threshold to get clear lines
  binary <-
    image_threshold(gray_img, threshold = "50%", type = "black")
  edges <- image_edge(binary, radius = 1)

  info <- image_info(gray_img)
  height <- image_info(gray_img)$height[1]
  width <- image_info(gray_img)$width[1]

  # Convert to matrix for analysis - safer approach
  edge_matrix <- NULL

  tryCatch(
    {
      # Use image_data for proper conversion
      edge_data <- image_data(edges, channels = "gray")
      edge_matrix <- as.numeric(edge_data)

      # Check if dimensions match expected - fix the condition length issue
      expected_length <- height * width
      actual_length <- length(edge_matrix)

      if (length(actual_length) > 1 || length(expected_length) > 1) {
        if (debug) {
          cat("Dimension calculation error:\n")
          cat("  height:", height, "(length:", length(height), ")\n")
          cat("  width:", width, "(length:", length(width), ")\n")
          cat(
            "  expected_length:",
            expected_length,
            "(length:",
            length(expected_length),
            ")\n"
          )
          cat(
            "  actual_length:",
            actual_length,
            "(length:",
            length(actual_length),
            ")\n"
          )
        }
        return(gray_img)
      }


      if (actual_length != expected_length) {
        if (debug) {
          cat(
            "Dimension mismatch: expected",
            expected_length,
            "got",
            actual_length,
            "\n"
          )
          cat("Skipping skew correction\n")
        }
        return(gray_img)
      }

      # Reshape to matrix (note: may need to transpose depending on magick's format)
      dim(edge_matrix) <- c(width, height) # Changed order
      edge_matrix <-
        t(edge_matrix) # Transpose to get correct orientation
    },
    error = function(e) {
      if (debug) {
        cat("Error in edge detection conversion:", e$message, "\n")
        cat("Skipping skew correction\n")
      }
      edge_matrix <<- NULL # Ensure edge_matrix is NULL on error
    }
  )

  # Return early if edge_matrix creation failed
  if (is.null(edge_matrix)) {
    return(gray_img)
  }

  # Find horizontal lines by detecting rows with many edge pixels
  row_scores <- apply(edge_matrix, 1, function(row) {
    sum(row > 0.5)
  })
  strong_rows <- which(row_scores > quantile(row_scores, 0.95))

  # Calculate skew angles from multiple horizontal lines
  angles <- c()

  for (row_idx in strong_rows[1:min(5, length(strong_rows))]) {
    row_data <- edge_matrix[row_idx, ]
    edge_points <- which(row_data > 0.5)

    if (length(edge_points) > 20) {
      # Sufficient points for line fitting
      x_coords <- edge_points
      y_coords <- rep(row_idx, length(edge_points))

      # Simple linear regression to find line angle
      if (length(x_coords) > 1) {
        fit <- lm(y_coords ~ x_coords)
        slope <- coef(fit)[2]
        angle <- atan(slope) * 180 / pi
        angles <- c(angles, angle)
      }
    }
  }

  # Calculate median skew angle
  if (length(angles) > 0) {
    skew_angle <- median(angles, na.rm = TRUE)

    # Only correct if skew is significant (> 0.5 degrees)
    if (abs(skew_angle) > 0.5 && abs(skew_angle) < 45) {
      corrected_img <- image_rotate(gray_img, -skew_angle)
      if (debug) {
        cat("Skew corrected by", round(skew_angle, 2), "degrees\n")
      }

      # if (debug) {
      #   image_write(gray_img, "debug_skew_input.png")
      #   image_write(corrected_img, "debug_skew_corrected.png")
      # }

      return(corrected_img)
    }
  }

  if (debug) {
    cat("No significant skew detected\n")
    # image_write(gray_img, "debug_skew_input.png")
  }

  return(gray_img)
}

calibrate_bubble_grid <- function(img, debug = FALSE, zoom = 2.0) {
  # Get original image dimensions for calculations and the final verification plot
  orig_info <- image_info(img)
  orig_width <- orig_info$width
  orig_height <- orig_info$height

  # Create a version of the image for interactive plotting, scaled if requested
  if (zoom != 1.0 && zoom > 0) {
    scale_geom <- paste0(round(zoom * 100), "%")
    cat(sprintf(
      "Displaying image at %s zoom for easier selection.\n",
      scale_geom
    ))
    plot_img <- image_scale(img, geometry = scale_geom)
  } else {
    plot_img <- img
  }

  # Get dimensions and raster for the (potentially scaled) plot image
  plot_info <- image_info(plot_img)
  plot_width <- plot_info$width
  plot_height <- plot_info$height
  plot_raster_interactive <- as.raster(plot_img)

  # If debugging, create a raster of the original image for the final verification plot
  if (debug) {
    orig_raster <- as.raster(img)
  }

  cat("=== Interactive Bubble Grid Calibration ===\n")
  cat("You will click on 8 reference bubbles to establish the grid:\n")
  cat("1. Part A: Question 1, Option A (top-left)\n")
  cat("2. Part A: Question 1, Option E (top-right)\n")
  cat("3. Part A: Question 12, Option A (bottom-left)\n")
  cat("4. Part A: Question 12, Option E (bottom-right)\n")
  cat("5. Part B: Question 13, Option A (top-left)\n")
  cat("6. Part B: Question 13, Option D (top-right)\n")
  cat("7. Part B: Question 25, Option A (bottom-left)\n")
  cat("8. Part B: Question 25, Option D (bottom-right)\n\n")

  # Open a new graphics device to avoid RStudio plot pane scaling issues.
  # This ensures the coordinate system of the plot matches the image pixels.
  graphics.off()
  window_width <- ifelse(zoom > 1.0, 15, 10) # Make the window bigger for zoom
  dev.new(
    width = window_width,
    height = window_width * (plot_height / plot_width),
    noRStudioGD = TRUE
  )

  # Set plot margins to zero to use the entire device area for the image.
  par(mar = c(0, 0, 0, 0))

  # Plot with a 1:1 aspect ratio and top-left origin.
  plot(
    NA,
    xlim = c(0, plot_width),
    ylim = c(plot_height, 0),
    type = "n",
    xlab = "",
    ylab = "",
    asp = 1,
    xaxs = "i",
    yaxs = "i"
  )

  rasterImage(plot_raster_interactive, 0, plot_height, plot_width, 0)

  cat("A new window has opened with the answer sheet.\n")
  cat("Click the 8 required points in that window...\n")
  title(
    main = "Click on bubbles in order listed in the R Console",
    cex.main = 1,
    col.main = "black"
  )

  scaled_clicks <- locator(
    8,
    type = "p",
    pch = 3,
    col = "red",
    cex = 1.5
  )

  # Close the device automatically after clicks are registered
  dev.off()


  # Check if clicks were successful
  if (is.null(scaled_clicks) ||
    is.null(scaled_clicks$x) || is.null(scaled_clicks$y)) {
    stop("No clicks detected. Make sure you click on the plot window.")
  }

  if (length(scaled_clicks$x) != 8) {
    stop(paste("Expected 8 clicks, but got", length(scaled_clicks$x)))
  }

  # Descale coordinates back to the original image space if we zoomed
  if (zoom != 1.0 && zoom > 0) {
    clicks <- list(x = scaled_clicks$x / zoom, y = scaled_clicks$y / zoom)
  } else {
    clicks <- scaled_clicks
  }

  # Ensure coordinates are numeric
  x_coords <- as.numeric(clicks$x)
  y_coords <- as.numeric(clicks$y)

  if (any(is.na(x_coords)) || any(is.na(y_coords))) {
    stop("Some coordinates are not numeric. Please try again.")
  }

  # Extract coordinates
  coords <- data.frame(x = x_coords, y = y_coords)
  if (debug) {
    cat("\n+++ DEBUG: Raw coordinates from locator():\n")
    print(coords)
  }

  # Calculate Part A parameters using all 4 corners
  part_a_start_x <- coords$x[1] # Q1-A
  part_a_start_y <- coords$y[1] # Q1-A
  part_a_col_spacing <- (coords$x[2] - coords$x[1]) / (PART_A_OPTIONS - 1) # Q1-A to Q1-E
  part_a_row_spacing <- (coords$y[3] - coords$y[1]) / (length(PART_A_QUESTIONS) - 1) # Q1-A to Q12-A

  # Calculate Part B parameters using all 4 corners
  part_b_start_x <- coords$x[5] # Q13-A
  part_b_start_y <- coords$y[5] # Q13-A
  part_b_col_spacing <- (coords$x[6] - coords$x[5]) / (PART_B_OPTIONS - 1) # Q13-A to Q13-D
  part_b_row_spacing <- (coords$y[7] - coords$y[5]) / (length(PART_B_QUESTIONS) - 1) # Q13-A to Q25-A

  # Estimate bubble radius using absolute values
  bubble_radius <- min(
    abs(part_a_col_spacing),
    abs(part_a_row_spacing),
    abs(part_b_col_spacing),
    abs(part_b_row_spacing)
  ) * 0.3

  params <- list(
    part_a_start_x = round(part_a_start_x),
    part_a_start_y = round(part_a_start_y),
    part_a_col_spacing = round(part_a_col_spacing),
    part_a_row_spacing = round(part_a_row_spacing),
    part_b_start_x = round(part_b_start_x),
    part_b_start_y = round(part_b_start_y),
    part_b_col_spacing = round(part_b_col_spacing),
    part_b_row_spacing = round(part_b_row_spacing),
    bubble_radius = round(bubble_radius)
  )

  cat("\nCalculated grid parameters:\n")
  print(params)

  if (debug) {
    # Show verification plot. Use the same coordinate system as the interactive plot
    plot(
      NA,
      xlim = c(0, orig_width),
      ylim = c(orig_height, 0),
      type = "n",
      xlab = "",
      ylab = "",
      main = "Verification: All bubble positions",
      asp = 1,
      xaxs = "i",
      yaxs = "i"
    )
    rasterImage(orig_raster, 0, orig_height, orig_width, 0)

    # Draw all calculated positions
    for (q in PART_A_QUESTIONS) {
      for (opt in seq_len(PART_A_OPTIONS)) {
        x_pos <- params$part_a_start_x + (opt - 1) * params$part_a_col_spacing
        y_pos <- params$part_a_start_y + (q - 1) * params$part_a_row_spacing
        symbols(
          x_pos,
          y_pos,
          circles = params$bubble_radius,
          inches = FALSE,
          add = TRUE,
          fg = "blue",
          lwd = 1
        )
      }
    }

    for (q in PART_B_QUESTIONS) {
      for (opt in seq_len(PART_B_OPTIONS)) {
        x_pos <- params$part_b_start_x + (opt - 1) * params$part_b_col_spacing
        y_pos <- params$part_b_start_y + (q - 13) * params$part_b_row_spacing
        symbols(
          x_pos,
          y_pos,
          circles = params$bubble_radius,
          inches = FALSE,
          add = TRUE,
          fg = "green",
          lwd = 1
        )
      }
    }
  }

  return(params)
}


is_bubble_filled <-
  function(filled_img,
           template_img,
           bubble,
           threshold = 0.15,
           debug = FALSE) {
    # Get image info
    info <- image_info(filled_img)

    # Ensure coordinates are within bounds
    x <-
      max(bubble$radius, min(bubble$x, info$width - bubble$radius))
    y <-
      max(bubble$radius, min(bubble$y, info$height - bubble$radius))
    r <- bubble$radius

    # Crop region around bubble from both images
    crop_spec <- sprintf("%dx%d+%d+%d", 2 * r, 2 * r, x - r, y - r)

    tryCatch(
      {
        filled_region <- image_crop(filled_img, crop_spec)
        template_region <- image_crop(template_img, crop_spec)

        # Convert both to grayscale if not already
        if (image_info(filled_region)$colorspace != "Gray") {
          filled_region <- image_convert(filled_region, colorspace = "gray")
        }
        if (image_info(template_region)$colorspace != "Gray") {
          template_region <-
            image_convert(template_region, colorspace = "gray")
        }

        # Get pixel data as matrices
        filled_data <- as.numeric(filled_region[[1]])
        template_data <- as.numeric(template_region[[1]])

        # Calculate difference (template - filled, so filled bubbles show as positive)
        difference <- template_data - filled_data

        # Calculate mean difference (darkness increase from template to filled)
        mean_difference <- mean(difference, na.rm = TRUE)

        if (debug && mean_difference > 0.05) {
          cat(
            sprintf(
              "Q%d-%s: difference=%.3f\n",
              bubble$question,
              bubble$option,
              mean_difference
            )
          )
        }

        return(mean_difference > threshold)
      },
      error = function(e) {
        if (debug) {
          cat(
            "Error checking bubble:",
            bubble$question,
            bubble$option,
            "\n"
          )
        }
        return(FALSE)
      }
    )
  }

refine_alignment_with_template_matching <- function(student_img, template_img, debug = FALSE) {
  # This function uses a self-contained Python script to perform the alignment.

  if (debug) {
    cat("\n--- Entering robust alignment function ---\n")
  }

  # --- Stage 1: R-side Setup ---
  student_path <- tempfile(fileext = ".png")
  anchor_path <- tempfile(fileext = ".png")

  anchor_region <- c(
    x = 180,
    y = 570,
    width = 120,
    height = 50
  )
  anchor_crop_geom <- sprintf(
    "%dx%d+%d+%d",
    anchor_region["width"],
    anchor_region["height"],
    anchor_region["x"],
    anchor_region["y"]
  )

  image_write(student_img, path = student_path, format = "png")
  image_write(image_crop(template_img, anchor_crop_geom),
    path = anchor_path,
    format = "png"
  )

  # --- Stage 2: Define and Execute the Python Script ---
  student_path_py <- gsub("\\\\", "/", student_path)
  anchor_path_py <- gsub("\\\\", "/", anchor_path)

  python_script <- sprintf(
    "
import cv2
import numpy as np

student_img_py = cv2.imread('%s', 0)
template_anchor_py = cv2.imread('%s', 0)

if student_img_py is None:
  raise Exception('Failed to load student image in Python.')
if template_anchor_py is None:
  raise Exception('Failed to load anchor image in Python.')

res = cv2.matchTemplate(student_img_py, template_anchor_py, cv2.TM_CCOEFF_NORMED)
_, _, _, top_left = cv2.minMaxLoc(res)
",
    student_path_py,
    anchor_path_py
  )

  reticulate::py_run_string(python_script)

  # --- Stage 3: Retrieve the simple result from Python ---
  top_left <- reticulate::py$top_left

  # Clean up the temporary files
  file.remove(student_path)
  file.remove(anchor_path)

  # Calculate the shift (delta x, delta y) in R
  dx <- top_left[[1]] - anchor_region["x"]
  dy <- top_left[[2]] - anchor_region["y"]

  if (debug) {
    cat(sprintf("Automated alignment shift detected: dx = %d, dy = %d\n", dx, dy))

    bottom_right <- c(top_left[[1]] + anchor_region["width"], top_left[[2]] + anchor_region["height"])
    student_img_viz <- image_draw(student_img)
    rect(
      top_left[[1]],
      top_left[[2]],
      bottom_right[1],
      bottom_right[2],
      border = "red",
      lwd = 2
    )
    text(top_left[[1]], top_left[[2]] - 10, "Found Anchor", col = "red")
    dev.off() # Required to prevent plotting loops in RStudio viewer
    image_write(student_img_viz, "debug_automated_alignment.png")
    cat("Automated alignment visualization saved to debug_automated_alignment.png\n")
  }

  return(list(dx = dx, dy = dy))
}



align_images <- function(img, template, debug = FALSE) {
  # First correct skew
  img_corrected <- correct_skew(img, debug = debug)

  # Convert to grayscale if needed
  if (length(dim(img_corrected)) == 3) {
    img_gray <- grayscale(img_corrected)
  } else {
    img_gray <- img_corrected
  }

  if (length(dim(template)) == 3) {
    template_gray <- grayscale(template)
  } else {
    template_gray <- template
  }

  # Convert to cv2 format
  img_cv <- as.numeric(img_gray) * 255
  template_cv <- as.numeric(template_gray) * 255

  # Detect keypoints and descriptors using ORB
  orb <- cv2$ORB_create(nfeatures = as.integer(1000))

  kp1_desc1 <- orb$detectAndCompute(template_cv, NULL)
  kp1 <- kp1_desc1[[1]]
  desc1 <- kp1_desc1[[2]]

  kp2_desc2 <- orb$detectAndCompute(img_cv, NULL)
  kp2 <- kp2_desc2[[1]]
  desc2 <- kp2_desc2[[2]]

  # Match features
  matcher <- cv2$BFMatcher(cv2$NORM_HAMMING, crossCheck = TRUE)
  matches <- matcher$match(desc1, desc2)

  # Sort matches by distance
  matches <-
    reticulate::py_eval("sorted(matches, key=lambda x: x.distance)")

  # Need at least 4 points for homography
  if (length(matches) < 4) {
    warning("Not enough matches found for alignment")
    return(img_corrected)
  }

  # Extract matched keypoints
  pts1 <- matrix(0, ncol = 2, nrow = length(matches))
  pts2 <- matrix(0, ncol = 2, nrow = length(matches))

  for (i in seq_len(length(matches))) {
    pts1[i, ] <- c(kp1[[matches[[i]]$queryIdx + 1]]$pt[[1]], kp1[[matches[[i]]$queryIdx + 1]]$pt[[2]])
    pts2[i, ] <- c(kp2[[matches[[i]]$trainIdx + 1]]$pt[[1]], kp2[[matches[[i]]$trainIdx + 1]]$pt[[2]])
  }

  # Find homography
  result <- cv2$findHomography(pts2, pts1, cv2$RANSAC, 5.0)
  M <- result[[1]]

  # Warp the image
  height <- nrow(template_cv)
  width <- ncol(template_cv)

  img_aligned_cv <-
    cv2$warpPerspective(
      as.numeric(img_corrected) * 255,
      M,
      tuple(as.integer(width), as.integer(height))
    )

  # Convert back to EBImage format
  img_aligned <- Image(img_aligned_cv / 255, dim = c(width, height))

  if (debug) {
    # Draw matches for visualization
    img_matches <- cv2$drawMatches(
      template_cv,
      kp1,
      img_cv,
      kp2,
      matches[1:min(50, length(matches))],
      NULL,
      flags = cv2$DrawMatchesFlags_NOT_DRAW_SINGLE_POINTS
    )
    display(Image(img_matches / 255))
  }

  return(img_aligned)
}


find_bubbles_by_omr <- function(img,
                                grid_params = NULL,
                                debug = FALSE) {
  gray_img <- image_convert(img, colorspace = "gray")

  # If no grid_params provided, run calibration
  if (is.null(grid_params)) {
    cat("No grid parameters provided. Running interactive calibration...\n")
    grid_params <- calibrate_bubble_grid(img, debug)
  }

  # Use calibrated parameters
  part_a_start_x <- grid_params$part_a_start_x
  part_a_start_y <- grid_params$part_a_start_y
  part_a_col_spacing <- grid_params$part_a_col_spacing
  part_a_row_spacing <- grid_params$part_a_row_spacing

  part_b_start_x <- grid_params$part_b_start_x
  part_b_start_y <- grid_params$part_b_start_y
  part_b_col_spacing <- grid_params$part_b_col_spacing
  part_b_row_spacing <- grid_params$part_b_row_spacing

  bubble_radius <- grid_params$bubble_radius

  if (debug) {
    cat(
      "OMR-based bubble detection\n",
      "Defined grid parameters: Part A X:",
      part_a_start_x,
      " Y:",
      part_a_start_y,
      "Part B X:",
      part_b_start_x,
      "Part B Y:",
      part_b_start_y,
      "\n"
    )
  }

  bubbles <- list()

  # Generate Part A bubble positions using calibrated parameters
  for (row in seq_len(length(PART_A_QUESTIONS))) {
    for (col in seq_len(PART_A_OPTIONS)) {
      x_pos <- part_a_start_x + (col - 1) * part_a_col_spacing
      y_pos <- part_a_start_y + (row - 1) * part_a_row_spacing

      question_num <- PART_A_QUESTIONS[row]
      bubble_id <- paste0("Q", question_num, "_", LETTERS[col])
      bubbles[[bubble_id]] <- list(
        question = question_num,
        option = LETTERS[col],
        x = round(x_pos),
        y = round(y_pos),
        radius = abs(bubble_radius),
        part = "A"
      )
    }
  }

  # Generate Part B bubble positions using calibrated parameters
  for (row in seq_len(length(PART_B_QUESTIONS))) {
    for (col in seq_len(PART_B_OPTIONS)) {
      x_pos <- part_b_start_x + (col - 1) * part_b_col_spacing
      y_pos <- part_b_start_y + (row - 1) * part_b_row_spacing

      question_num <- PART_B_QUESTIONS[row]
      bubble_id <- paste0("Q", question_num, "_", LETTERS[col])
      bubbles[[bubble_id]] <- list(
        question = question_num,
        option = LETTERS[col],
        x = round(x_pos),
        y = round(y_pos),
        radius = abs(bubble_radius),
        part = "B"
      )
    }
  }

  if (debug) {
    cat("OMR positioned", length(bubbles), "bubbles\n")

    # Create visualization using base R plot, explicitly using the first frame
    # to prevent errors with multi-frame magick objects.
    img_info <- image_info(gray_img[1])
    png("detected_omr_bubbles.png",
      width = img_info$width,
      height = img_info$height
    )
    par(mar = c(0, 0, 0, 0))
    plot(
      NA,
      xlim = c(0, img_info$width),
      ylim = c(img_info$height, 0),
      xlab = "",
      ylab = "",
      asp = 1,
      xaxs = "i",
      yaxs = "i"
    )
    rasterImage(
      as.raster(gray_img[1]),
      0,
      img_info$height,
      img_info$width,
      0
    )



    # Draw all bubble positions
    for (i in seq_along(bubbles)) {
      bubble <- bubbles[[i]]
      color <- if (bubble$part == "A") {
        "blue"
      } else {
        "green"
      }

      symbols(
        bubble$x,
        bubble$y,
        circles = abs(bubble_radius),
        inches = FALSE,
        add = TRUE,
        fg = color,
        lwd = 1
      )
    }

    dev.off()
    cat("OMR bubble detection visualization saved\n")
  }

  return(bubbles)
}

detect_bubble_locations <- function(blank_template_path,
                                    expected_count = EXPECTED_BUBBLE_COUNT,
                                    debug = FALSE,
                                    density = 200,
                                    grid_params = NULL) {
  if (debug) {
    cat("Reading template at", density, "DPI\n")
  }
  template_img <- image_read_pdf(blank_template_path, density = density)

  # Step 1: Skew correction
  corrected_img <- correct_skew(template_img, debug = debug)

  # Add debug image saving
  if (debug) {
    image_write(corrected_img, "debug_template_skewcorrected.png")
  }

  # Step 2: OMR-based bubble detection
  bubbles <- find_bubbles_by_omr(corrected_img, grid_params, debug = debug)

  if (length(bubbles) != expected_count) {
    if (debug) {
      cat(
        "Found",
        length(bubbles),
        "bubbles, expected",
        expected_count,
        "\n"
      )
    }
    stop(sprintf(
      "Expected %d bubbles but found %d",
      expected_count,
      length(bubbles)
    ))
  }

  return(bubbles)
}

extract_answers_omr <- function(filled_img_frame,
                                bubble_locations,
                                template_img = NULL,
                                correct_answers_df = NULL,
                                debug = FALSE,
                                fill_threshold = 0.3,
                                partial_credit_mca = FALSE, max_debug_q = 3, threshold = 40) {
  # Skew correction is the first step for the frame
  filled_corrected <- correct_skew(filled_img_frame, debug = debug)

  # --- Automated Alignment Step ---
  template_corrected <- correct_skew(template_img, debug = FALSE)

  alignment_shift <- refine_alignment_with_template_matching(
    student_img = filled_corrected,
    template_img = template_corrected,
    debug = debug
  )

  shifted_bubble_locations <- lapply(bubble_locations, function(bubble) {
    bubble$x <- bubble$x + alignment_shift$dx
    bubble$y <- bubble$y + alignment_shift$dy
    return(bubble)
  })

  answers <- extract_by_omr_analysis(
    filled_img = filled_corrected,
    bubble_locations = shifted_bubble_locations,
    debug = debug,
    fill_threshold = fill_threshold, max_debug_q, threshold
  )
  results_with_score <- create_results_dataframe(answers, correct_answers_df, debug, partial_credit_mca)

  results_with_score$shifted_bubble_locations <- shifted_bubble_locations

  return(results_with_score)
}


extract_by_omr_analysis <- function(filled_img,
                                    bubble_locations,
                                    debug = FALSE,
                                    fill_threshold = 0.3,
                                    max_debug_q = 3,
                                    threshold = 40) {
  gray_img <- image_convert(filled_img, colorspace = "gray")

  # Apply inverse binary threshold for OMR
  binary_img <- image_threshold(gray_img, threshold = paste0(threshold, "%"), type = "white") %>%
    image_negate()

  # Morphological cleaning
  cleaned_img <- apply_morphology(binary_img, "close", 3)

  if (debug) {
    image_write(cleaned_img, paste0("debug_omr_binary", threshold, ".png"))
    cat("OMR binary image saved\n")
  }

  answers <- list()

  for (q in seq_len(TOTAL_QUESTIONS)) {
    question_bubbles <- bubble_locations[sapply(bubble_locations, function(b) {
      b$question == q
    })]
    filled_options <- c()

    # debug first 25 questions
    if (debug && q <= max_debug_q) {
      cat("\nQuestion", q, "OMR analysis:\n")
    }

    for (bubble in question_bubbles) {
      fill_intensity <- check_bubble_fill_omr(cleaned_img, bubble, debug, max_debug_q)

      if (fill_intensity > fill_threshold) {
        # Threshold for filled bubble
        filled_options <- c(filled_options, bubble$option)
      }
    }

    answer <- if (length(filled_options) == 0) {
      "NONE"
    } else {
      paste(sort(filled_options), collapse = "")
    }
    answers[[q]] <- answer
  }


  # Create visualization
  # Explicitly use the first frame to avoid errors with multi-frame images.

  if (debug) {
    img_info <- image_info(gray_img[1])
    png("omr_filled_analysis.png",
      width = img_info$width,
      height = img_info$height
    )
    par(mar = c(0, 0, 0, 0))
    plot(
      NA,
      xlim = c(0, img_info$width),
      ylim = c(img_info$height, 0),
      xlab = "",
      ylab = "",
      asp = 1,
      xaxs = "i",
      yaxs = "i"
    )
    rasterImage(
      as.raster(gray_img[1]),
      0,
      img_info$height,
      img_info$width,
      0
    )

    for (q in seq_len(TOTAL_QUESTIONS)) {
      question_bubbles <-
        bubble_locations[sapply(bubble_locations, function(b) {
          b$question == q
        })]

      for (bubble in question_bubbles) {
        fill_intensity <-
          check_bubble_fill_omr(cleaned_img, bubble, FALSE) # false required here to prevent printing twice when debug is true

        # Color based on fill intensity
        color <- if (fill_intensity > fill_threshold) {
          "red"
        } else {
          "blue"
        }

        symbols(
          bubble$x,
          bubble$y,
          circles = bubble$radius,
          inches = FALSE,
          add = TRUE,
          fg = color,
          lwd = 2
        )

        # Add intensity value
        text(
          bubble$x,
          bubble$y + bubble$radius + 10,
          round(fill_intensity, 2),
          col = color,
          cex = 0.6
        )
      }
    }
    dev.off()
    cat("OMR filled bubbles analysis saved\n")
  }

  return(answers)
}

check_bubble_fill_omr <- function(binary_img, bubble, debug = FALSE, max_debug_q = 3) {
  # Use absolute coordinates directly
  crop_x <- bubble$x
  crop_y <- bubble$y

  # Ensure coordinates are within bounds
  img_info <- image_info(binary_img)
  crop_x <- max(bubble$radius, min(crop_x, img_info$width - bubble$radius))
  crop_y <- max(bubble$radius, min(crop_y, img_info$height - bubble$radius))

  # Extract circular region around bubble
  r <- bubble$radius
  crop_spec <-
    sprintf("%dx%d+%d+%d", 2 * r, 2 * r, crop_x - r, crop_y -
      r)

  tryCatch(
    {
      bubble_region_img <- image_crop(binary_img, crop_spec)

      # Convert to matrix for analysis
      bubble_data <- as.numeric(bubble_region_img[[1]])

      # Create circular mask
      region_size <- 2 * r
      center <- r
      mask <- matrix(FALSE, region_size, region_size)

      for (i in seq_len(region_size)) {
        for (j in seq_len(region_size)) {
          if (sqrt((i - center)^2 + (j - center)^2) <= r * 0.8) {
            mask[i, j] <- TRUE
          }
        }
      }

      # Calculate fill intensity within circular mask
      if (length(bubble_data) == length(mask)) {
        masked_pixels <- bubble_data[mask]
        fill_intensity <- mean(masked_pixels, na.rm = TRUE)
      } else {
        fill_intensity <- mean(bubble_data, na.rm = TRUE)
      }

      if (debug && bubble$question <= max_debug_q) {
        cat(
          sprintf(
            "  Q%d-%s: fill intensity=%.3f\n",
            bubble$question,
            bubble$option,
            fill_intensity
          )
        )
      }

      return(fill_intensity)
    },
    error = function(e) {
      # Log the error, but then stop execution
      stop(
        sprintf(
          "Fatal Error analyzing bubble Q%d-%s: %s",
          bubble$question,
          bubble$option,
          e$message
        ),
        call. = FALSE
      )
    }
  )
}


score_mca_partial_credit <- function(student_answer_str, correct_answer_str) {
  if (is.na(student_answer_str) || is.na(correct_answer_str) || correct_answer_str == "NONE") {
    return(0)
  }

  # Split into individual characters, handling "NONE" if it somehow gets here
  student_selections <- if (student_answer_str == "NONE" || student_answer_str == "") {
    character(0)
  } else {
    unlist(strsplit(student_answer_str, ""))
  }

  correct_options <- unlist(strsplit(correct_answer_str, ""))

  if (length(correct_options) == 0) {
    return(0) # Should not happen with valid data
  }

  # Value of each correct selection (based on the total number of correct options)
  points_per_correct_option <- 1 / length(correct_options)

  score <- 0

  # Track selected options to avoid double counting and for accurate penalty application
  selected_correct <- student_selections[student_selections %in% correct_options]
  selected_incorrect <- student_selections[!student_selections %in% correct_options]

  # Award points for correct selections
  score <- score + (length(selected_correct) * points_per_correct_option)

  # Deduct points for incorrect selections
  score <- score - (length(selected_incorrect) * points_per_correct_option)

  score <- max(0, score)
  score <- round(score, 1)
  score <- min(1.0, score)

  return(score)
}


create_results_dataframe <- function(answers,
                                     correct_answers_df = NULL,
                                     debug = FALSE,
                                     partial_credit_mca = FALSE) {
  results_df <- data.frame(
    Question = seq_len(length(answers)),
    Student_Answer = unlist(answers),
    stringsAsFactors = FALSE
  )

  if (!is.null(correct_answers_df)) {
    correct_answers_to_join <- correct_answers_df %>%
      select(question, answer)

    results_df <- results_df %>%
      left_join(correct_answers_to_join, by = c("Question" = "question")) %>%
      rename(Correct_Answer = answer) %>%
      mutate(
        Score_Points = pmap_dbl(
          list(
            Student_Answer = Student_Answer,
            Correct_Answer = Correct_Answer,
            Question = Question
          ),
          function(Student_Answer, Correct_Answer, Question) {
            # Check if it's an MCA question AND partial_credit_mca is TRUE
            is_mca <- !is.na(Correct_Answer) && nchar(Correct_Answer) > 1

            if (is_mca && partial_credit_mca) {
              return(score_mca_partial_credit(Student_Answer, Correct_Answer))
            } else {
              # For single-choice or if partial_credit_mca is FALSE
              return(as.numeric(Student_Answer == Correct_Answer))
            }
          }
        )
      )
  } else {
    results_df$Score_Points <- NA # No score if no correct answers provided
  }

  # Calculate a total 'correct count' based on if Score_Points is 1 (for compatibility with old 'score' output) but not used
  correct_count <- sum(results_df$Score_Points == 1.0, na.rm = TRUE)
  final_score <- round(sum(results_df$Score_Points, na.rm = TRUE), 1)

  if (debug) {
    cat("Scores calculated:", final_score, "points\n")
  }

  return(list(
    results_df = results_df,
    score = final_score,
    total = nrow(results_df)
  ))
}


process_bubble_sheet_omr <- function(filled_pdf_path,
                                     blank_template_path,
                                     correct_answers_df = NULL,
                                     grid_params = NULL,
                                     density = 200,
                                     debug = FALSE,
                                     fill_threshold = 0.3,
                                     name_region = NULL,
                                     id_region = NULL,
                                     partial_credit_mca = FALSE,
                                     max_debug_q = 3,
                                     threshold = 40) {
  # --- SETUP: Load template and detect master bubble locations ---
  cat("\n--- Step 1: Analyzing Blank Template ", blank_template_path, "---\n")
  template_img <- image_read_pdf(blank_template_path, density = density)[1]
  bubble_locations <- detect_bubble_locations(
    blank_template_path = blank_template_path,
    expected_count = EXPECTED_BUBBLE_COUNT,
    grid_params = grid_params,
    debug = debug,
    density = density
  )

  # --- PROCESSING: Loop through each page of the student PDF ---
  cat("\n--- Step 2: Loading Exam Answer Pages", filled_pdf_path, " ---\n")
  student_pages <- image_read_pdf(filled_pdf_path, density = density)
  num_pages <- length(student_pages)
  if (debug) cat("Found", num_pages, "page(s) in the student PDF.\n")
  cat("Processing...\n")
  pb <- txtProgressBar(min = 0, max = num_pages, style = 3, width = 50, char = "=")



  all_results <- list()
  annotated_pages_list <- list()

  for (i in seq_len(num_pages)) {
    if (debug) cat(paste0("\n--- Processing Page ", i, " of ", num_pages, " ---\n"))

    setTxtProgressBar(pb, i)

    Sys.sleep(1 / 1000) # 1 millisecond to allow for breaking

    current_page_img <- detect_and_correct_orientation(student_pages[i], template_img, debug = debug)

    # Extract student details if regions are provided
    student_details <- list(student_name = "N/A", student_id = "N/A")
    if (!is.null(name_region) && !is.null(id_region)) {
      student_details <- extract_student_info(
        pdf_path_or_img = current_page_img,
        name_region = name_region,
        id_region = id_region,
        density = density,
        debug = debug
      )
    }

    page_result <- extract_answers_omr(
      filled_img_frame = current_page_img,
      bubble_locations = bubble_locations,
      template_img = template_img,
      correct_answers_df = correct_answers_df,
      debug = debug,
      fill_threshold = fill_threshold,
      partial_credit_mca = partial_credit_mca, max_debug_q, threshold
    )

    page_summary <- list(
      page_number = i,
      student_details = student_details,
      results = page_result$results_df,
      score = page_result$score,
      total = page_result$total
    )
    all_results[[i]] <- page_summary

    # --- ZERO-SCORE WARNING ---
    if (!is.null(page_result$score) && !is.na(page_result$score) && page_result$score == 0) {
      warning(
        "Page ", i, " of '", basename(filled_pdf_path), "' scored 0.",
        "\nIf unexpected, consider a less conservative 'fill_threshold' (current value: ", fill_threshold, ").",
        call. = FALSE
      )
    }

    # --- ANNOTATION BLOCK ---
    page_to_annotate <- current_page_img

    if (!is.null(correct_answers_df) && !is.null(page_result$shifted_bubble_locations)) {
      results_df <- page_result$results_df
      page_specific_locations <- page_result$shifted_bubble_locations

      page_with_marks <- image_draw(page_to_annotate)

      for (q_row in seq_len(nrow(results_df))) {
        q_data <- results_df[q_row, ]

        # Get bubble position for annotation
        bubble_pos <- page_specific_locations[[paste0("Q", q_data$Question, "_A")]]

        if (!is.null(bubble_pos)) {
          score_to_display <- if (q_data$Score_Points < 0.1) {
            "X" # Still display 'X' if the score is exactly 0
          } else if (q_data$Score_Points == 1.0) {
            "1"
          } else {
            sprintf("%.1f", q_data$Score_Points) # Display the numerical score for non-zero, non-one scores
          }
          mark_color <- if (q_data$Score_Points == 0) "red" else "darkgreen" # Red for 'X' or green for score
          text(
            x = bubble_pos$x - (bubble_pos$radius * 4), y = bubble_pos$y,
            labels = score_to_display, col = mark_color, cex = 2.5, font = 1
          )
        }

        # 3. Draw circles around correct answers (Red for correct, regardless of student's choice)
        if (!is.na(q_data$Correct_Answer) && q_data$Correct_Answer != "NONE") {
          correct_options <- strsplit(q_data$Correct_Answer, "")[[1]]

          for (opt in correct_options) {
            bubble_id <- paste0("Q", q_data$Question, "_", opt)
            bubble_pos <- page_specific_locations[[bubble_id]]

            if (!is.null(bubble_pos)) {
              symbols(
                x = bubble_pos$x,
                y = bubble_pos$y,
                circles = bubble_pos$radius,
                inches = FALSE,
                add = TRUE,
                fg = "red",
                lwd = 4
              )
            }
          }
        }
      }
      dev.off()

      # Total score annotation (sum of individual scores for partial credit)
      total_score_sum <- round(sum(results_df$Score_Points, na.rm = TRUE), 1)
      score_text <- paste0("Total Score: ", total_score_sum, " / ", page_result$total)
      final_annotated_page <- image_annotate(page_with_marks, score_text,
        gravity = "northeast",
        size = 48, color = "red", boxcolor = "yellow", location = "+50+50"
      )
      annotated_pages_list[[i]] <- final_annotated_page
    } else {
      annotated_pages_list[[i]] <- page_to_annotate
    }
  }

  # --- OUTPUT: Save annotated PDF and return results ---
  if (length(annotated_pages_list) > 0) {
    cat("\nFinalising and saving marked PDF...\n")
    final_image <- do.call(c, annotated_pages_list)

    # Apply compression settings to reduce file size
    final_image <- image_strip(final_image) # Remove any extra profiles/metadata
    final_image <- image_flatten(final_image) # Combine layers if any (might not affect single-frame PDFs much)

    output_path <- sub("\\.pdf$", "_marked.pdf", filled_pdf_path, ignore.case = TRUE)
    # Use compression, quality, and density parameters for PDF output
    # density controls the output resolution, compression and quality control how the image is saved.
    image_write(final_image,
      path = output_path, format = "pdf",
      density = 150, # Save at 150 DPI
      compression = "Jpeg",
      quality = 75
    )
    cat("Annotated PDF saved to:", output_path, "\n")
  } else if (debug) {
    cat("No pages processed. No output file created.\n")
  }

  if (!is.null(dev.list())) {
    for (dev_i in dev.list()) {
      dev.off(dev_i)
    }
  }

  return(list(bubble_locations = bubble_locations, results_by_page = all_results))
}



apply_morphology <-
  function(binary_img,
           operation = "close",
           kernel_size = 3) {
    tryCatch(
      {
        # Try using magick's built-in morphology
        if (operation == "close") {
          return(image_morphology(binary_img, "Close", paste0("disk:", kernel_size)))
        } else if (operation == "open") {
          return(image_morphology(binary_img, "Open", paste0("disk:", kernel_size)))
        }
      },
      error = function(e) {
        # Fallback - just return the original image
        cat("Morphological operations not available, using original binary image\n")

        return(binary_img)
      }
    )
  }


run_calibration <- function(blank_template_path, grid_param_file = "grid_parameters.rds", density = 200) {
  if (!file.exists(grid_param_file)) {
    cat("Grid parameter file not found. Starting one-time interactive calibration...\n")
    cat("Please click on the four corners of the bubble grid as instructed.\n")

    template_img <- magick::image_read_pdf(blank_template_path, density = density)
    grid_params <- calibrate_bubble_grid(template_img, debug = TRUE, zoom = 2)
    saveRDS(grid_params, file = grid_param_file)
    cat(paste("Calibration complete. Grid parameters saved to '", grid_param_file, "'\n", sep = ""))
  } else {
    grid_params <- readRDS(grid_param_file)
  }
}
