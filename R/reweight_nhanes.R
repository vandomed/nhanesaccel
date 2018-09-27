#' Calculate Adjusted Mobile Exam Center (MEC) Weights
#'
#' Calculates adjusted Mobile Exam Center (MEC) weights for the subsample of
#' NHANES 2003-2004 and 2005-2006 participants with valid accelerometer data,
#' making the subsample nationally representative. This function is used
#' internally by \code{\link{process_nhanes}}, but could also be used
#' independently.
#'
#' @param accel_data Numeric matrix or data frame with one row per participant.
#' It can have any number of columns but one must have respondent sequence
#' numbers (NHANES ID variable "seqn") and another must indicate whether each
#' participant has valid data (1 if valid, 0 if not valid).
#'
#' @param wave Integer value specifying NHANES wave. Choices are 1 for NHANES
#' 2003-2004 and 2 for NHANES 2005-2006.
#'
#' @param seqn_column Column number (or name) for respondent sequence numbers
#' (NHANES ID variable "SEQN").
#'
#' @param include_column Column number (or name) for 0/1 indicator for which
#' participants to include.
#'
#'
#' @return
#' Numeric matrix identical to \code{accel_data}, but with extra
#' \code{wtmec2yr_adj} column.
#'
#'
#' @examples
#' # Process NHANES 2003-2004 data using default settings
#' nhanes0304 <- process_nhanes(waves = 1)
#'
#' # Create new 'include' variable: 1 if >= 4 valid days, otherwise 0
#' nhanes0304$include2 <- ifelse(nhanes0304$valid_days >= 4, 1, 0)
#'
#' # Rename previous 2-year weight variable so new one doesn't overwrite it
#' names(nhanes0304)[10] = "wtmec2yr_adj_old"
#'
#' # Calculate new 2-year weights for subset with >= 4 valid days
#' nhanes0304 <- reweight_nhanes(accel_data = nhanes0304, wave = 1)
#'
#' # Notice that 'wtmec2yr_adj' differ from 'wtmec2yr_adj_old', and that
#' # participants with 1-3 valid days are now assigned zero weight
#' nhanes0304[1: 10, ]
#'
#' # This is just an example. In practice, it would be easier to just add
#' # `valid.days = 4' to the initial process_nhanes function call above.
#'
#'
#' @export
reweight_nhanes <- function(accel_data,
                            wave,
                            seqn_column = "seqn",
                            include_column = "include") {

  # Set variables to NULL to avoid notes from CRAN check
  wave1_demo <- wave2_demo <- NULL

  # Load demographics binary file
  if (wave == 1) {
    data("wave1_demo", envir = environment())
    demo <- wave1_demo
    rm(wave1_demo)
  }
  else {
    data("wave2_demo", envir = environment())
    demo <- wave2_demo
    rm(wave2_demo)
  }

  # Calculate adjusted weights
  wtmec2yr_adj <- rep(0, nrow(accel_data))
  for (a in 1: 2) {
    for (b in 1: 3) {
      for (c in 1: 9) {
        indices1 <- which(demo[, 2] == a & demo[, 4] == b & demo[, 3] == c)
        ids_demo <- demo[indices1, 1]
        ids_valid <-
          ids_demo[which(ids_demo %in%
                           accel_data[accel_data[, include_column] == 1, 1])]
        indices2 <-
          which(demo[, 2] == a & demo[, 4] == b & demo[, 3] == c &
                  demo[, 1] %in% accel_data[accel_data[, include_column] == 1, 1])
        indices3 <- which(accel_data[, seqn_column] %in% ids_valid)
        wtmec2yr_adj[indices3] <- demo[indices2, 5] * sum(demo[indices1, 5]) /
          sum(demo[indices2, 5])
      }
    }
  }

  # Add weights column to accel_data
  if ("wtmec2yr_adj" %in% colnames(accel_data)) {
    accel_data[, "wtmec2yr_adj"] <- wtmec2yr_adj
  } else {
    accel_data <- cbind(accel_data, wtmec2yr_adj)
  }

  # Return accel_data
  return(accel_data)

}
