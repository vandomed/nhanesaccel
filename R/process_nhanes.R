#' Process NHANES 2003-2006 Accelerometer Data (Alternate Implementation)
#'
#' Calculates a variety of physical activity variables from the time-series
#' accelerometer data in NHANES 2003-2006. A data dictionary for the variables
#' created is available here:
#' \url{https://vandomed.github.io/process_nhanes_dictionary}.
#'
#' @param waves Integer value for which wave of data to process. Choices are 1
#' for NHANES 2003-2004, 2 for NHANES 2005-2006 data, and 3 for both.
#'
#' @param directory Character string specifying directory in which to write .csv
#' file, if \code{write_csv = TRUE}.
#'
#' @param nci_methods Logical value for whether to set all arguments so as to
#' replicate the data processing methods used in the NCI's SAS programs. More
#' specifically:
#'
#' \code{valid_days = 4}
#'
#' \code{valid_wk_days = 0}
#'
#' \code{valid_we_days = 0}
#'
#' \code{int_cuts = c(100, 760, 2020, 5999)}
#'
#' \code{youth_mod_cuts = c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393,
#' 2580, 2781, 3000, 3239)}
#'
#' \code{youth_vig_cuts = c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375,
#' 5679, 6007, 6363, 6751)}
#'
#' \code{cpm_nci = TRUE}
#'
#' \code{days_distinct = TRUE}
#'
#' \code{nonwear_window = 60}
#'
#' \code{nonwear_tol = 2}
#'
#' \code{nonwear_tol_upper = 100}
#'
#' \code{nonwear_nci = TRUE}
#'
#' \code{weartime_minimum = 600}
#'
#' \code{weartime_maximum = 1440}
#'
#' \code{active_bout_length = 10}
#'
#' \code{active_bout_tol = 2}
#'
#' \code{mvpa_bout_tol_lower = 0}
#'
#' \code{vig_bout_tol_lower = 0}
#'
#' \code{active_bout_nci = TRUE}
#'
#' \code{sed_bout_tol = 0}
#'
#' \code{sed_bout_tol_maximum = 759}
#'
#' \code{artifact_thresh = 32767}
#'
#' \code{artifact_action = 3}
#'
#' If \code{TRUE}, you can still specify non-default values for \code{brevity},
#' \code{weekday_weekend}, and \code{return_form}.
#'
#' @param brevity Integer value controlling the number of physical activity
#' variables generated. Choices are 1 for basic indicators of physical activity
#' volume, 2 for addditional indicators of activity intensities, activity bouts,
#' sedentary behavior, and peak activity, and 3 for additional hourly count
#' averages.
#'
#' @param hourly_var Character string specifying what hourly activity variable
#' to record, if \code{brevity = 3}. Choices are "counts", "cpm", "sed_min",
#' "sed_bouted_10min", and "sed_breaks".
#'
#' @param hourly_wearmin Integer value specifying minimum number of wear time
#' minutes needed during a given hour to record a value for the hourly activity
#' variable.
#'
#' @param hourly_normalize Logical value for whether to normalize hourly
#' activity by number of wear time minutes.
#'
#' @param valid_days Integer value specifying minimum number of valid days to
#' be considered valid for analysis.
#'
#' @param valid_wk_days Integer value specifying minimum number of valid
#' weekdays to be considered valid for analysis.
#'
#' @param valid_we_days Integer value specifying minimum number of valid weekend
#' days to be considered valid for analysis.
#'
#' @param int_cuts Numeric vector with four cutpoints from which five intensity
#' ranges are derived. For example, \code{int_cuts = c(100, 760, 2020, 5999)}
#' creates: 0-99 = intensity 1; 100-759 = intensity level 2; 760-2019 =
#' intensity 3; 2020-5998 = intensity 4; >= 5999 = intensity 5. Intensities 1-5
#' are typically viewed as sedentary, light, lifestyle, moderate, and vigorous.
#'
#' @param youth_mod_cuts Integer vector of 12 count cutpoints for classifying
#' moderate physical activity in youth, for ages 6, 7, ..., 17. To replicate the
#' NCI's SAS programs, set \code{youth_mod_cuts = c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239)}.
#'
#' @param youth_vig_cuts Integer vector of 12 count cutpoints for classifying
#' vigorous physical activity in youth, for ages 6, 7, ..., 17. To replicate the
#' NCI's SAS programs, set \code{youth_vig_cuts = c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751)}.
#'
#' @param cpm_nci Logical value for whether to calculate average counts per
#' minute by dividing average daily counts by average daily wear time, as
#' opposed to taking the average of each day's counts per minute value. Strongly
#' recommend leave as \code{FALSE} unless you wish to replicate the NCI's SAS
#' programs.

#' @param days_distinct Logical value for whether to treat each day of data as
#' distinct, as opposed to analyzing the entire monitoring period as one
#' continuous segment.
#'
#' @param nonwear_window Integer value specifying minimum length of a non-wear
#' period.
#'
#' @param nonwear_tol Integer value specifying tolerance for non-wear algorithm,
#' i.e. number of minutes with non-zero counts allowed during a non-wear
#' interval.
#'
#' @param nonwear_tol_upper Integer value specifying maximum count value for a
#' minute with non-zero counts during a non-wear interval.
#'
#' @param nonwear_nci Logical value for whether to use non-wear algorithm from
#' NCI's SAS programs.
#'
#' @param weartime_minimum Integer value specifying minimum number of wear time
#' minutes for a day to be considered valid.
#'
#' @param weartime_maximum Integer value specifying maximum number of wear time
#' minutes for a day to be considered valid. The default is 1440, but you may
#' want to use a lower value (e.g. 1200) if participants were instructed to
#' remove devices for sleeping, but often did not.
#'
#' @param active_bout_length Integer value specifying minimum length of an
#' active bout.
#'
#' @param active_bout_tol Integer value specifying number of minutes with counts
#' outside the required range to allow during an active bout. If non-zero and
#' \code{active_bout_nci = FALSE}, specifying non-zero values for
#' \code{mvpa_bout_tol_lower} and \code{vig_bout_tol_lower} is highly
#' recommended. Otherwise minutes immediately before and after an active bout
#' will tend to be classified as part of the bout.
#'
#' @param mvpa_bout_tol_lower Integer value specifying lower cut-off for count
#' values outside of required intensity range for an MVPA bout.
#'
#' @param vig_bout_tol_lower Integer value specifying lower cut-off for count
#' values outside of required intensity range for a vigorous bout.
#'
#' @param active_bout_nci Logical value for whether to use algorithm from the
#' NCI's SAS programs for classifying active bouts.
#'
#' @param sed_bout_tol Integer value specifying number of minutes with counts
#' outside sedentary range to allow during a sedentary bout.
#'
#' @param sed_bout_tol_maximum Integer value specifying upper cut-off for count
#' values outside sedentary range during a sedentary bout.
#'
#' @param artifact_thresh Integer value specifying the smallest count value that
#' should be considered an artifact.
#'
#' @param artifact_action Integer value controlling method of correcting
#' artifacts. Choices are 1 to exclude days with one or more artifacts, 2 to
#' lump artifacts into non-wear time, 3 to replace artifacts with the average of
#' neighboring count values, and 4 to take no action.
#'
#' @param weekday_weekend Logical value for whether to calculate averages for
#' weekdays and weekend days separately (in addition to all valid days).
#'
#' @param return_form Character string controlling how variables are returned.
#' Choices are "daily" for per-day summaries, "averages" for averages across
#' all valid days, and "both" for a list containing both.
#'
#' @param write_csv Logical value for whether to write the results to a .csv
#' file in \code{directory}.
#'
#'
#' @return
#' Data frame or list of two data frames, depending on \code{return_form}.
#'
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). National Center for Health
#' Statistics (NCHS). National Health and Nutrition Examination Survey Data.
#' Hyattsville, MD: US Department of Health and Human Services, Centers for
#' Disease Control and Prevention, 2003-6
#' \url{https://wwwn.cdc.gov/nchs/nhanes/Default.aspx}. Accessed Sep. 4, 2018.
#'
#' National Cancer Institute. Risk factor monitoring and methods: SAS programs
#' for analyzing NHANES 2003-2004 accelerometer data. Available at:
#' \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed Sep. 4, 2018.
#'
#' Van Domelen, D.R. (2018) accelerometry: Functions for processing
#' accelerometer data. R package version 3.1.2.
#' \url{http://CRAN.R-project.org/package=accelerometry}.
#'
#'
#' @examples
#' # Process NHANES 2003-2006 data using default settings
#' nhanes1 <- process_nhanes()
#'
#' # Process NHANES 2003-2004 with following non-default settings: require >= 4
#' # valid days, use 90- rather than 60-minute window for non-wear algorithm,
#' # and request averages across all days and for weekdays/weekends separately
#' nhanes2 <- process_nhanes(
#'   waves = 1,
#'   valid_days = 4,
#'   nonwear_window = 90,
#'   weekday_weekend = TRUE
#' )
#'
#' # Process data according to methods used in NCI's SAS programs
#' youth_mod_cuts <- c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580,
#'                     2781, 3000, 3239)
#' youth_vig_cuts <- c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679,
#'                     6007, 6363, 6751)
#' nhanes3 <- process_nhanes(
#'   waves = 3,
#'   brevity = 2,
#'   valid_days = 4,
#'   youth_mod_cuts = youth_mod_cuts,
#'   youth_vig_cuts = youth_vig_cuts,
#'   cpm_nci = TRUE,
#'   days_distinct = TRUE,
#'   nonwear_tol = 2,
#'   nonwear_tol_upper = 100,
#'   nonwear_nci = TRUE,
#'   weartime_maximum = 1440,
#'   active_bout_tol = 2,
#'   active_bout_nci = TRUE,
#'   artifact_thresh = 32767,
#'   artifact_action = 3
#' )
#'
#' # Repeat, but use nci_methods input for convenience
#' nhanes4 <- process_nhanes(
#'   waves = 3,
#'   brevity = 2,
#'   nci_methods = TRUE
#' )
#'
#' # Results are identical
#' all.equal(nhanes3, nhanes4)
#'
#'
#' @export
process_nhanes <- function(waves = 3,
                           directory = getwd(),
                           nci_methods = FALSE,
                           brevity = 1,
                           hourly_var = "cpm",
                           hourly_wearmin = 0,
                           hourly_normalize = FALSE,
                           valid_days = 1,
                           valid_wk_days = 0,
                           valid_we_days = 0,
                           int_cuts = c(100, 760, 2020, 5999),
                           youth_mod_cuts = rep(int_cuts[3], 12),
                           youth_vig_cuts = rep(int_cuts[4], 12),
                           cpm_nci = FALSE,
                           days_distinct = FALSE,
                           nonwear_window = 60,
                           nonwear_tol = 0,
                           nonwear_tol_upper = 99,
                           nonwear_nci = FALSE,
                           weartime_minimum = 600,
                           weartime_maximum = 1440,
                           active_bout_length = 10,
                           active_bout_tol = 0,
                           mvpa_bout_tol_lower = 0,
                           vig_bout_tol_lower = 0,
                           active_bout_nci = FALSE,
                           sed_bout_tol = 0,
                           sed_bout_tol_maximum = int_cuts[2] - 1,
                           artifact_thresh = 25000,
                           artifact_action = 1,
                           weekday_weekend = FALSE,
                           return_form = "averages",
                           write_csv = FALSE) {

  # If requested, set inputs to mimic NCI's SAS programs
  if (nci_methods) {

    valid_days <- 4
    valid_wk_days <- 0
    valid_we_days <- 0
    int_cuts <- c(100, 760, 2020, 5999)
    youth_mod_cuts <- c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580,
                        2781, 3000, 3239)
    youth_vig_cuts <- c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679,
                        6007, 6363, 6751)
    cpm_nci <- TRUE
    days_distinct <- TRUE
    nonwear_window <- 60
    nonwear_tol <- 2
    nonwear_tol_upper <- 100
    nonwear_nci <- TRUE
    weartime_minimum <- 600
    weartime_maximum <- 1440
    active_bout_length <- 10
    active_bout_tol <- 2
    mvpa_bout_tol_lower <- 0
    vig_bout_tol_lower <- 0
    active_bout_nci <- TRUE
    sed_bout_tol <- 0
    sed_bout_tol_maximum <- 759
    artifact_thresh <- 32767
    artifact_action <- 3

  }

  # Save int_cuts into int_cuts_original
  int_cuts_original <- int_cuts

  # Calculate acceptable range for wear time
  weartime.range <- c(weartime_minimum, weartime_maximum)

  # Set variables to NULL to avoid notes from CRAN check
  w1 <- wave1_ages <- wave1_demo <- wave1_paxcal <- wave1_paxday <-
    wave1_paxinten <- wave1_paxstat <- wave1_seqn <- NULL
  w2 <- wave2_ages <- wave2_demo <- wave2_paxcal <- wave2_paxday <-
    wave2_paxinten <- wave2_paxstat <- wave2_paxstep <- wave2_seqn <- NULL

  # Process 03-04 data if requested
  if (waves %in% c(1, 3)) {

    # Load data for NHANES 2003-2004 and show progress bar
    cat("Loading NHANES 2003-2004 data... \n")
    progress(0, 2, progress.bar = TRUE)
    data("w1", envir = environment())
    progress(1, 2, progress.bar = TRUE)
    data("wave1_paxinten", envir = environment())
    progress(2, 2, progress.bar = TRUE)
    progress(3, 2, progress.bar = TRUE)

    # Start and end points of each ID
    mat1 <- w1[, 1: 3]
    ids <- mat1[, 1]

    # Create stat, cal, day, and age vectors
    wave1_paxstat <- w1[, 4]
    wave1_paxcal <- w1[, 5]
    wave1_paxday <- w1[, 6]
    wave1_ages <- w1[, 7]

    # Initialize lists for per-day and per-person variables
    person.vars1 <- list()
    day.vars1 <- list()

    # Initialize vectors for non-valid IDs
    invalid.ii <- c()
    invalid.ids <- c()

    # Initialize progress bar for data processing
    cat("\nProcessing NHANES 2003-2004 data...\n")
    statusvals <- c(seq(1, 7176, 72), 7176)
    statuscounter <- 0

    # Loop through data for each participant
    for (ii in 1: 7176) {

      # Update progress bar
      if (ii %in% statusvals) {
        statuscounter <- statuscounter + 1
        progress(statuscounter, progress.bar = TRUE)
      }

      # Load count data for iith participant
      counts.ii <- wave1_paxinten[mat1[ii, 2]: mat1[ii, 3]]

      # Get values for id, paxstat, and paxcal
      ids.ii <- ids[ii]
      stat.ii <- wave1_paxstat[ii]
      cal.ii <- wave1_paxcal[ii]

      # Get length of counts.ii
      n.minutes <- length(counts.ii)

      # If < weartime_minimum minutes of data or status/calibration > 1, skip
      if (n.minutes < 1440 || stat.ii > 1 || cal.ii > 1) {
        invalid.ii <- c(invalid.ii, ii)
        invalid.ids <- c(invalid.ids, ids.ii)
        day.vars1[[ii]] <- NA
        person.vars1[[ii]] <- NA
        next
      }

      # Get start/end indices for each day of monitoring
      end.indices <- seq(1440, n.minutes, 1440)
      start.indices <- end.indices - 1439
      n.days <- length(start.indices)

      # Get days of week (1 = Sunday, ..., 7 = Saturday)
      start.day <- wave1_paxday[ii]
      days <- start.day: (start.day + n.days)
      days <- ifelse(days > 7, days - 7, days)

      # Assign moderate and vigorous cutpoints according to age
      int_cuts <- int_cuts_original
      age.ii <- wave1_ages[ii]
      if (age.ii < 18) {
        int_cuts[3] <- youth_mod_cuts[age.ii - 5]
        int_cuts[4] <- youth_vig_cuts[age.ii - 5]
      }

      # Call process_uni
      vars.ii <-
        process_uni(counts = counts.ii,
                    start_day = start.day,
                    id = ids.ii,
                    brevity = brevity,
                    hourly_var = hourly_var,
                    hourly_wearmin = hourly_wearmin,
                    hourly_normalize = hourly_normalize,
                    valid_days = valid_days,
                    valid_wk_days = valid_wk_days,
                    valid_we_days = valid_we_days,
                    int_cuts = int_cuts,
                    cpm_nci = cpm_nci,
                    days_distinct = days_distinct,
                    nonwear_window = nonwear_window,
                    nonwear_tol = nonwear_tol,
                    nonwear_tol_upper = nonwear_tol_upper,
                    nonwear_nci = nonwear_nci,
                    weartime_minimum = weartime_minimum,
                    weartime_maximum = weartime_maximum,
                    active_bout_length = active_bout_length,
                    active_bout_tol = active_bout_tol,
                    mvpa_bout_tol_lower = mvpa_bout_tol_lower,
                    vig_bout_tol_lower = vig_bout_tol_lower,
                    active_bout_nci = active_bout_nci,
                    sed_bout_tol = sed_bout_tol,
                    sed_bout_tol_maximum = sed_bout_tol_maximum,
                    artifact_thresh = artifact_thresh,
                    artifact_action = artifact_action,
                    weekday_weekend = weekday_weekend,
                    return_form = return_form)

      if (return_form == "averages") {
        person.vars1[[ii]] <- vars.ii
      } else if (return_form == "daily") {
        day.vars1[[ii]] <- vars.ii
      } else {
        person.vars1[[ii]] <- vars.ii$averages
        day.vars1[[ii]] <- vars.ii$day.vars
      }

    }

    # Convert lists to matrices
    if (return_form %in% c("daily", "both")) {
      day.vars1 <- do.call(rbind, day.vars1)
      locs <- which(is.na(day.vars1[, 1]))
      day.vars1[locs, 1] <- invalid.ids
      day.vars1[locs, 3] <- 0
    }
    if (return_form %in% c("averages", "both")) {
      person.vars1 <- do.call(rbind, person.vars1)
      person.vars1[invalid.ii, 1] <- invalid.ids
      person.vars1[invalid.ii, 2: 5] <- 0
      person.vars1 <- reweight_nhanes(accel_data = person.vars1, wave = 1,
                                      seqn_column = 1, include_column = 5)
      person.vars1 <- as.data.frame(person.vars1)
      person.vars1$wtmec4yr_adj <- person.vars1$wtmec2yr_adj / 2
      person.vars1$nhanes_wave <- 1
    }

    # Clear variables
    rm(w1, wave1_paxstat, wave1_paxcal, wave1_paxday, wave1_paxinten, wave1_ages)

  }

  # Process 05-06 data if requested
  if (waves %in% c(2, 3)) {

    # Load data for NHANES 2005-2006 and show progress bar
    cat("\nLoading NHANES 2005-2006 data...\n")
    if (brevity == 1) {
      progress(0, 2, progress.bar = TRUE)
      data("w2", envir = environment())
      progress(1, 2, progress.bar = TRUE)
      data("wave2_paxinten", envir = environment())
      progress(2, 2, progress.bar = TRUE)
      progress(3, 2, progress.bar = TRUE)
    } else {
      progress(0, 3, progress.bar = TRUE)
      data("w2", envir=environment())
      progress(1, 3, progress.bar = TRUE)
      data("wave2_paxinten", envir = environment())
      progress(2, 3, progress.bar = TRUE)
      data("wave2_paxstep", envir = environment())
      progress(3, 3, progress.bar = TRUE)
      progress(4, 3, progress.bar = TRUE)
    }

    # Start and end points of each ID
    mat1 <- w2[, 1: 3]
    ids <- mat1[, 1]

    # Create stat, cal, day, and age vectors
    wave2_paxstat <- w2[, 4]
    wave2_paxcal <- w2[, 5]
    wave2_paxday <- w2[, 6]
    wave2_ages <- w2[, 7]

    # Initialize lists for per-day and per-person variables
    person.vars2 <- list()
    day.vars2 <- list()

    # Initialize vectors for non-valid IDs
    invalid.ii <- c()
    invalid.ids <- c()

    # Initialize progress bar for data processing
    cat("\nProcessing NHANES 2005-2006 data...\n")
    statusvals <- c(seq(1, 7455, 75), 7455)
    statuscounter <- 0

    # Loop through data for each participant
    for (ii in 1: 7455) {

      # Update progress bar
      if (ii %in% statusvals) {
        statuscounter <- statuscounter + 1
        progress(statuscounter, progress.bar = TRUE)
      }

      # Load count and step data for iith participant
      counts.ii <- wave2_paxinten[mat1[ii, 2]: mat1[ii, 3]]
      if (brevity %in% c(2, 3)) {
        steps.ii <- wave2_paxstep[mat1[ii, 2]: mat1[ii, 3]]
      } else {
        steps.ii <- NULL
      }

      # Get values for id, paxstat, and paxcal
      ids.ii <- ids[ii]
      stat.ii <- wave2_paxstat[ii]
      cal.ii <- wave2_paxcal[ii]

      # Get length of counts.ii
      n.minutes <- length(counts.ii)

      # If < weartime_minimum minutes of data or status/calibration > 1, skip
      if (n.minutes < 1440 || stat.ii > 1 || cal.ii > 1) {
        invalid.ii <- c(invalid.ii, ii)
        invalid.ids <- c(invalid.ids, ids.ii)
        day.vars2[[ii]] <- NA
        person.vars2[[ii]] <- NA
        next
      }

      # Get start/end indices for each day of monitoring
      end.indices <- seq(1440, n.minutes, 1440)
      start.indices <- end.indices - 1439
      n.days <- length(start.indices)

      # Get days of week (1 = Sunday, ..., 7 = Saturday)
      start.day <- wave2_paxday[ii]
      days <- start.day: (start.day + n.days)
      days <- ifelse(days > 7, days - 7, days)

      # Assign moderate and vigorous cutpoints according to age
      int_cuts <- int_cuts_original
      age.ii <- wave2_ages[ii]
      if (age.ii < 18) {
        int_cuts[3] <- youth_mod_cuts[age.ii - 5]
        int_cuts[4] <- youth_vig_cuts[age.ii - 5]
      }

      # Call process_uni
      vars.ii <-
        process_uni(counts = counts.ii,
                    steps = steps.ii,
                    start_day = start.day,
                    id = ids.ii,
                    brevity = brevity,
                    hourly_var = hourly_var,
                    hourly_wearmin = hourly_wearmin,
                    hourly_normalize = hourly_normalize,
                    valid_days = valid_days,
                    valid_wk_days = valid_wk_days,
                    valid_we_days = valid_we_days,
                    int_cuts = int_cuts,
                    cpm_nci = cpm_nci,
                    days_distinct = days_distinct,
                    nonwear_window = nonwear_window,
                    nonwear_tol = nonwear_tol,
                    nonwear_tol_upper = nonwear_tol_upper,
                    nonwear_nci = nonwear_nci,
                    weartime_minimum = weartime_minimum,
                    weartime_maximum = weartime_maximum,
                    active_bout_length = active_bout_length,
                    active_bout_tol = active_bout_tol,
                    mvpa_bout_tol_lower = mvpa_bout_tol_lower,
                    vig_bout_tol_lower = vig_bout_tol_lower,
                    active_bout_nci = active_bout_nci,
                    sed_bout_tol = sed_bout_tol,
                    sed_bout_tol_maximum = sed_bout_tol_maximum,
                    artifact_thresh = artifact_thresh,
                    artifact_action = artifact_action,
                    weekday_weekend = weekday_weekend,
                    return_form = return_form)

      if (return_form == "averages") {
        person.vars2[[ii]] <- vars.ii
      } else if (return_form == "daily") {
        day.vars2[[ii]] <- vars.ii
      } else {
        person.vars2[[ii]] <- vars.ii$averages
        day.vars2[[ii]] <- vars.ii$day.vars
      }

    }

    # Convert lists to matrices
    if (return_form %in% c("daily", "both")) {
      day.vars2 <- do.call(rbind, day.vars2)
      locs <- which(is.na(day.vars2[, 1]))
      day.vars2[locs, 1] <- invalid.ids
      day.vars2[locs, 3] <- 0
    }
    if (return_form %in% c("averages", "both")) {
      person.vars2 <- do.call(rbind, person.vars2)
      person.vars2[invalid.ii, 1] <- invalid.ids
      person.vars2[invalid.ii, 2: 5] <- 0
      person.vars2 <- reweight_nhanes(accel_data = person.vars2, wave = 2,
                                      seqn_column = 1, include_column = 5)
      person.vars2 <- as.data.frame(person.vars2)
      person.vars2$wtmec4yr_adj <- person.vars2$wtmec2yr_adj / 2
      person.vars2$nhanes_wave <- 2
    }

    # Clear variables
    rm(w2, wave2_paxstat, wave2_paxcal, wave2_paxday, wave2_paxinten,
       wave2_ages)

  }

  # Combine 2003-2004 and 2005-2006 data if necessary
  if (waves == 1) {

    if (return_form == "averages") {

      person.vars <- person.vars1
      names(person.vars)[1] <- "seqn"

    } else if (return_form == "daily") {

      day.vars <- day.vars1
      names(day.vars)[1] <- "seqn"

    } else {

      day.vars <- day.vars1
      person.vars <- person.vars1

      names(day.vars)[1] <- "seqn"
      names(person.vars)[1] <- "seqn"

    }

  } else if (waves == 2) {

    if (return_form == "averages") {

      person.vars <- person.vars2
      names(person.vars)[1] <- "seqn"

    } else if (return_form == "daily") {

      day.vars <- day.vars2
      names(day.vars)[1] <- "seqn"

    } else {

      day.vars <- day.vars2
      person.vars <- person.vars2

      names(day.vars)[1] <- "seqn"
      names(person.vars)[1] <- "seqn"

    }

  } else {

    if (return_form == "averages") {

      person.vars <- rbind(person.vars1, person.vars2)
      names(person.vars)[1] <- "seqn"

    } else if (return_form == "daily") {

      day.vars <- rbind(day.vars1, day.vars2)
      names(day.vars)[1] <- "seqn"

    } else {

      day.vars <- rbind(day.vars1, day.vars2)
      person.vars <- rbind(person.vars1, person.vars2)

      names(day.vars)[1] <- "seqn"
      names(person.vars)[1] <- "seqn"

    }
  }

  # Tell user that data processing is complete
  cat("\nDone.\n")

  # Write .csv file(s) according to write_csv and return_form
  if (write_csv) {

    # Get date for filename
    curdate <- as.character(strftime(Sys.Date(), format = "%Y-%m-%d"))

    # Write per-day file if requested
    if (return_form %in% c("daily", "both")) {
      filestem <- "accel_days_"
      dayfile <- paste(filestem, curdate, ".csv", sep = "")
      if (file.exists(dayfile)) {
        reps <- 1
        repeat {
          reps <- reps + 1
          daytest <- paste(filestem, curdate, "_", reps, ".csv", sep = "")
          if (!file.exists(daytest)) {
            dayfile <- daytest
            break
          }
        }
      }
      write.csv(x = day.vars, file = dayfile, quote = FALSE, row.names = FALSE,
                na = "")
    }

    # Write per-person file if requested
    if (return_form %in% c("averages", "both")) {
      filestem <- "accel_aves_"
      personfile <- paste(filestem, curdate, ".csv", sep = "")
      if (file.exists(personfile)) {
        reps <- 1
        repeat {
          reps <- reps + 1
          persontest <- paste(filestem, curdate, "_", reps, ".csv", sep = "")
          if (! file.exists(persontest)) {
            personfile <- persontest
            break
          }
        }
      }
      write.csv(x = person.vars, file = personfile, quote = FALSE,
                row.names = FALSE, na = "")
    }

    # Write .csv file with function settings
    settings <-
      c("waves", waves,
        "valid_days", valid_days,
        "valid_wk_days", valid_wk_days,
        "valid_we_days", valid_we_days,
        "int_cuts", int_cuts,
        "youth_mod_cuts", youth_mod_cuts,
        "youth_vig_cuts", youth_vig_cuts,
        "cpm_nci", cpm_nci,
        "days_distinct", days_distinct,
        "nonwear_window", nonwear_window,
        "nonwear_tol", nonwear_tol,
        "nonwear_tol_upper", nonwear_tol_upper,
        "nonwear_nci", nonwear_nci,
        "weartime_minimum", weartime_minimum,
        "weartime_maximum", weartime_maximum,
        "active_bout_length", active_bout_length,
        "active_bout_tol", active_bout_tol,
        "mvpa_bout_tol_lower", mvpa_bout_tol_lower,
        "vig_bout_tol_lower", vig_bout_tol_lower,
        "active_bout_nci", active_bout_nci,
        "sed_bout_tol", sed_bout_tol,
        "sed_bout_tol_maximum", sed_bout_tol_maximum,
        "artifact_thresh", artifact_thresh,
        "artifact_action", artifact_action)
    settings <- as.data.frame(settings)
    colnames(settings) <- NULL
    filestem <- "settings_"
    settingsfile <- paste(filestem, curdate, ".csv", sep = "")
    if (file.exists(settingsfile)) {
      reps <- 1
      repeat {
        reps <- reps + 1
        settingstest <- paste(filestem, curdate, "_", reps, ".csv", sep = "")
        if (! file.exists(settingstest)) {
          settingsfile <- settingstest
          break
        }
      }
    }
    write.csv(x = settings, file = settingsfile, quote = FALSE,
              row.names = FALSE)

    # Print message to user
    cat(paste("\nPlease see", directory, "for .csv file(s) with NHANES physical activity variables.\n"))
  }

  # Return data frame(s)
  if (return_form == "averages") {
    ret <- person.vars
  } else if (return_form == "daily") {
    ret <- as.data.frame(day.vars)
  } else {
    ret <- list(person.vars = person.vars, day.vars = as.data.frame(day.vars))
  }
  return(ret)
}
