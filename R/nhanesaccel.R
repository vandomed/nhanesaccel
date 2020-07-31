#' Process Accelerometer Data from NHANES 2003-2006
#'
#' Functions for processing accelerometer data from the National Health and
#' Nutrition Examination Survey (NHANES), years 2003-2006 (data included). The
#' main function, \code{process_nhanes}, generates a variety of physical
#' activity variables, including indicators of physical activity volume,
#' intensity, frequency, and duration. It also generates variables that can be
#' used to study patterns of physical activity, i.e. weekday/weekend differences
#' and hourly trends. Numerous function inputs are available to control data
#' processing algorithms like non-wear time classification and activity bout
#' detection.
#'
#' \tabular{ll}{
#' Package: \tab nhanesaccel \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.0 \cr
#' Date: \tab 2020-07-30 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' Includes three functions: \code{\link{process_nhanes}},
#' \code{\link{process_nhanes_app}}, and \code{\link{reweight_nhanes}}.
#'
#'
#' @author Dane R. Van Domelen (\email{vandomed@gmail.com}), W. Stephen Pittard,
#' and Tamara B. Harris
#'
#'
#' @references
#' Centers for Disease Control and Prevention (CDC). National Center for Health
#' Statistics (NCHS). National Health and Nutrition Examination Survey Data.
#' Hyattsville, MD: US Department of Health and Human Services, Centers for
#' Disease Control and Prevention, 2003-6.
#' \url{https://wwwn.cdc.gov/nchs/nhanes/Default.aspx}. Accessed Jan. 7, 2019.
#'
#' National Cancer Institute. Risk factor monitoring and methods: SAS programs
#' for analyzing NHANES 2003-2004 accelerometer data. Available at:
#' \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed Jan. 7, 2019.
#'
#' Van Domelen, D.R. (2018) accelerometry: Functions for processing
#' accelerometer data. R package version 3.1.2.
#' \url{http://CRAN.R-project.org/package=accelerometry}.
#'
#' Acknowledgment: This material is based upon work supported by the National
#' Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
#'
#'
#' @docType package
#' @import accelerometry
#' @importFrom dvmisc inside
#' @import haven
#' @import shiny
#' @import svMisc
#' @import survey
#' @importFrom utils data write.csv
#' @name nhanesaccel
NULL
