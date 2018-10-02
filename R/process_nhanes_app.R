#' Shiny App to Process NHANES 2003-2006 Accelerometer Data
#'
#' Simply a Shiny app that provides a GUI for \code{\link{process_nhanes}}.
#'
#' @export
process_nhanes_app <- function() {

  #require(shiny)
  shinyApp(

    # ui part -----------------------------------------------------------------

    ui <- fluidPage(

      # Application title
      titlePanel("Web App for Processing NHANES Accelerometer Data"),

      br(),

      fluidRow(
        column(12,

               navlistPanel(
                 widths = c(4, 8),

                 tabPanel("Intro",

                          actionButton(
                            inputId = "reset1",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci1",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          h4("Important Note!"),

                          span("You should click ", strong("Open in Browser"), "before doing anything. Otherwise the file download at the end won't work, due to a Shiny-related bug."),

                          h4("Basic background info"),

                          span("Accelerometry was included in the 2003-2004 and 2005-2006 waves of the National Health and Nutrition Examination Survey (NHANES). Participants age >= 6 years were asked to wear an ActiGraph GT1M on a belt for 7 consecutive days, removing it for water activities and sleeping. You can read more about the study in the famous",
                               a("Troiano et al. paper",
                                 href="https://www.ncbi.nlm.nih.gov/pubmed/18091006",
                                 target="_blank"),
                               "."),

                          h4("How this app works"),

                          span("The app is basically a GUI for the R package ",
                               a("nhanesaccel",
                                 href="https://github.com/vandomed/nhanesaccel",
                                 target="_blank"),
                               "which contains the NHANES data and functions for processing it. To use the app, go through the links on the sidebar and adjust options as you see fit. On the last tab, you'll be able to process the data and download a file for whatever statistical software package you prefer."),

                          span("At any point, you can click ", strong("Reset all options"), "to go back to the defaults or ", strong("Set NCI defaults"), "to mimic the National Cancer Institute's ",
                               a("SAS programs",
                                 href="",
                                 target="_blank"),
                               ".")

                 ),

                 tabPanel("Years",

                          actionButton(
                            inputId = "reset2",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci2",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(
                            radioButtons(
                              inputId = "waves",
                              label = "What years of data do you want to process?",
                              choiceNames = list("2003-2004", "2005-2006", "Both"),
                              choiceValues = list(1, 2, 3),
                              selected = 3
                            )
                          )

                          # h4("Notes"),
                          #
                          # p("The device and protocol were exactly the same for both the 2003-2004 and 2005-2006 wave. One distinction is that the accelerometer recorded steps in 2005-2006 but not 2003-2004."),
                          #
                          # p("It is usually best to use the combined 2003-2006 dataset to explore research questions, but in some cases this may not be possible because certain NHANES components are included in one wave but not the other.")

                 ),

                 tabPanel("Wear time",

                          actionButton(
                            inputId = "reset3",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci3",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            radioButtons(
                              inputId = "nonwear_nci",
                              label = "What algorithm do you want to use?",
                              inline = TRUE,
                              choiceNames = list("NCI", "Simple"),
                              choiceValues = list(TRUE, FALSE),
                              selected = FALSE
                            ),
                            radioButtons(
                              inputId = "days_distinct",
                              label = "Should the non-wear algorithm be applied separately to each 24-hour period? This choice also applies to the bout detection algorithm.",
                              inline = TRUE,
                              choiceNames = list("Yes", "No"),
                              choiceValues = list(TRUE, FALSE),
                              selected = FALSE,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "nonwear_window",
                              label = "Minimum length of non-wear period, in minutes",
                              value = 60, min = 5, max = 120,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "nonwear_tol",
                              label = "Tolerance, in minutes ('tol')",
                              value = 0, min = 0, max = 5,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "nonwear_tol_upper",
                              label = "Max count value for minutes with non-zero counts",
                              value = 99, min = 0, max = 1000,
                              width = '90%'
                            )

                          ),

                          h4("Notes"),

                          p("You can choose between two types of algorithms: the NCI's algorithm [1] and a simpler one. The NCI's algorithm is more liberal in classifying non-wear, because it was designed to also remove periods when participants slept while wearing the device."),

                          p("The NCI's algorithm defines a non-wear period as an interval of length 'window' that starts with a count value of 0, does not contain any periods with ('tol' + 1) consecutive non-zero count values, and does not contain any counts greater than 'tol.upper'. If these criteria are met, the  non-wear period continues until there are ('tol' + 1) consecutive non-zero count values or a single count value greater than 'tol.upper'."),

                          p("The 'simple' alternative is more straightforward: it classifies as non-wear any interval of length 'window' in which no more than 'tol' counts are non-zero, and those are still less than 'tol.upper'."),

                          h4("References"),

                          span("1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: "),
                          a("http://riskfactor.cancer.gov/tools/nhanes_pam",
                            href="https://epi.grants.cancer.gov/nhanes_pam/",
                            target="_blank")

                 ),

                 tabPanel("Artifacts",

                          actionButton(
                            inputId = "reset4",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci4",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(
                            numericInput(
                              inputId = "artifact_thresh",
                              label = "What cutpoint do you want to use to flag counts as 'artifacts' rather than meaningful measurements?",
                              value = 25000, min = 1, max = 32768
                            ),
                            radioButtons(
                              inputId = "artifact_action",
                              label = "How do you want to handle artifacts?",
                              choiceNames = list("Exclude days with 1 or more artifacts",
                                                 "Lump minutes with artifacts into non-wear time",
                                                 "Replace artifacts with average of neighboring count values",
                                                 "Nothing"),
                              choiceValues = 1: 4
                            )

                          )

                 ),

                 tabPanel("Compliance",

                          actionButton(
                            inputId = "reset5",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci5",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(
                            sliderInput(
                              inputId = "weartime_range",
                              width = '80%',
                              label = "How many minutes of wear time are required for a day to be considered valid?",
                              min = 60, max = 1440,
                              value = c(600, 1440), step = 1
                            ),
                            radioButtons(
                              inputId = "valid_days",
                              width = '80%',
                              inline = TRUE,
                              label = "How many valid days must a participant have to be included?",
                              choices = 1: 7, selected = 1
                            ),
                            radioButtons(
                              inputId = "valid_wk_days",
                              width = '80%',
                              inline = TRUE,
                              label = "How many valid weekdays must a participant have to be included?",
                              choices = 0: 5, selected = 0
                            ),
                            radioButtons(
                              inputId = "valid_we_days",
                              inline = TRUE,
                              width = '80%',
                              label = "How many valid weekend days must a participant have to be included?",
                              choices = 0: 2, selected = 0
                            )
                          ),

                          h4("Notes"),

                          p("Standard practice is to require at least 10 hours (600 minutes) for a day to be considered valid for analysis. But it may be prudent to also apply a maximum, particularly in NHANES, where some participants appear to have broken protocol and slept while wearing the device (the non-wear algorithm may not always catch these instances). For most variables, e.g. counts per minute of wear time, sedentary time in minutes or as a percent of wear time, days with close to 24 hours of wear time will tend to have very atypical and non-representative values.")

                 ),

                 tabPanel("Variables to calculate",

                          actionButton(
                            inputId = "reset6",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci6",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(
                            radioButtons(
                              inputId = "brevity",
                              width = '90%',
                              label = "What physical activity variables do you want to calculate?",
                              choiceNames = list("Basic indicators of volume",
                                                 "Basic indicators of volume + intensity/bout variables",
                                                 "Basic indicators of volume + intensity/bout variables + an hourly variable"),
                              choiceValues = 1: 3,
                              selected = 1
                            ),
                            radioButtons(
                              inputId = "weekday_weekend",
                              width = '90%',
                              label = "In addition to averages across all valid days of monitoring, do you want to calculate averages for weekdays and weekends separately?",
                              choiceNames = list("Yes", "No"),
                              choiceValues = c(TRUE, FALSE),
                              selected = FALSE
                            )
                          ),

                          h4("Notes"),

                          span("See the ",
                               a("data dictionary",
                                 href="https://github.com/vandomed/nhanesaccel/blob/master/process_nhanes_dictionary.csv",
                                 target="_blank"),
                               "for a list of variables that can be generated. The 3 choices above correspond to 'brevity' values of 1, 2, and 3. The dictionary lists all of the variables and for what values of 'brevity' they are calculated.", p(), "If weekday/weekend averages are requested, the dataset will also include those variables, which will be named as indicated in the dictionary but with 'wk_' and 'we_' prefixes.")

                 ),

                 tabPanel("Intensity cutpoints",

                          actionButton(
                            inputId = "reset7",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci7",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            numericInput(
                              inputId = "int_cuts1",
                              label = "Count cutpoint 1 (lowest count value for 'light' intensity)",
                              value = 100, min = 0, max = 32767,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "int_cuts2",
                              label = "Count cutpoint 2 (lowest count value for 'lifestyle' intensity)",
                              value = 760, min = 0, max = 32767,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "int_cuts3",
                              label = "Count cutpoint 3 (lowest count value for 'moderate' intensity)",
                              value = 2020, min = 0, max = 32767,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "int_cuts4",
                              label = "Count cutpoint 4 (lowest count value for 'vigorous' intensity)",
                              value = 5999, min = 0, max = 32767,
                              width = '90%'
                            ),
                            textInput(
                              inputId = 'mod_cut_youth',
                              label = "If you want to use different 'moderate' cutpoints for youth, enter cutpoints for ages 6, 7, ..., 17, separated by commas.",
                              width = '90%'
                            ),
                            textInput(
                              inputId = 'vig_cut_youth',
                              label = "If you want to use different 'vigorous' cutpoints for youth, enter cutpoints for ages 6, 7, ..., 17, separated by commas.",
                              width = '90%'
                            )
                          ),

                          h4("Notes"),

                          span("To replicate the NCI's SAS programs for moderate and vigorous cutpoints in youth, use the following:", p(),
                               code("youth_mod_cuts = c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239)"),
                               p(),
                               code("youth_vig_cuts = c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751)")),

                          h4("References"),

                          span("1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: "),
                          a("http://riskfactor.cancer.gov/tools/nhanes_pam",
                            href="https://epi.grants.cancer.gov/nhanes_pam/",
                            target="_blank")

                 ),

                 tabPanel("Bouts",

                          actionButton(
                            inputId = "reset8",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci8",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            radioButtons(
                              inputId = "active_bout_nci",
                              label = "Which algorithm do you want to use for MVPA/vigorous bouts?",
                              inline = TRUE,
                              choiceNames = list("NCI", "Simple"),
                              choiceValues = list(TRUE, FALSE),
                              selected = FALSE
                            ),
                            numericInput(
                              inputId = "active_bout_length",
                              label = "Minimum length of MVPA/vigorous bouts, in minutes",
                              value = 10,
                              min = 2, max = 120,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "active_bout_tol",
                              label = "Tolerance for MVPA/vigorous bouts, in minutes",
                              value = 0, min = 0, max = 10,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "mvpa_bout_tol_lower",
                              label = "For MVPA bouts, minimum count value for minutes outside range",
                              value = 0, min = 0, max = 100000,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "vig_bout_tol_lower",
                              label = "For vigorous bouts, minimum count value for minutes outside range",
                              value = 0, min = 0, max = 100000,
                              width = '90%'
                            )

                          ),

                          wellPanel(

                            numericInput(
                              inputId = "sed_bout_tol",
                              label = "Tolerance for sedentary bouts, in minutes",
                              value = 0, min = 0, max = 10,
                              width = '90%'
                            ),
                            numericInput(
                              inputId = "sed_bout_tol_maximum",
                              label = "For sedentary bouts, maximum count value for minutes outside range",
                              value = 0, min = 0, max = 100000,
                              width = '90%'
                            )

                          ),

                          h4("Notes"),

                          p("The bout detection algorithm is just like the non-wear algorithm. You can choose between the NCI's algorithm [1] and a simpler one."),

                          p("The NCI's algorithm defines a bout as an interval of length 'window' that starts with a count value in the target range, does not contain any periods with ('tol' + 1) consecutive count values outside of the target range, and does not contain any counts greater less than 'tol_lower'. If these criteria are met, the  bout continues until there are ('tol' + 1) consecutive count values outside of the target range or a single count value less than 'tol_lower'."),

                          p("The 'simple' alternative is more straightforward: it classifies as a bout any interval of length 'window' in which no more than 'tol' counts are outside the target range, and those are still greater than 'tol_lower'."),

                          h4("References"),

                          span("1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: "),
                          a("http://riskfactor.cancer.gov/tools/nhanes_pam",
                            href="https://epi.grants.cancer.gov/nhanes_pam/",
                            target="_blank")

                 ),

                 tabPanel("Miscellaneous",

                          actionButton(
                            inputId = "reset9",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci9",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            radioButtons(
                              inputId = "cpm_nci",
                              label = "Do you want to calculate average counts per minute (cpm) as average daily counts divided by average daily wear time? (Not recommended!)",
                              inline = TRUE,
                              choiceNames = list("Yes", "No"),
                              choiceValues = list(TRUE, FALSE),
                              selected = FALSE
                            )

                          ),

                          wellPanel(

                            radioButtons(
                              inputId = "hourly_var",
                              label = "What hourly activity variable do you want to record?",
                              choiceNames = list("Counts", "Counts per minute", "Sedentary minutes", "Sedentary minutes in >= 10-min bouts", "Sedentary breaks"),
                              choiceValues = list("counts", "cpm", "sed_min", "sed_bouted_10min", "sed_breaks"),
                              selected = "counts",
                              width = '90%'
                            ),

                            numericInput(
                              inputId = "hourly_wearmin",
                              label = "How much wear time per hour do you want to require to record a measurement?",
                              value = 60, min = 0, max = 60,
                              width = '90%'
                            ),

                            radioButtons(
                              inputId = "hourly_normalize",
                              label = "Do you want the hourly variable to be normalized by wear time?",
                              choiceNames = list("Yes", "No"),
                              choiceValues = list(TRUE, FALSE),
                              selected = FALSE,
                              width = '90%'
                            )

                          )

                 ),

                 tabPanel("Other data to merge",

                          actionButton(
                            inputId = "reset10",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci10",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            checkboxGroupInput(
                              inputId = "datasets",
                              label = "What other datasets do you want to merge in?",
                              choiceNames = list("Demographics", "Occupation", "Body measures", "Blood pressure"),
                              choiceValues = list("demo", "ocq", "bmx", "bpx")
                            )

                          )

                 ),

                 tabPanel("Process data and download files",

                          actionButton(
                            inputId = "reset11",
                            label = "Reset all options",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          span(style = "margin-left :5px"),

                          actionButton(
                            inputId = "set_nci11",
                            label = "Set NCI defaults",
                            icon = icon("play-circle"),
                            width = '35%'
                          ),

                          br(),
                          br(),

                          wellPanel(

                            actionButton(
                              inputId = "process",
                              label = "Process data",
                              icon = icon("play-circle"),
                              width = '35%'
                            ),
                            br(),
                            br(),
                            downloadLink("download_csv", "Download .csv file"),
                            p(),
                            downloadLink("download_rda", "Download .rda file (R)"),
                            p(),
                            downloadLink("download_dta", "Download .dta file (Stata)"),
                            p(),
                            downloadLink("download_sas7bdat", "Download .sas7bdat file (SAS)"),
                            p(),
                            downloadLink("download_sav", "Download .sav file (SPSS)")

                          )

                 )

               )

        )
      )

    ),


    # server part -------------------------------------------------------------

    # Define server logic required to draw a histogram
    server <- function(input, output, session) {

      # When 'Reset all options' gets clicked
      observeEvent(
        eventExpr = c(input$reset1, input$reset2, input$reset3,
                      input$reset4, input$reset5, input$reset6,
                      input$reset7, input$reset8, input$reset9,
                      input$reset10, input$reset11),
        ignoreInit = TRUE,
        handlerExpr = {

          updateRadioButtons(session,
                             inputId = "valid_days",
                             selected = 1)
          updateRadioButtons(session,
                             inputId = "valid_wk_days",
                             selected = 0)
          updateRadioButtons(session,
                             inputId = "valid_we_days",
                             selected = 0)
          updateNumericInput(session,
                             inputId = "int_cuts1",
                             value = 100)
          updateNumericInput(session,
                             inputId = "int_cuts2",
                             value = 760)
          updateNumericInput(session,
                             inputId = "int_cuts3",
                             value = 2020)
          updateNumericInput(session,
                             inputId = "int_cuts4",
                             value = 5999)
          updateRadioButtons(session,
                             inputId = "cpm_nci",
                             selected = FALSE)
          updateRadioButtons(session,
                             inputId = "days_distinct",
                             selected = FALSE)
          updateNumericInput(session,
                             inputId = "nonwear_window",
                             value = 60)
          updateNumericInput(session,
                             inputId = "nonwear_tol",
                             value = 0)
          updateNumericInput(session,
                             inputId = "nonwear_tol_upper",
                             value = 99)
          updateRadioButtons(session,
                             inputId = "nonwear_nci",
                             selected = FALSE)
          updateSliderInput(session,
                            inputId = "weartime_range",
                            value = c(600, 1440))
          updateNumericInput(session,
                             inputId = "active_bout_length",
                             value = 10)
          updateNumericInput(session,
                             inputId = "active_bout_tol",
                             value = 0)
          updateNumericInput(session,
                             inputId = "mvpa_bout_tol_lower",
                             value = 0)
          updateNumericInput(session,
                             inputId = "vig_bout_tol_lower",
                             value = 0)
          updateRadioButtons(session,
                             inputId = "active_bout_nci",
                             selected = FALSE)
          updateNumericInput(session,
                             inputId = "sed_bout_tol",
                             value = 0)
          updateNumericInput(session,
                             inputId = "sed_bout_tol_maximum",
                             value = 759)
          updateNumericInput(session,
                             inputId = "artifact_thresh",
                             value = 25000)
          updateRadioButtons(session,
                             inputId = "artifact_action",
                             selected = 1)

          showModal(modalDialog(
            title = "Options reset",
            "Options have been returned to their original default values."
          ))

        }
      )


      # When 'Set NCI defaults' gets clicked
      observeEvent(
        eventExpr = c(input$set_nci1, input$set_nci2, input$set_nci3,
                      input$set_nci4, input$set_nci5, input$set_nci6,
                      input$set_nci7, input$set_nci8, input$set_nci9,
                      input$set_nci10, input$set_nci11),
        ignoreInit = TRUE,
        handlerExpr = {

          updateRadioButtons(session,
                             inputId = "valid_days",
                             selected = 4)
          updateRadioButtons(session,
                             inputId = "valid_wk_days",
                             selected = 0)
          updateRadioButtons(session,
                             inputId = "valid_we_days",
                             selected = 0)
          updateNumericInput(session,
                             inputId = "int_cuts1",
                             value = 100)
          updateNumericInput(session,
                             inputId = "int_cuts2",
                             value = 760)
          updateNumericInput(session,
                             inputId = "int_cuts3",
                             value = 2020)
          updateNumericInput(session,
                             inputId = "int_cuts4",
                             value = 5999)
          updateRadioButtons(session,
                             inputId = "cpm_nci",
                             selected = TRUE)
          updateRadioButtons(session,
                             inputId = "days_distinct",
                             selected = TRUE)
          updateNumericInput(session,
                             inputId = "nonwear_window",
                             value = 60)
          updateNumericInput(session,
                             inputId = "nonwear_tol",
                             value = 2)
          updateNumericInput(session,
                             inputId = "nonwear_tol_upper",
                             value = 100)
          updateRadioButtons(session,
                             inputId = "nonwear_nci",
                             selected = TRUE)
          updateSliderInput(session,
                            inputId = "weartime_range",
                            value = c(600, 1440))
          updateNumericInput(session,
                             inputId = "active_bout_length",
                             value = 10)
          updateNumericInput(session,
                             inputId = "active_bout_tol",
                             value = 2)
          updateNumericInput(session,
                             inputId = "mvpa_bout_tol_lower",
                             value = 0)
          updateNumericInput(session,
                             inputId = "vig_bout_tol_lower",
                             value = 0)
          updateRadioButtons(session,
                             inputId = "active_bout_nci",
                             selected = TRUE)
          updateNumericInput(session,
                             inputId = "sed_bout_tol",
                             value = 0)
          updateNumericInput(session,
                             inputId = "sed_bout_tol_maximum",
                             value = 759)
          updateNumericInput(session,
                             inputId = "artifact_thresh",
                             value = 32767)
          updateRadioButtons(session,
                             inputId = "artifact_action",
                             selected = 3)

          showModal(modalDialog(
            title = "Options set",
            "Options have been set to mimic the NCI's SAS programs. You can still adjust each parameter individually if you like."
          ))

        }
      )

      # When 'Process data' gets clicked
      accel <- NULL
      observeEvent(
        eventExpr = input$process,
        ignoreInit = TRUE,
        handlerExpr = {

          showModal(modalDialog(
            title = "Data processing underway...",
            "This could take 10 seconds to 2 minutes, depending on your selections. Box will close when finished."
          ))

          # Fix up some variables
          brevity <- as.integer(input$brevity)
          int_cuts <- c(input$int_cuts1, input$int_cuts2, input$int_cuts3, input$int_cuts4)
          if (! is.null(input$youth_mod_cuts)) {
            youth_mod_cuts <- as.integer(youth_mod_cuts)
          } else {
            youth_mod_cuts <- rep(int_cuts[3], 12)
          }
          if (! is.null(input$youth_vig_cuts)) {
            youth_vig_cuts <- as.integer(youth_vig_cuts)
          } else {
            youth_vig_cuts <- rep(int_cuts[4], 12)
          }

          # Process accelerometer data
          accel <- process_nhanes(
            waves = input$waves,
            brevity = brevity,
            hourly_var = input$hourly_var,
            hourly_wearmin = input$hourly_wearmin,
            hourly_normalize = input$hourly_normalize == "TRUE",
            valid_days = input$valid_days,
            valid_wk_days = input$valid_wk_days,
            valid_we_days = input$valid_we_days,
            int_cuts = int_cuts,
            youth_mod_cuts = youth_mod_cuts,
            youth_vig_cuts = youth_vig_cuts,
            cpm_nci = input$cpm_nci == "TRUE",
            days_distinct = input$days_distinct == "TRUE",
            nonwear_window = input$nonwear_window,
            nonwear_tol = input$nonwear_tol,
            nonwear_tol_upper = input$nonwear_tol_upper,
            nonwear_nci = input$nonwear_nci == "TRUE",
            weartime_minimum = input$weartime_range[1],
            weartime_maximum = input$weartime_range[2],
            active_bout_length = input$active_bout_length,
            active_bout_tol = input$active_bout_tol,
            mvpa_bout_tol_lower = input$mvpa_bout_tol_lower,
            vig_bout_tol_lower = input$vig_bout_tol_lower,
            active_bout_nci = input$active_bout_nci == "TRUE",
            sed_bout_tol = input$sed_bout_tol,
            sed_bout_tol_maximum = input$sed_bout_tol_maximum,
            artifact_thresh = input$artifact_thresh,
            artifact_action = input$artifact_action,
            weekday_weekend = input$weekday_weekend == "TRUE",
            return_form = "averages"
          )

          # Merge in other datasets
          if (! is.null(input$datasets)) {

            if ("demo" %in% input$datasets) {

              if (input$waves == 1) {
                #data(demo_c)
                names(demo_c) <- tolower(names(demo_c))
                accel <<- dplyr::left_join(x = accel, y = demo_c)
              } else if (input$waves == 2) {
                #data(demo_d)
                names(demo_d) <- tolower(names(demo_d))
                accel <<- dplyr::left_join(x = accel, y = demo_d)
              } else if (input$waves == 3) {
                demo <- dplyr::full_join(demo_c, demo_d)
                names(demo) <- tolower(names(demo))
                accel <- dplyr::left_join(accel, demo)
              }

            }

            if ("ocq" %in% input$datasets) {

              if (input$waves == 1) {
                #data(ocq_c)
                names(ocq_c) <- tolower(names(ocq_c))
                accel <<- dplyr::left_join(x = accel, y = ocq_c)
              } else if (input$waves == 2) {
                #data(ocq_d)
                names(ocq_d) <- tolower(names(ocq_d))
                accel <<- dplyr::left_join(x = accel, y = ocq_d)
              } else if (input$waves == 3) {
                ocq <- dplyr::full_join(ocq_c, ocq_d)
                names(ocq) <- tolower(names(ocq))
                accel <<- dplyr::left_join(accel, ocq)
              }

            }

            if ("bmx" %in% input$datasets) {

              if (input$waves == 1) {
                #data(bmx_c)
                names(bmx_c) <- tolower(names(bmx_c))
                accel <<- dplyr::left_join(x = accel, y = bmx_c)
              } else if (input$waves == 2) {
                #data(bmx_d)
                names(bmx_d) <- tolower(names(bmx_d))
                accel <<- dplyr::left_join(x = accel, y = bmx_d)
              } else if (input$waves == 3) {
                bmx <- dplyr::full_join(bmx_c, bmx_d)
                names(bmx) <- tolower(names(bmx))
                accel <<- dplyr::left_join(accel, bmx)
              }

            }

            if ("bpx" %in% input$datasets) {

              if (input$waves == 1) {
                #data(bpx_c)
                names(bpx_c) <- tolower(names(bpx_c))
                accel <<- dplyr::left_join(x = accel, y = bpx_c)
              } else if (input$waves == 2) {
                #data(bpx_d)
                names(bpx_d) <- tolower(names(bpx_d))
                accel <<- dplyr::left_join(x = accel, y = bpx_d)
              } else if (input$waves == 3) {
                bpx <- dplyr::full_join(bpx_c, bpx_d)
                names(bpx) <- tolower(names(bpx))
                accel <<- dplyr::left_join(accel, bpx)
              }

            }

          }

          removeModal()

        }
      )

      # When 'Download' buttons get clicked
      output$download_csv <- downloadHandler(
        filename = paste("nhanes-", Sys.Date(), ".csv", sep = ""),
        content = function(file) {
          write.csv(accel, file, row.names = FALSE)
        }
      )
      output$download_rda <- downloadHandler(
        filename = paste("nhanes-", Sys.Date(), ".rda", sep = ""),
        content = function(file) {
          save(accel, file = file)
        }
      )
      output$download_dta <- downloadHandler(
        filename = paste("nhanes-", Sys.Date(), ".dta", sep = ""),
        content = function(file) {
          write_dta(accel, path = file)
        }
      )
      output$download_sas7bdat <- downloadHandler(
        filename = paste("nhanes-", Sys.Date(), ".sas7bdat", sep = ""),
        content = function(file) {
          write_sas(accel, path = file)
        }
      )
      output$download_sav <- downloadHandler(
        filename = paste("nhanes-", Sys.Date(), ".sav", sep = ""),
        content = function(file) {
          write_sav(accel, path = file)
        }
      )

    }

  )

}

