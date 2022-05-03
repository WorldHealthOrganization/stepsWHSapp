# NOTE: this version is currently used as a stand-alone package in R folder

# load packages
library(here)
library(shiny)
library(shinyjs)
library(dplyr)
library(readr)

# load WHS function
source("WHSdepression.R")

# create a vector of questions DA1-23 
da_questions <- 
  c("DA1. Have you ever been told by a doctor or other health worker that you have depression?",
    "DA2. When were you diagnosed?",
    "DA3. How many years passed since your doctor or health care provider told you for the first time that you have depression?",
    "DA4. Have you been taking any medications or other treatment, like counseling or psychotherapy, either alone or in group, for depression in the last 12 months?",
    "DA5. Have you been taking any medications or other treatment, like counseling or psychotherapy, either alone or in group, for depression in the last 2 weeks?",
    "DA6. During the last 12 months, have you had a period lasting several days when you felt sad, empty or depressed?",
    "DA7. During the last 12 months, have you had a period lasting several days when you lost interest in most things you usually enjoy such as personal relationships, work or hobbies/recreation?",
    "DA8. During the last 12 months, have you had a period lasting several days when you have been feeling your energy decreased or that you are tired all the time?",
    "DA9. Was this period of sadness, loss of interest or low energy for more than 2 weeks?",
    "DA10. Was this period of sadness, loss of interest or low energy most of the day, nearly every day?",
    "DA11. Did you lose your appetite?",
    "DA12. Did you notice any slowing down in your thinking?",
    "DA13. Did you notice any problems falling asleep?",
    "DA14. Did you notice any problems waking up too early?",
    "DA15. Did you have any difficulties concentrating; for example, listening to others, working, watching TV, listening to the radio?",
    "DA16. Did you notice any slowing down in your moving around?",
    "DA17. Did you feel anxious and worried most days?",
    "DA18. Were you so restless or jittery nearly every day that you paced up and down and couldn’t sit still?",
    "DA19. Did you feel negative about yourself or like you had lost confidence?",
    "DA20. Did you frequently feel hopeless - that there was no way to improve things?",
    "DA21. Did your interest in sex decrease?",
    "DA22. Did you think of death, or wish you were dead?",
    "DA23. During this period, did you ever try to end your life?")

# vector with only question codes
da_codes <- c("da1","da2","da3","da4","da5","da6","da7","da8","da9",
              "da10","da11","da12","da13","da14","da15","da16","da17",
              "da18","da19","da20","da21","da22","da23")

# vector with required questions for WHS depression function
required_vec <- c("da1","da5","da6","da7","da8","da9","da10","da11",
                  "da12","da13","da14","da15","da16","da17",
                  "da18","da19","da20","da21","da22","da23")

# vector of answer choices for DA1, DA4-23
answer_choices <- c("Yes","No","Don't know","Refuse")

################################################################################

ui <- fluidPage(
  
  # set up shinyjs
  useShinyjs(),
  # main title
  #title = 'WHO STEPS Depression Self-assessment Tool',
  tags$h3("WHO STEPS Depression Self-assessment Tool"),
  # create select inputs (radio buttons) for 20 questions out of 23 in the module
  radioButtons("da1", da_questions[1], answer_choices, selected = character(0), inline=TRUE),
  # hide conditioned questions on start
  hidden(radioButtons("da5", da_questions[5], answer_choices, selected = character(0), inline=TRUE)),
  radioButtons("da6", da_questions[6], answer_choices, selected = character(0), inline=TRUE),
  radioButtons("da7", da_questions[7], answer_choices, selected = character(0), inline=TRUE),
  radioButtons("da8", da_questions[8], answer_choices, selected = character(0), inline=TRUE),
  hidden(radioButtons("da9", da_questions[9], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da10", da_questions[10], answer_choices, selected = character(0), inline=TRUE)),
  br(),
  hidden(p(id = "da11_da23_intro", "During this period:")),
  hidden(radioButtons("da11", da_questions[11], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da12", da_questions[12], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da13", da_questions[13], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da14", da_questions[14], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da15", da_questions[15], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da16", da_questions[16], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da17", da_questions[17], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da18", da_questions[18], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da19", da_questions[19], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da20", da_questions[20], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da21", da_questions[21], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da22", da_questions[22], answer_choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons("da23", da_questions[23], answer_choices, selected = character(0), inline=TRUE)),
  # submit button for survey
  actionButton(inputId = "submit", label = "submit")
  
)

################################################################################

server <- function(input, output, session) {
  
  # disable submit button on start-up
  disable("submit")
  
  # show or hide (toggle) questions per relevant answer choice
  # alternate between hiding and showing
  observeEvent(input$da1, {
    toggleElement("da5", condition = input$da1==answer_choices[1])
  })
  
  observeEvent(ignoreInit = TRUE, c(input$da6, input$da7, input$da8), {

    if ((!is.null(input$da6) && input$da6 == "Yes") ||
        (!is.null(input$da7) && input$da7 == "Yes") ||
        (!is.null(input$da8) && input$da8 == "Yes")) {
      showElement("da9")
      showElement("da10")
      showElement("da11_da23_intro")
      showElement("da11")
      showElement("da12")
      showElement("da13")
      showElement("da14")
      showElement("da15")
      showElement("da16")
      showElement("da17")
      showElement("da18")
      showElement("da19")
      showElement("da20")
      showElement("da21")
      showElement("da22")
      showElement("da23")
    }
    else if ((!is.null(input$da6) && input$da6 != "Yes") ||
             (!is.null(input$da7) && input$da7 != "Yes") ||
             (!is.null(input$da8) && input$da8 != "Yes")) {
      hideElement("da9")
      hideElement("da10")
      hideElement("da11_da23_intro")
      hideElement("da11")
      hideElement("da12")
      hideElement("da13")
      hideElement("da14")
      hideElement("da15")
      hideElement("da16")
      hideElement("da17")
      hideElement("da18")
      hideElement("da19")
      hideElement("da20")
      hideElement("da21")
      hideElement("da22")
      hideElement("da23")
    }

  })
  
  # what happens on clicking submit
  observeEvent(
    input$submit, {
 
      # collect values/answers as a list 
      listed_responses <<- lapply(1:23, function(i) input[[paste0('da', i)]])
      
      # function for potential NULL values in the list, converting them to NA
      # when there are skipped questions
      nullToNA <- function(x) {
        x[sapply(x, is.null)] <- NA
        return(x)
      }
      listed_responses <<- nullToNA(listed_responses)
      
      # create one row df with unlisted responses (first unlist as matrix, transpose & set column names)
      unlisted_responses <<- setNames(as.data.frame(t(as.matrix(unlist(listed_responses)))), da_codes)
      
      # recode character values to numbers for using with WHS function
      unlisted_responses <<- unlisted_responses %>%
        mutate(across(everything(), ~dplyr::recode(., 'Yes'= 1, 'No'= 2, "Don't know" = 77, 'Refuse'= 88)))
      
      # produce depression results
      depression_results <<- whs_depression(unlisted_responses) %>% mutate(across(everything(), as.character))
      
      # show a modal pop-up with result message
      showModal(modalDialog(
        title = "Completed! Your result:",
        paste(if(is.na(depression_results$depression) || depression_results$depression==0){ paste("No, you don't have symptoms of depression.") }
              else if(depression_results$depression==1){ paste("Yes, you have symptoms of depression. It is advisable to consult with your family doctor or general practitioner for a thorough exam and screening.") }
              else { paste("No, you don't have symptoms of depression.") }
        )
      ))
      
      # save response (if needed)
      write_csv(depression_results, "depression_results.csv", append = TRUE)
    })
  
  # activate submit button only when radios are selected (observe all conditions)
  observeEvent(
    ignoreInit = TRUE, 
    c(input$da1, input$da5, input$da6, input$da7, input$da8,
      input$da9, input$da10, input$da11, input$da12, input$da13,
      input$da14, input$da15, input$da16, input$da17, input$da18,
      input$da19, input$da20, input$da21, input$da22, input$da23), {
        
        # 1
        if ((!is.null(input$da1) && input$da1 == "Yes") &&
            (!is.null(input$da5) && input$da5 != "") && # must NOT be empty (subquestion)
            (!is.null(input$da6) && input$da6 != "Yes") &&
            (!is.null(input$da7) && input$da7 != "Yes") &&
            (!is.null(input$da8) && input$da8 != "Yes")) {
          # da9-da23 must BE empty
          is.null(input$da9)
          is.null(input$da10)
          is.null(input$da11) 
          is.null(input$da12)
          is.null(input$da13)
          is.null(input$da14)
          is.null(input$da15)
          is.null(input$da16)
          is.null(input$da17)
          is.null(input$da18)
          is.null(input$da19)
          is.null(input$da20)
          is.null(input$da21)
          is.null(input$da22)
          is.null(input$da23)
          updateRadioButtons(session = session, inputId = "da9", selected = character(0))
          updateRadioButtons(session = session, inputId = "da10", selected = character(0))
          updateRadioButtons(session = session, inputId = "da11", selected = character(0))
          updateRadioButtons(session = session, inputId = "da12", selected = character(0))
          updateRadioButtons(session = session, inputId = "da13", selected = character(0))
          updateRadioButtons(session = session, inputId = "da14", selected = character(0))
          updateRadioButtons(session = session, inputId = "da15", selected = character(0))
          updateRadioButtons(session = session, inputId = "da16", selected = character(0))
          updateRadioButtons(session = session, inputId = "da17", selected = character(0))
          updateRadioButtons(session = session, inputId = "da18", selected = character(0))
          updateRadioButtons(session = session, inputId = "da19", selected = character(0))
          updateRadioButtons(session = session, inputId = "da20", selected = character(0))
          updateRadioButtons(session = session, inputId = "da21", selected = character(0))
          updateRadioButtons(session = session, inputId = "da22", selected = character(0))
          updateRadioButtons(session = session, inputId = "da23", selected = character(0))
          enable("submit")
        } 
        # 2
        else if ((!is.null(input$da1) && input$da1 != "Yes") &&
                (!is.null(input$da6) && input$da6 != "Yes") &&
                (!is.null(input$da7) && input$da7 != "Yes") &&
                (!is.null(input$da8) && input$da8 != "Yes")) {
          # da5 must BE empty
          is.null(input$da5)
          # da9-da23 must BE empty
          is.null(input$da9)
          is.null(input$da10)
          is.null(input$da11) 
          is.null(input$da12)
          is.null(input$da13)
          is.null(input$da14)
          is.null(input$da15)
          is.null(input$da16)
          is.null(input$da17)
          is.null(input$da18)
          is.null(input$da19)
          is.null(input$da20)
          is.null(input$da21)
          is.null(input$da22)
          is.null(input$da23)
          updateRadioButtons(session = session, inputId = "da5", selected = character(0))
          updateRadioButtons(session = session, inputId = "da9", selected = character(0))
          updateRadioButtons(session = session, inputId = "da10", selected = character(0))
          updateRadioButtons(session = session, inputId = "da11", selected = character(0))
          updateRadioButtons(session = session, inputId = "da12", selected = character(0))
          updateRadioButtons(session = session, inputId = "da13", selected = character(0))
          updateRadioButtons(session = session, inputId = "da14", selected = character(0))
          updateRadioButtons(session = session, inputId = "da15", selected = character(0))
          updateRadioButtons(session = session, inputId = "da16", selected = character(0))
          updateRadioButtons(session = session, inputId = "da17", selected = character(0))
          updateRadioButtons(session = session, inputId = "da18", selected = character(0))
          updateRadioButtons(session = session, inputId = "da19", selected = character(0))
          updateRadioButtons(session = session, inputId = "da20", selected = character(0))
          updateRadioButtons(session = session, inputId = "da21", selected = character(0))
          updateRadioButtons(session = session, inputId = "da22", selected = character(0))
          updateRadioButtons(session = session, inputId = "da23", selected = character(0))
          enable("submit")
        } 
        # 3
        else if ((!is.null(input$da1) && input$da1 == "Yes") &&
                (!is.null(input$da5) && input$da5 != "") && # must NOT be empty
                ((!is.null(input$da6) && input$da6 == "Yes") ||
                  (!is.null(input$da7) && input$da7 == "Yes") ||
                  (!is.null(input$da8) && input$da8 == "Yes")) &&
                # then the rest of da9 to da23
                # da9-da23 must NOT be empty
                (!is.null(input$da9)) && 
                (!is.null(input$da10)) && (!is.null(input$da11)) && (!is.null(input$da12)) &&
                (!is.null(input$da13)) && (!is.null(input$da14)) && (!is.null(input$da15)) &&
                (!is.null(input$da16)) && (!is.null(input$da17)) && (!is.null(input$da18)) &&
                (!is.null(input$da19)) && (!is.null(input$da20)) && (!is.null(input$da21)) &&
                (!is.null(input$da22)) && (!is.null(input$da23))) {
          enable("submit")
        } 
        # 4
        else if ((!is.null(input$da1) && input$da1 != "Yes") &&
                ((!is.null(input$da6) && input$da6 == "Yes") ||
                  (!is.null(input$da7) && input$da7 == "Yes") ||
                  (!is.null(input$da8) && input$da8 == "Yes")) &&
                # then the rest of da9 to da23
                # da9-da23 must NOT be empty
                (!is.null(input$da9)) && 
                (!is.null(input$da10)) && (!is.null(input$da11)) && (!is.null(input$da12)) &&
                (!is.null(input$da13)) && (!is.null(input$da14)) && (!is.null(input$da15)) &&
                (!is.null(input$da16)) && (!is.null(input$da17)) && (!is.null(input$da18)) &&
                (!is.null(input$da19)) && (!is.null(input$da20)) && (!is.null(input$da21)) &&
                (!is.null(input$da22)) && (!is.null(input$da23))) {
          # da5 must BE empty
          is.null(input$da5)
          updateRadioButtons(session = session, inputId = "da5", selected = character(0))
          enable("submit")
        } else { 
          disable("submit") 
        }

      }
    )
  
}

################################################################################

shinyApp(ui, server)


