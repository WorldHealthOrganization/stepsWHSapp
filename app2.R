
library(here)
library(shiny)
library(shinyjs)
library(tidyverse)

source("WHSdepression.R")

# create a vector of questions DA1-23 
da.questions <- 
  c("DA1. Have you ever been told by a doctor or other health worker that you have depression?",
    "DA5. Have you been taking any medications or other treatment, like counseling or psychotherapy, either alone or in group, for depression in the last 2 weeks?",
    "DA6. During the last 12 months, have you had a period lasting several days when you felt sad, empty or depressed?",
    "DA7. During the last 12 months, have you had a period lasting several days when you lost interest in most things you usually enjoy such as personal relationships, work or hobbies/recreation?",
    "DA8. During the last 12 months, have you had a period lasting several days when you have been feeling your energy decreased or that you are tired all the time?",
    "DA9. Was this period of sadness, loss of interest or low energy for more than 2 weeks?",
    "DA10. Was this period of sadness, loss of interest or low energy most of the day, nearly every day?",
    "DA11. During this period, did you lose your appetite?",
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

required_vec <- c("da1","da5","da6","da7","da8","da9",
                  "da10","da11","da12","da13","da14","da15","da16","da17",
                  "da18","da19","da20","da21","da22","da23")

# create a vector of answer choices for DA1, DA4-23
answer.choices <- c("Yes","No","Don't know","Refuse")




ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  title = 'STEPS WHS Method Self-assessment Tool',
  
  # create some select inputs
  lapply(1:20, function(i) {
    radioButtons(paste0('da', i), paste0(da.questions[i]),
                choices = answer.choices, selected = character(0), inline = T)
  }),

  verbatimTextOutput('da_out'),
  actionButton(inputId = "submit", label = "submit")
  
)


server <- function(input, output, session) {
  
  toggle("da5")
  observeEvent(input$da1, {
    toggle("da5", condition = input$da1==answer.choices[1]) # if you want to alternate between hiding and showing
  })
  
  # note we use the syntax input[['foo']] instead of input$foo, because we have
  # to construct the id as a character string, then use it to access the value;
  # same thing applies to the output object below
  output$da_out <- renderPrint({
    res <<- lapply(1:20, function(i) input[[paste0('da', i)]])
    str(setNames(res, paste0('da', 1:20)))
  })
  
  
  
  observeEvent(
    input$submit,{
      # write a matrix called responses
      responses <<- matrix(ncol = length(required_vec), nrow=0)
      # add column names from vector
      colnames(responses) <<- required_vec
      
      # list values from table
      listed_responses <<- lapply(1:20, function(i) input[[paste0('da', i)]])
      
      # function for potential NULL values in the list, converting them to NA
      # when there are skipped questions
      nullToNA <- function(x) {
        x[sapply(x, is.null)] <- NA
        return(x)
      }
      listed_responses <<- nullToNA(listed_responses)
      
      # create one row matrix with unlisted responses
      unlisted_responses <<- t(as.matrix(unlist(listed_responses)))
      #unlisted_responses <<- as.data.frame(unlist(listed_responses))
      
      # combine it with preexisting df responses
      responses <<- rbind(responses, unlisted_responses)
      
      df_flat <<- as.data.frame(responses)
      colnames(df_flat) <<- required_vec
      
      # recode character values to numbers for using with WHS function
      # base R approach:
      # nm1 <<- setNames(c(1,2,77,88), answer.choices)
      # df_flat[] <<- lapply(df_flat, function(x) nm1[x])
      # OR dplyr aproach:
      df_flat <<- df_flat %>%
        mutate(across(everything(), ~dplyr::recode(., 'Yes'= 1, 'No'= 2, "Don't know" = 77, 'Refuse'= 88)))
      
      depression_results <<- whs_depression(df_flat) %>% mutate(across(everything(), as.character))
      
      showModal(modalDialog(
        title = "Completed! Your result:",
        paste(if(is.na(depression_results$depression) || depression_results$depression==0){ paste("No, you don't have symptoms of depression.") }
              else if(depression_results$depression==1){ paste("Yes, you have symptoms of depression. It is advisable to consult with your family doctor or general practitioner for a thorough exam and screening.") }
              else { paste("No, you don't have symptoms of depression.") }
        )
      ))
      # write.table(responses,sep = ",", file = "responses.csv", append= TRUE, quote= FALSE)
    }
  )
  
  
}


shinyApp(ui, server)
