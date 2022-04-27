
# Load packages
library(here)
library(shiny)
library(shinysurveys)
library(tidyverse)

# View(teaching_r_questions)

source("WHSdepression.R")

# create a vector of questions DA1,DA5-23, repeated three times each 
da_questions <- 
  rep(c("DA1. Have you ever been told by a doctor or other health worker that you have depression?",
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
        "DA23. During this period, did you ever try to end your life?"), each = 4)

# testing part
# remove question text after "."
# da_questions <- gsub("\\..*","",da_questions)

# create a vector of answer choices for DA1,DA5-23
answer_choices <- c("Yes","No","Don't know","Refuse")

# create a vector of id values for each question, repeated three times
da_codes <- rep(c("da1","da5","da6","da7","da8","da9","da10","da11",
                  "da12","da13","da14","da15","da16","da17","da18",
                  "da19","da20","da21","da22","da23"), each = 4)


# Define questions in the format of a shinysurvey
survey_questions <- data.frame(
  question = da_questions,
  option = answer_choices,
  input_type = "mc",
  input_id = da_codes,
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)

# adding dependences 
survey_questions <- survey_questions %>% 
  # da5 is shown when da1=1
  mutate(dependence = ifelse(input_id=="da5", "da1", dependence),
         dependence_value = ifelse(input_id=="da5", "Yes", dependence_value)) %>% 
  # da9-23 are shown when da6=1 or da7=1 or da8=1
  mutate(dependence = ifelse(input_id %in% c("da9","da10","da11","da12","da13",
                                             "da14","da15","da16","da17","da18",
                                             "da19","da20","da21","da22","da23"), "da6", dependence),
         dependence_value = ifelse(input_id %in% c("da9","da10","da11","da12","da13",
                                                   "da14","da15","da16","da17","da18",
                                                   "da19","da20","da21","da22","da23"), "Yes", dependence_value))


# what is needed
# survey_questions_needed <- survey_questions %>% 
#   mutate(dependence = ifelse(input_id=="da5", "da1", dependence),
#          dependence_value = ifelse(input_id=="da5", "Yes", dependence_value)) %>% 
#   mutate(dependence = ifelse(input_id %in% c("da9","da10","da11","da12","da13",
#                                              "da14","da15","da16","da17","da18",
#                                              "da19","da20","da21","da22","da23"), "da6;da7;da8", dependence),
#          dependence_value = ifelse(input_id %in% c("da9","da10","da11","da12","da13",
#                                                    "da14","da15","da16","da17","da18",
#                                                    "da19","da20","da21","da22","da23"), "Yes", dependence_value))


write_csv(survey_questions, file = "surveyfile.csv")
#write_csv(survey_questions_needed, file = "surveyfiletest.csv")
survey_questions <- read_csv(file = "surveyfile.csv")



# Define shiny UI
ui <- fluidPage(
  surveyOutput(df = survey_questions,
               survey_title = "STEPS WHS Method Self-assessment Tool",
               survey_description = "The following tool allows you to assess your potential state of depression. 
               It contains questions that ask about thoughts, moods and feelings. 
               Please answer the questions even if you do not talk often about these issues.")
)

# Define shiny server
server <- function(input, output, session) {
  
  renderSurvey()
  
  observeEvent(input$submit, {
    
    response_data <<- getSurveyData() 
    print(response_data)
    
    df_flat <<- response_data %>% 
      # prepare values for whs_depression function by recoding values in response variable
      mutate(response = replace(response, response=="Yes", 1)) %>%
      mutate(response = replace(response, response=="No", 2)) %>%
      mutate(response = replace(response, response=="Don't know", 77)) %>%
      mutate(response = replace(response, response=="Refuse", 88)) %>%
      mutate(response = replace(response, response=="HIDDEN-QUESTION", NA)) %>%
      # rotate data frame to wider from longer format
      pivot_wider(names_from = "question_id", values_from = "response") 
  
    
    depression_results <<- whs_depression(df_flat) %>% mutate(across(everything(), as.character))
    
    # if(nrow(depression_results)==0){ 
    #   depression_results <<- depression_results %>% 
    #     add_row(c=NA) %>% mutate(across(everything(), as.character))
    # }
    
    #print(depression_results)
    
    # showModal(modalDialog(
    #   title = "Completed! ",
    #   paste("Your result is:", 
    #         if(depression_results$c=="Yes"){ paste("Yes, you have symptoms of depression. It is advisable to consult with your family doctor or general practitioner for a thorough exam and screening.") }
    #         else if(depression_results$c=="No"){ paste("No, you don't have symptoms of depression.") }
    #         )
    # ))
    
    # 2nd version if result message
    showModal(modalDialog(
      title = "Completed! Your result:",
      paste(if(is.na(depression_results$depression) || depression_results$depression==0){ paste("No, you don't have symptoms of depression.") }
            else if(depression_results$depression==1){ paste("Yes, you have symptoms of depression. It is advisable to consult with your family doctor or general practitioner for a thorough exam and screening.") }
            else { paste("No, you don't have symptoms of depression.") }
            )
    ))
    
  })
}

# Run the shiny application
shinyApp(ui, server)



