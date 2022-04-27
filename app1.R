
library(here)
library(shiny)

# create a vector of questions DA1-23 
da.questions <- 
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

# create a vector of answer choices for DA1, DA4-23
answer.choices <- c("Yes","No","Don't know","Refuse")

da.question.numbers <- c("da1","da2","da3","da4","da5","da6","da7","da8","da9",
                         "da10","da11","da12","da13","da14","da15","da16","da17",
                         "da18","da19","da20","da21","da22","da23")
# required questions
required_vec <- c("da1","da5","da6","da7","da8","da9",
               "da10","da11","da12","da13","da14","da15","da16","da17",
               "da18","da19","da20","da21","da22","da23")

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  tags$h3("STEPS WHS Method Self-assessment Tool"),
  radioButtons("da1", da.questions[1], answer.choices, selected = character(0), inline=TRUE),
  # hide conditioned questions on start
  hidden(radioButtons("da5", da.questions[5], answer.choices, selected = character(0), inline=TRUE)),
  radioButtons("da6", da.questions[6], answer.choices, selected = character(0), inline=TRUE),
  radioButtons(da.question.numbers[7], da.questions[7], answer.choices, selected = character(0), inline=TRUE),
  radioButtons(da.question.numbers[8], da.questions[8], answer.choices, selected = character(0), inline=TRUE),
  hidden(radioButtons(da.question.numbers[9], da.questions[9], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[10], da.questions[10], answer.choices, selected = character(0), inline=TRUE)),
  br(),
  hidden(p(id = "da11_da23_intro", "During this period:")),
  hidden(radioButtons(da.question.numbers[11], da.questions[11], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[12], da.questions[12], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[13], da.questions[13], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[14], da.questions[14], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[15], da.questions[15], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[16], da.questions[16], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[17], da.questions[17], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[18], da.questions[18], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[19], da.questions[19], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[20], da.questions[20], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[21], da.questions[21], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[22], da.questions[22], answer.choices, selected = character(0), inline=TRUE)),
  hidden(radioButtons(da.question.numbers[23], da.questions[23], answer.choices, selected = character(0), inline=TRUE)),
  tableOutput("responsesTableStatic"),
  #dataTableOutput("responsesTableDynamic"),
  verbatimTextOutput('da_out'),
  actionButton(inputId = "submit", label = "submit")
  #actionButton("submit", "Submit", class = "btn-primary")
  
)



server <- function(input, output, session) {
  
  # note we use the syntax input[['foo']] instead of input$foo, because we have
  # to construct the id as a character string, then use it to access the value;
  # same thing applies to the output object below
  output$da_out <- renderPrint({
    res <<- lapply(1:20, function(i) input[[paste0('da', i)]])
    str(setNames(res, paste0('da', 1:20)))
  })
  
  
  # show (toggle) questions per relevant answer choice
  observeEvent(input$da1, {
    toggle("da5", condition = input$da1==answer.choices[1]) # if you want to alternate between hiding and showing
  })
  observeEvent(input$da6, {
    toggle("da9", condition = input$da6==answer.choices[1])
    toggle("da10", condition = input$da6==answer.choices[1])
    toggle("da11_da23_intro", condition = input$da6==answer.choices[1])
    toggle("da11", condition = input$da6==answer.choices[1])
    toggle("da12", condition = input$da6==answer.choices[1])
    toggle("da13", condition = input$da6==answer.choices[1])
    toggle("da14", condition = input$da6==answer.choices[1])
    toggle("da15", condition = input$da6==answer.choices[1])
    toggle("da16", condition = input$da6==answer.choices[1])
    toggle("da17", condition = input$da6==answer.choices[1])
    toggle("da18", condition = input$da6==answer.choices[1])
    toggle("da19", condition = input$da6==answer.choices[1])
    toggle("da20", condition = input$da6==answer.choices[1])
    toggle("da21", condition = input$da6==answer.choices[1])
    toggle("da22", condition = input$da6==answer.choices[1])
    toggle("da23", condition = input$da6==answer.choices[1])
  })
  observeEvent(input$da7, {
    toggle("da9", condition = input$da7==answer.choices[1])
    toggle("da10", condition = input$da7==answer.choices[1])
    toggle("da11_da23_intro", condition = input$da7==answer.choices[1])
    toggle("da11", condition = input$da7==answer.choices[1])
    toggle("da12", condition = input$da7==answer.choices[1])
    toggle("da13", condition = input$da7==answer.choices[1])
    toggle("da14", condition = input$da7==answer.choices[1])
    toggle("da15", condition = input$da7==answer.choices[1])
    toggle("da16", condition = input$da7==answer.choices[1])
    toggle("da17", condition = input$da7==answer.choices[1])
    toggle("da18", condition = input$da7==answer.choices[1])
    toggle("da19", condition = input$da7==answer.choices[1])
    toggle("da20", condition = input$da7==answer.choices[1])
    toggle("da21", condition = input$da7==answer.choices[1])
    toggle("da22", condition = input$da7==answer.choices[1])
    toggle("da23", condition = input$da7==answer.choices[1])
  })
  observeEvent(input$da8, {
    toggle("da9", condition = input$da8==answer.choices[1])
    toggle("da10", condition = input$da8==answer.choices[1])
    toggle("da11_da23_intro", condition = input$da8==answer.choices[1])
    toggle("da11", condition = input$da8==answer.choices[1])
    toggle("da12", condition = input$da8==answer.choices[1])
    toggle("da13", condition = input$da8==answer.choices[1])
    toggle("da14", condition = input$da8==answer.choices[1])
    toggle("da15", condition = input$da8==answer.choices[1])
    toggle("da16", condition = input$da8==answer.choices[1])
    toggle("da17", condition = input$da8==answer.choices[1])
    toggle("da18", condition = input$da8==answer.choices[1])
    toggle("da19", condition = input$da8==answer.choices[1])
    toggle("da20", condition = input$da8==answer.choices[1])
    toggle("da21", condition = input$da8==answer.choices[1])
    toggle("da22", condition = input$da8==answer.choices[1])
    toggle("da23", condition = input$da8==answer.choices[1])
  })
  
  
  observeEvent(
    input$submit,{
      # write a matrix called responses
      responses <<- matrix(ncol= length(required_vec), nrow=0)
      colnames(responses) <<- required_vec
      
      # unlist values from json table
      listed_responses <<- lapply(1:20, function(i) input[[paste0('da', i)]])
      
      
      # create one row matrix with unlisted responses
      unlisted_responses <<- t(as.matrix(unlist(listed_responses)))
      
      # combine it with preexiating df responses
      responses <<- rbind(responses, unlisted_responses)
      
      df_flat <<- as.data.frame(responses)
      
      nm1 <<- setNames(c(1,2,77,88), answer.choices)
      df_flat[] <<- lapply(df_flat, function(x) nm1[x])
      
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


