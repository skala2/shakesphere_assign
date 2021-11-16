library(shiny)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(tibble)
library(dplyr)
library(shinythemes)
library(ggplot2)

#The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, StopWords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(StopWords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}
# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(position="left",
                 selectInput("BookName","Choose a book:",books),
                 checkboxInput("StopWords","Stop Words",value=TRUE),
                 actionButton("RunButton","Run"),
                 hr(),
                 h3("Word Cloud Settings"),
                 sliderInput("MaxWords","Max # of Words",min=10,max=200,value=100,step=10),
                 sliderInput("LargeWords","Large Words length",min=1,max=8,value=4),
                 sliderInput("SmallWords","Small Words Length",min=0.1, max=4, value=0.5),
                 hr(),
                 h3("Word Count Settings"),
                 sliderInput("MinWordCount","Minimum Words",min = 10,max = 100,value = 25),
                 sliderInput("FontSize","Font Size",min = 8,max = 30,value = 14)),
    mainPanel(position="right",
              tabsetPanel(
                tabPanel("Word Cloud",
                         plotOutput("cloud",
                                    width = "80%",
                                    height = "600px")),
                tabPanel("Word Counts",
                         plotOutput("freq",
                                    width = "80%",
                                    height = "600px"))
              ))
  )
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$RunButton,{
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$BookName,input$StopWords)
    })
    
  })
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$LargeWords, input$SmallWords),
          random.order = FALSE, 
          max.words = input$MaxWords, 
          colors=pal))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    v %>% filter(n > input$MinWordCount) %>% ggplot(aes(reorder(word,n),n)) + 
      geom_col() +
      coord_flip() +
      theme(text=element_text(size=input$FontSize)) +
      xlab('') +
      ylab('')
  })
}

shinyApp(ui = ui, server = server)
