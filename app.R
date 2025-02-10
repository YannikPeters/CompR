library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(topicmodels)
library(syuzhet)

ui <- fluidPage(
  titlePanel("Social Media Data Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload First Dataset (CSV)", accept = c(".csv")),
      fileInput("file2", "Upload Second Dataset (CSV)", accept = c(".csv")),
      actionButton("compare", "Compare Datasets"),
      hr(),
      h4("Summary"),
      textOutput("removed_count")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("wordcloud")),
        tabPanel("Topic Modeling", plotOutput("topics")),
        tabPanel("Sentiment Analysis", plotOutput("sentiment_plot"))
      )
    )
  )
)

server <- function(input, output) {
  data1 <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  data2 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })
  
  removed_posts <- eventReactive(input$compare, {
    df1 <- data1()
    df2 <- data2()
    
    # Annahme: Die ID-Spalte heißt "id"
    removed <- df1 %>% filter(!(id %in% df2$id))
    
    return(removed)
  })
  
  output$removed_count <- renderText({
    req(removed_posts())
    paste("Number of removed posts:", nrow(removed_posts()))
  })
  
  output$wordcloud <- renderPlot({
    req(removed_posts())
    
    text_data <- removed_posts()$text  # Annahme: Die Spalte mit dem Text heißt "text"
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    
    wordcloud(corpus, max.words = 100, random.order = FALSE)
  })
  
  output$topics <- renderPlot({
    req(removed_posts())
    
    text_data <- removed_posts()$text
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    dtm <- DocumentTermMatrix(corpus)
    
    lda_model <- LDA(dtm, k = 3, control = list(seed = 123))
    topics <- terms(lda_model, 5)
    barplot(sort(rowSums(as.matrix(dtm)), decreasing = TRUE)[1:10], las = 2, col = "lightblue")
  })
  
  output$sentiment_plot <- renderPlot({
    req(removed_posts())
    
    sentiment_scores <- get_sentiment(removed_posts()$text, method = "bing")
    ggplot(sentiment_scores, aes(sentiment)) + 
      geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
      labs(title = "Sentiment Distribution", x = "Sentiment Score", y = "Frequency")
  })
}

shinyApp(ui, server)

