# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(topicmodels)
library(sentimentr)

# Define UI (User Interface)
ui <- fluidPage(
  titlePanel("CompR"),  # Main Title
  
  # Subheadline and Introduction Text
  h3("Social Media Data Comparison"),
  p("This app allows users to compare two social media datasets collected at different times. 
     It identifies removed posts based on their IDs and analyzes them using NLP techniques, 
     including word clouds, topic modeling, and sentiment analysis."),
  
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

# Define Server Logic
server <- function(input, output) {
  
  # Load the first dataset
  data1 <- reactive({
    req(input$file1)  # Ensure the file is uploaded
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  # Load the second dataset
  data2 <- reactive({
    req(input$file2)  # Ensure the file is uploaded
    read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })
  
  # Function to detect the correct ID column
  detect_id_column <- function(df) {
    possible_ids <- c("id", "tweet_id", "comment_id")
    found_id <- intersect(possible_ids, names(df))
    
    if (length(found_id) > 0) {
      return(found_id[1])  # Use the first matching column
    } else {
      return(NULL)
    }
  }
  
  # Identify removed posts (present in the first dataset but missing in the second)
  removed_posts <- eventReactive(input$compare, {
    df1 <- data1()
    df2 <- data2()
    
    id_col_1 <- detect_id_column(df1)
    id_col_2 <- detect_id_column(df2)
    
    # Ensure both datasets contain at least one valid ID column
    if (is.null(id_col_1) || is.null(id_col_2)) {
      showNotification("Error: No valid ID column ('id', 'tweet_id', or 'comment_id') found in one or both datasets.", type = "error")
      return(NULL)
    }
    
    # Filter posts that are missing in the second dataset
    removed <- df1 %>% filter(!(!!sym(id_col_1) %in% df2[[id_col_2]]))
    
    return(removed)
  })
  
  # Display the number of removed posts
  output$removed_count <- renderText({
    req(removed_posts())
    paste("Number of removed posts:", nrow(removed_posts()))
  })
  
  # Generate a Word Cloud of removed posts
  output$wordcloud <- renderPlot({
    req(removed_posts())
    
    text_data <- removed_posts()$text  # Assumption: The text column is named "text"
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in removed posts.", type = "error")
      return(NULL)
    }
    
    # Create a text corpus and clean the text
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    
    # Generate the word cloud
    wordcloud(corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  })
  
  # Perform Topic Modeling on removed posts with dynamic topic selection
  output$topics <- renderPlot({
    req(removed_posts())
    
    text_data <- removed_posts()$text
    num_docs <- length(text_data)  # Count the number of documents
    
    if (is.null(text_data) || num_docs == 0) {
      showNotification("Error: No text data available for topic modeling.", type = "error")
      return(NULL)
    }
    
    # Dynamic selection of the number of topics
    k <- ifelse(num_docs < 10, 1,
                ifelse(num_docs < 30, 2,
                       ifelse(num_docs < 100, 3, 5)))
    
    # Create a document-term matrix
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    dtm <- DocumentTermMatrix(corpus)
    
    # Check if there are enough documents for LDA
    if (nrow(dtm) < 5) {
      showNotification("Error: Not enough data for Topic Modeling.", type = "error")
      return(NULL)
    }
    
    # Apply Latent Dirichlet Allocation (LDA) for topic modeling
    lda_model <- LDA(dtm, k = k, control = list(seed = 123))
    topics <- terms(lda_model, 5)
    
    # Create a bar plot for the top words
    barplot(sort(rowSums(as.matrix(dtm)), decreasing = TRUE)[1:10], las = 2, col = "lightblue",
            main = paste("Top 10 Words (", k, "Topics)"))
  })
  
  # Perform Sentiment Analysis on removed posts using `sentimentr`
  output$sentiment_plot <- renderPlot({
    req(removed_posts())
    
    text_data <- removed_posts()$text
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data for sentiment analysis.", type = "error")
      return(NULL)
    }
    
    # Compute sentiment scores using sentimentr
    sentiment_scores <- sentiment(text_data)
    
    # Create a histogram of sentiment scores
    ggplot(sentiment_scores, aes(x = sentiment)) + 
      geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
      labs(title = "Sentiment Distribution", x = "Sentiment Score", y = "Frequency") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui, server)

