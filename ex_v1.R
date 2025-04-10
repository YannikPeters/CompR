# ===== Load required libraries ===== #
library(shiny)
library(shinyjs) #For enhanced UI
library(dplyr)
library(tm)
library(topicmodels)
library(sentimentr)
library(wordcloud2)
library(highcharter)
library(tidytext)
library(reshape2)
library(ggplot2)
library(DT)
library(stringdist)
library(shinyBS) # For tooltips
library(shiny) # For icon()

# ====================================================== #
#                   COMPR - SHINY APP                    #
# ====================================================== #
# Architecture:                                          #
# - Frontend (UI): User Interface Components             #
# - Backend (Server): Data Processing & Business Logic   #
# - Data Processing Modules: Reusable Processing Units   #
# ====================================================== #

# ====================== #
#      FRONTEND (UI)     #
# ====================== #
# Responsibilities:                                      #
# - Layout organization                                  #
# - Input controls                                       #
# - Output display areas                                 #
# - User interaction elements                            #
# ====================================================== #


# ===== Define UI (User Interface) ===== #
ui <- fluidPage(
  useShinyjs(), 
  tags$head(
    tags$style(HTML("          #placeholder for custom CSS
      .form-group, .shiny-input-container {
        margin-bottom: 8px;
      }
      .form-group {
        padding-bottom: 0px;
      }
      .shiny-date-input {
        padding-bottom: 0px;
        margin-bottom: 0px;
      }
    ")),
    tags$style(HTML("
  .tooltip-inner {
    max-width: 300px;
    padding: 10px;
    background-color: #f8f9fa;
    color: #212529;
    border: 1px solid #dee2e6;
    border-radius: 4px;
  }
  .fa-info-circle {
    color: #007bff;
    margin-left: 5px;
    cursor: pointer;
  }
")),
    
    tags$script(HTML('
    $(document).ready(function() {
      // Initialize tooltips
      $("[data-toggle=\'tooltip\']").tooltip(); 
    });
  '))
  ),
  
  
  
  
  
  
  
  # ----- Main Layout ----- #
  
  # Main Title
  titlePanel("CompR"),  
  
  # Subheadline and Introduction Text
  h3("Social Media Data Comparison"),
  p("This app allows users to compare two social media datasets collected at different times. 
     It identifies removed posts based on their IDs and analyzes them using NLP techniques, 
     including word clouds, topic modeling, and sentiment analysis."),
  
  sidebarLayout(
    sidebarPanel(
      # Improved Dataset Selection Section
      h4("Dataset Comparison"),
      wellPanel(
        # Dataset 1
        fluidRow(
          column(6, fileInput("file1", "Dataset 1", accept = ".csv", width = "100%")),
          column(6, dateInput("date1", "Collection Date", value = Sys.Date()-30, width = "100%"))
        ),
        # Dataset 2
        fluidRow(
          column(6, fileInput("file2", "Dataset 2", accept = ".csv", width = "100%")),
          column(6, dateInput("date2", "Collection Date", value = Sys.Date(), width = "100%"))
        ),
        actionButton("compare", "Compare Datasets", 
                     class = "btn-primary", width = "100%")
      ),
      
      uiOutput("date_validation_msg"),
      
      
      
      hr(),
      h4("Summary"),
      textOutput("dataset1_count"),
      textOutput("dataset2_count"),
      textOutput("removed_count"),
      
      hr(),
      h4("Data Quality Indicators"),
      textOutput("completeness"),
      textOutput("data_loss"),
      textOutput("removed_per_day"),  # New line for daily removal rate
      textOutput("removal_rate_daily"),  
      
      conditionalPanel(
        condition = "input.tabset == 'Sentiment Analysis'",
        hr(),
        h4("Sentiment Comparison: Dataset 1 vs Dataset 2"),
        tableOutput("dataset_comparison_table") %>% 
          tagAppendAttributes(style = "border: 1px solid #ddd; width: 100%; text-align: center;")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Word Frequency", 
                 fluidRow(
                   column(6, h4("Removed Posts"), highchartOutput("word_freq_plot_removed")),
                   column(6, h4("Remaining Posts"), highchartOutput("word_freq_plot_remaining"))
                 )
        ),
        tabPanel("Word Cloud",
                 fluidRow(
                   column(12,
                          radioButtons("word_cloud_gram", "Gram Type:",
                                       choices = c("Uni-gram", "Bi-gram"), 
                                       selected = "Uni-gram", inline = TRUE)
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Removed Posts"),
                          wordcloud2Output("word_cloud_removed"),
                          htmlOutput("removed_word_stats")
                   ),
                   column(6,
                          h4("Remaining Posts"),
                          wordcloud2Output("word_cloud_remaining"),
                          htmlOutput("remaining_word_stats")
                   )
                 )
        ),
        tabPanel("Topic Modeling",
                 fluidRow(
                   column(12,
                          sliderInput("num_topics", "Number of Topics:", 
                                      min = 2, max = 10, value = 5)
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Removed Posts Topics"),
                          highchartOutput("topic_treemap_removed", height = "500px")
                   ),
                   column(6,
                          h4("Remaining Posts Topics"), 
                          highchartOutput("topic_treemap_remaining", height = "500px")
                   )
                 ),
                 hr(),
                 h4("Top Terms Comparison"),
                 dataTableOutput("topic_terms_table")
        ),
        tabPanel("Sentiment Analysis", 
                 fluidRow(
                   column(6, h4("Removed Posts Sentiment"), 
                          highchartOutput("sentiment_plot_removed")),
                   column(6, h4("Remaining Posts Sentiment"), 
                          highchartOutput("sentiment_plot_remaining"))
                 ),
                 hr(),
                 h4("Most Extreme Posts"),
                 fluidRow(
                   column(6, 
                          h5("Most Positive (Removed)"),
                          uiOutput("most_positive_removed_box")
                   ),
                   column(6, 
                          h5("Most Negative (Removed)"),
                          uiOutput("most_negative_removed_box")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          h5("Most Positive (Remaining)"),
                          uiOutput("most_positive_remaining_box")
                   ),
                   column(6, 
                          h5("Most Negative (Remaining)"),
                          uiOutput("most_negative_remaining_box")
                   )
                 )
        ),
        tabPanel("Text Changes",
                 fluidRow(
                   column(12,
                          h4("Text Edit Distances Between Datasets"),
                          htmlOutput("edit_distance_summary_ui"),
                          hr(),
                          h4("Most Edited Posts"),
                          dataTableOutput("most_edited_posts"),
                          hr(),
                          h4("Export Edited Posts"),
                          fluidRow(
                            column(6, downloadButton("download_csv", "Download as CSV")),
                            column(6, downloadButton("download_pdf", "Download as PDF"))
                          )
                   )
                 )
        )
      )  # Closing tabsetPanel
    )  # Closing mainPanel
  )  # Closing sidebarLayout
)  # Closing fluidPage

# ======================= #
#      BACKEND (SERVER)   #
# ======================= #
# Responsibilities:                                      #
# - Data loading & validation                            #
# - Reactive calculations                                #
# - Business logic implementation                        #
# - Output generation for UI                             #
# ====================================================== #

# ===== Define Server Logic ===== #
server <- function(input, output) {
  # ===== Text Processing Module ===== #
  text_processor <- list(
    clean = function(text) {
      corpus <- Corpus(VectorSource(text)) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeWords, stopwords("SMART")) %>%
        tm_map(stripWhitespace)
      sapply(corpus, as.character)
    },
    
    get_freq = function(text, gram_type = "Uni-gram") {
      if (gram_type == "Uni-gram") {
        corpus <- Corpus(VectorSource(text))
        dtm <- DocumentTermMatrix(corpus)
        freq <- colSums(as.matrix(dtm))
        data.frame(word = names(freq), freq = freq) %>% 
          arrange(desc(freq))
      } else {
        tibble(text = text) %>%
          unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
          count(bigram, sort = TRUE) %>%
          rename(word = bigram, freq = n)
      }
    }
  )
  
  # Create a reactive value to track if comparison was done
  comparison_done <- reactiveVal(FALSE)
  
  # Observe when compare button is pressed
  observeEvent(input$compare, {
    comparison_done(TRUE)
  })
  
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
  
  # Function to calculate Levenshtein distance between matching posts
  calculate_edit_distance <- function(df1, df2) {
    id_col_1 <- detect_id_column(df1)
    id_col_2 <- detect_id_column(df2)
    
    if (is.null(id_col_1) || is.null(id_col_2)) return(NULL)
    
    # Get matching posts
    matching <- df1 %>% 
      inner_join(df2, by = setNames(id_col_2, id_col_1), 
                 suffix = c("_1", "_2"))
    
    if (nrow(matching) == 0) return(NULL)
    
    # Calculate edit distance for each pair
    matching %>% 
      mutate(
        edit_distance = stringdist::stringdist(text_1, text_2, method = "lv"),
        normalized_distance = edit_distance / pmax(nchar(text_1), nchar(text_2))
      ) %>% 
      select(!!sym(id_col_1), text_1, text_2, edit_distance, normalized_distance)
  }
  
  # Date validation message
  output$date_validation_msg <- renderUI({
    if (is.null(input$date1) || is.null(input$date2)) {
      return(div(style = "color: red;", "Please enter dates for both datasets"))
    }
    if (input$date2 <= input$date1) {
      return(div(style = "color: red;", "Error: Dataset 2 date must be after Dataset 1"))
    }
    return(NULL)
  })
  
  
  # In your server section (add these new outputs):
  output$removed_per_day <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Daily removal rate: N/A")
    paste("Daily Removed Posts:", round(nrow(removed_posts())/days, 1), "posts/day")
  })
  
  output$daily_removal_count <- renderText({
    req(removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Total days: N/A")
    paste("Time period:", days, "days")
  })
  
  # Daily removal rate calculation - with percentage and total days
  output$removal_rate_daily <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2, data1())
    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days_diff <= 0) return("Invalid date range")
    total_posts <- nrow(data1())
    daily_removal_percent <- round((nrow(removed_posts()) / days_diff / total_posts * 100), 2)
    paste("Daily Removal Rate:", daily_removal_percent, "% of total posts/day (over", days_diff, "days)")
  })
  
  output$mean_edit_distance <- renderText({
    req(edit_distances())
    paste("Mean Edit Distance:", round(mean(edit_distances()$edit_distance), 2))
  })
  
  output$mean_normalized_distance <- renderText({
    req(edit_distances())
    paste("Mean Normalized Distance:", round(mean(edit_distances()$normalized_distance), 3))
  })
  
  
  # Enable/disable compare button based on date validity
  observe({
    # Only enable compare button when dates are valid
    if (!is.null(input$date1) && !is.null(input$date2) && input$date2 > input$date1) {
      shinyjs::enable("compare")
    } else {
      shinyjs::disable("compare")
    }
  })
  
  # Calculate the number of posts in Dataset 1
  output$dataset1_count <- renderText({
    req(comparison_done(), data1())
    paste("Number of posts in Dataset 1:", nrow(data1()))
  })
  
  # Calculate the number of posts in Dataset 2
  output$dataset2_count <- renderText({
    req(comparison_done(), data2())
    paste("Number of posts in Dataset 2:", nrow(data2()))
  })
  
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
  
  # Get remaining posts
  remaining_posts <- reactive({
    req(data1(), data2())
    data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
  })
  
  # Calculate edit distances between matching posts
  edit_distances <- reactive({
    req(data1(), data2())
    calculate_edit_distance(data1(), data2())
  })
  
  
  
  # Display the number of removed posts
  output$removed_count <- renderText({
    req(comparison_done(), removed_posts())
    paste("Number of removed posts:", nrow(removed_posts()))
  })
  
  # Calculate completeness
  output$completeness <- renderText({
    req(comparison_done(), data1(), data2())
    completeness <- (nrow(data2()) / nrow(data1())) * 100
    paste("Completeness:", round(completeness, 1), "%")
  })
  
  # Calculate data loss
  output$data_loss <- renderText({
    req(comparison_done(), removed_posts())
    data_loss <- (nrow(removed_posts()) / nrow(data1())) * 100
    paste("Data Loss:", round(data_loss, 1), "%")
  })  
  
  #Calculate Levenshtein distance 
  output$edit_distance_summary_ui <- renderUI({
    req(edit_distances())
    dist_data <- edit_distances()
    
    if (is.null(dist_data)) return(HTML("No matching posts to compare"))
    
    tagList(
      tags$span("Mean Edit Distance: ", round(mean(dist_data$edit_distance), 2),
                tags$span(icon("info-circle"), id = "edit_distance_info"),
                tags$br(),
                tags$span("Mean Normalized Distance: ", round(mean(dist_data$normalized_distance), 3),
                          tags$span(icon("info-circle"), id = "normalized_info"),
                          
                          # Initialize tooltips
                          bsTooltip("edit_distance_info", 
                                    "Levenshtein distance counts the minimum number of single-character edits needed to change one text into another",
                                    placement = "right"),
                          bsTooltip("normalized_info", 
                                    "Normalized distance divides the edit distance by the length of the longer text (range 0-1)",
                                    placement = "right")
                )))
  })
  
  # In the server function, add these download handlers: 
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("edited_posts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(edit_distances())
      write.csv(edit_distances(), file, row.names = FALSE)
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("edited_posts_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(edit_distances())
      
      # Create a temporary directory
      temp_dir <- tempdir()
      
      # Copy the report file to temp directory
      temp_report <- file.path(temp_dir, "report.Rmd")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      
      # Set up parameters
      params <- list(
        edited_posts = edit_distances(),
        date1 = input$date1,
        date2 = input$date2
      )
      
      # Knit the document
      rmarkdown::render(
        input = temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        clean = TRUE
      )
    }
  )

  # ===== Generate Word Frequency Plots ===== #
  output$word_freq_plot_removed <- renderHighchart({
    req(comparison_done(), removed_posts())
    
    text_data <- removed_posts()$text  # Assumption: The text column is named "text"
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in removed posts.", type = "error")
      return(NULL)
    }
    
    cleaned_text <- text_processor$clean(text_data)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)  # Add slicing to match original behavior
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Removed Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#AA0114", showInLegend = FALSE)
  })
  
  output$word_freq_plot_remaining <- renderHighchart({
    req(comparison_done(), data1(), data2())
    
    remaining <- data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
    
    text_data <- remaining$text
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in remaining posts.", type = "error")
      return(NULL)
    }
    
    # Clean text first
    cleaned_text <- text_processor$clean(text_data)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Remaining Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#4472c4", showInLegend = FALSE)
  })
  
  
  # ===== Generate Word Cloud ===== #

  process_text_data <- function(text_data, gram_type = "Uni-gram") {
    if (is.null(text_data) || all(text_data == "")) return(NULL)
    
    # Clean once, use everywhere
    cleaned_text <- text_processor$clean(text_data)
    
    # Dynamic max words based on dataset size
    total_words <- length(unlist(strsplit(cleaned_text, "\\s+")))
    max_words <- ifelse(total_words < 50, floor(total_words/2), 100)
    
    if (gram_type == "Uni-gram") {
      # Uni-gram processing
      corpus <- Corpus(VectorSource(cleaned_text))
      dtm <- DocumentTermMatrix(corpus)
      freq <- colSums(as.matrix(dtm))
      word_freq <- data.frame(word = names(freq), freq = freq) |> 
        arrange(desc(freq)) |> 
        filter(freq > 1) |> 
        slice(1:max_words)
    } else {
      
      # Bi-gram processing with dynamic adjustment
      text_df <- tibble(text = cleaned_text)
      word_freq <- text_df |> 
        unnest_tokens(bigram, text, token = "ngrams", n = 2) |> 
        count(bigram, sort = TRUE) |> 
        filter(n > 1) |> 
        rename(word = bigram, freq = n)
      
      # Adjust for small datasets
      available_bigrams <- nrow(word_freq)
      if (available_bigrams > 0) {
        word_freq <- word_freq |> 
          slice(1:min(max_words, available_bigrams))
      }
    }
    return(word_freq)
  }
  
  # Replace the existing reactive calculations with:
  removed_word_freq <- reactive({
    req(comparison_done(), removed_posts())
    cleaned <- text_processor$clean(removed_posts()$text)
    text_processor$get_freq(cleaned, input$word_cloud_gram) %>%
      filter(freq > 1) %>%  # Add your existing filters
      slice(1:100)          # Add your existing slicing
  }) 
  
  remaining_word_freq <- reactive({
    req(comparison_done(), remaining_posts())
    cleaned <- text_processor$clean(remaining_posts()$text)
    text_processor$get_freq(cleaned, input$word_cloud_gram) %>%
      filter(freq > 1) %>%  # Add your existing filters
      slice(1:100)          # Add your existing slicing
  }) 
  
  # Word cloud renders with dynamic sizing
  output$word_cloud_removed <- renderWordcloud2({
    req(comparison_done(), removed_posts())
    # Show loading placeholder
    if(is.null(removed_word_freq())) {
      return(div(class = "loading-placeholder", "Generating word cloud..."))
    }
    
    if (nrow(removed_word_freq()) == 0) {
      showNotification("No valid bigrams in removed posts", type = "warning")
      return(NULL)
    }
    
    wordcloud2(
      removed_word_freq(),
      size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
      minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
      gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
      color = "#FF6B6B",
      shape = 'circle'
    )
  })
  
  # Remaining posts word cloud - fixed hover issue
  output$word_cloud_remaining <- renderWordcloud2({
    req(comparison_done(), remaining_posts())
    # Show loading placeholder
    if(is.null(remaining_word_freq())) {
      return(div(class = "loading-placeholder", "Generating word cloud..."))
    }
    
    if (nrow(remaining_word_freq()) == 0) {
      showNotification("No valid bigrams in remaining posts", type = "warning")
      return(NULL)
    }
    
    wordcloud2(
      remaining_word_freq(),
      size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
      minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
      gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
      color = "#4ECDC4",
      shape = 'circle'
    )
  })
  # Add CSS for loading placeholder
  tags$style(HTML("
  .loading-placeholder {
    height: 400px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: #666;
    font-style: italic;
  }
"))
  
  # ===== Topic Modeling ===== #
  run_lda <- function(text_data, k) {
    cleaned <- text_processor$clean(text_data)
    corpus <- Corpus(VectorSource(cleaned))
    
    dtm <- DocumentTermMatrix(corpus)
    rowTotals <- apply(dtm, 1, sum)
    dtm <- dtm[rowTotals > 0, ]
    
    if (nrow(dtm) < 5) return(NULL)
    
    LDA(dtm, k = k, control = list(seed = 123))
  }
  
  create_topic_treemap <- function(lda_model, title = "") {
    topic_terms <- tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(3, beta) %>%
      summarise(terms = paste(term, collapse = ", "),
                weight = mean(beta)*100)
    
    hchart(topic_terms, "treemap", 
           hcaes(x = topic, value = weight, color = weight, name = terms),
           name = "Prevalence") %>%
      hc_title(text = title) %>%
      hc_tooltip(pointFormat = "<b>Topic {point.topic}</b><br>
              Terms: {point.terms}<br>
              Weight: {point.value:.1f}%") %>%
      hc_colorAxis(minColor = "#FFEFEF", maxColor = "#AA0114")
  }
  
  get_topic_terms_table <- function(lda_model) {
    tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      mutate(probability = round(beta*100, 1)) %>%
      summarise(Terms = paste0(term, " (", probability, "%)", collapse = ", ")) %>%
      rename(Topic = topic)
  }
  
  # Modified topic modeling with date validation
  output$topic_treemap_removed <- renderHighchart({
    req(comparison_done(), removed_posts(), input$num_topics, input$date2 > input$date1)
    lda <- run_lda(removed_posts()$text, input$num_topics)
    if(is.null(lda)) return(NULL)
    create_topic_treemap(lda, "Removed Posts Topics")
  })
  
  output$topic_treemap_remaining <- renderHighchart({
    req(comparison_done(), data1(), data2(), input$num_topics, input$date2 > input$date1)
    remaining <- data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
    lda <- run_lda(remaining$text, input$num_topics)
    if(is.null(lda)) return(NULL)
    create_topic_treemap(lda, "Remaining Posts Topics")
  })
  
  output$topic_terms_table <- renderDataTable({
    req(comparison_done(), removed_posts(), data1(), data2(), input$num_topics)
    
    removed_lda <- run_lda(removed_posts()$text, input$num_topics)
    remaining <- data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
    remaining_lda <- run_lda(remaining$text, input$num_topics)
    
    bind_cols(
      get_topic_terms_table(removed_lda) %>% rename(Removed = Terms),
      get_topic_terms_table(remaining_lda) %>% select(Remaining = Terms)
    )
  }, options = list(dom = 't'), rownames = FALSE)
  
  # ===== Sentiment Analysis ===== #
  get_sentiment_distribution <- function(text_vector) {
    if (is.null(text_vector)) {
      return(data.frame(
        category = c("Negative", "Neutral", "Positive"),
        percentage = c(0, 0, 0)
      ))
    }
    
    # Process in chunks for large datasets
    chunk_size <- 500
    chunks <- split(text_vector, ceiling(seq_along(text_vector)/chunk_size))
    
    results <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk)
      sentiment(sentences)
    })
    
    all_scores <- unlist(lapply(results, function(x) x$sentiment))
    
    category <- cut(all_scores, 
                    breaks = c(-Inf, -0.01, 0.01, Inf),
                    labels = c("Negative", "Neutral", "Positive"))
    
    counts <- table(factor(category, levels = c("Negative", "Neutral", "Positive")))
    percentages <- prop.table(counts) * 100
    
    data.frame(
      category = names(percentages),
      percentage = as.numeric(percentages)
    )
  }
  
  # Optimized version of sentiment_by for extreme posts
  get_extreme_posts <- function(df, n = 1, type = "positive") {
    if (nrow(df) == 0) return("No data")
    
    # Process in chunks
    chunk_size <- 500
    chunks <- split(df, ceiling(seq_len(nrow(df))/chunk_size))
    
    all_scores <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk$text)
      scores <- sentiment_by(sentences)
      data.frame(text = chunk$text, score = scores$ave_sentiment)
    }) %>% bind_rows()
    
    if (type == "positive") {
      all_scores %>% 
        arrange(desc(score)) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    } else {
      all_scores %>% 
        arrange(score) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    }
  }
  
  # Sentiment distribution plots 
  output$sentiment_plot_removed <- renderHighchart({
    req(comparison_done(), removed_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(removed_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Removed Posts", 
                    data = sentiment_data$percentage, 
                    color = "#AA0114") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  output$sentiment_plot_remaining <- renderHighchart({
    req(comparison_done(), remaining_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(remaining_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Remaining Posts", 
                    data = sentiment_data$percentage, 
                    color = "#4472c4") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  # Sentiment comparison table
  output$dataset_comparison_table <- renderTable({
    req(comparison_done(), data1(), data2())
    
    withProgress(message = 'Comparing datasets...', value = 0, {
      incProgress(0.3, detail = "Analyzing Dataset 1")
      sent1 <- get_sentiment_distribution(data1()$text)
      
      incProgress(0.6, detail = "Analyzing Dataset 2")
      sent2 <- get_sentiment_distribution(data2()$text)
      
      data.frame(
        Category = c("Negative", "Neutral", "Positive"),
        Dataset1 = paste0(round(sent1$percentage, 1), "%"),
        Dataset2 = paste0(round(sent2$percentage, 1), "%")
      )
    })
  })
  
  # ===== Most Extreme Posts - Dynamic Boxes ===== #
  # Reactive expressions for text
  most_positive_removed_text <- reactive({
    req(comparison_done(), removed_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(removed_posts(), type = "positive")
    })
  })
  
  most_negative_removed_text <- reactive({
    req(comparison_done(), removed_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(removed_posts(), type = "negative")
    })
  })
  
  most_positive_remaining_text <- reactive({
    req(comparison_done(), remaining_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(remaining_posts(), type = "positive")
    })
  })
  
  most_negative_remaining_text <- reactive({
    req(comparison_done(), remaining_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(remaining_posts(), type = "negative")
    })
  })
  
  # Dynamic UI boxes
  output$most_positive_removed_box <- renderUI({
    text <- most_positive_removed_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_removed_box <- renderUI({
    text <- most_negative_removed_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_positive_remaining_box <- renderUI({
    text <- most_positive_remaining_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_remaining_box <- renderUI({
    text <- most_negative_remaining_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  output$most_edited_posts <- renderDataTable({
    req(edit_distances())
    edit_distances() %>% 
      arrange(desc(edit_distance)) %>% 
      slice_head(n = 20) %>% 
      datatable(options = list(
        pageLength = 5,
        scrollX = TRUE,
        autoWidth = TRUE
      ))
  })
}

# ======================= #
#       APPLICATION       #
#       LAUNCHER          #
# ======================= #
shinyApp(ui, server)