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
library(quanteda) # Add this line
library(quanteda.textstats)
library(KeynessMeasures)  # Add this line
library(servr)
library(LDAvis)


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
  ')),
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
      
      # conditionalPanel(
      #   condition = "input.tabset == 'Sentiment Analysis'",
      #   hr(),
      #   h4("Sentiment Comparison: Dataset 1 vs Dataset 2"),
      #   tableOutput("dataset_comparison_table") %>% 
      #     tagAppendAttributes(style = "border: 1px solid #ddd; width: 100%; text-align: center;")
      # )
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
        # tabPanel("Word Cloud",
        #          fluidRow(
        #            column(12,
        #                   radioButtons("word_cloud_gram", "Gram Type:",
        #                                choices = c("Uni-gram", "Bi-gram"), 
        #                                selected = "Uni-gram", inline = TRUE)
        #            )
        #          ),
        #          fluidRow(
        #            column(6,
        #                   h4("Removed Posts"),
        #                   wordcloud2Output("word_cloud_removed"),
        #                   htmlOutput("removed_word_stats")
        #            ),
        #            column(6,
        #                   h4("Remaining Posts"),
        #                   wordcloud2Output("word_cloud_remaining"),
        #                   htmlOutput("remaining_word_stats")
        #            )
        #          )
        # ),
        tabPanel("Topic Modeling",
                 fluidRow(
                   column(12,
                          sliderInput("num_topics", "Number of Topics:", 
                                      min = 2, max = 10, value = 5),
                          radioButtons("topic_dataset", "Show Topics For:",
                                       choices = c("Removed Posts", "Remaining Posts"),
                                       selected = "Removed Posts",
                                       inline = TRUE)
                   )
                 ),
                 uiOutput("ldavis_output")
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
        ),
        tabPanel("Keyness Analysis",
                 # In your UI section, add:
                 fluidRow(
                   column(12,
                          uiOutput("keyness_controls"),  # This will render the tab panel
                          highchartOutput("keyness_plot"),
                          uiOutput("keyness_interpretation")
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
        tm_map(removeWords, stopwords(source = "smart")) %>%
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
  output$word_cloud_removed <- renderWordcloud2({
    req(comparison_done(), removed_posts())
    
    if (is.null(removed_posts()$text) || all(removed_posts()$text == "")) {
      return(div(class = "loading-placeholder", "No text data in removed posts"))
    }
    
    cleaned_text <- text_processor$clean(removed_posts()$text)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)
    
    if (nrow(word_freq) == 0) {
      return(div(class = "loading-placeholder", "No frequent words in removed posts"))
    }
    
    wc <- wordcloud2(
      word_freq,
      size = 0.8,
      minSize = 0.01,
      gridSize = 10,
      color = "#FF6B6B",
      shape = 'circle'
    )
    
    wc %>% htmlwidgets::onRender("
    function(el) {
      var canvas = el.querySelector('canvas');
      if (!canvas) return;
      
      canvas.addEventListener('mousemove', function(evt) {
        if (evt.target.tagName === 'CANVAS') {
          var word = evt.target.textContent;
          if (word) {
            Shiny.setInputValue('hover_word_removed', word);
          }
        }
      });
      
      canvas.addEventListener('mouseout', function(e) {
        Shiny.setInputValue('hover_word_removed', null);
      });
    }
  ")
  })
  
  output$word_cloud_remaining <- renderWordcloud2({
    req(comparison_done(), remaining_posts())
    
    if (is.null(remaining_posts()$text) || all(remaining_posts()$text == "")) {
      return(div(class = "loading-placeholder", "No text data in remaining posts"))
    }
    
    cleaned_text <- text_processor$clean(remaining_posts()$text)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)
    
    if (nrow(word_freq) == 0) {
      return(div(class = "loading-placeholder", "No frequent words in remaining posts"))
    }
    
    wc <- wordcloud2(
      word_freq,
      size = 0.8,
      minSize = 0.01,
      gridSize = 10,
      color = "#4ECDC4",
      shape = 'circle'
    )
    
    wc %>% htmlwidgets::onRender("
    function(el) {
      var canvas = el.querySelector('canvas');
      if (!canvas) return;
      
      canvas.addEventListener('mousemove', function(evt) {
        if (evt.target.tagName === 'CANVAS') {
          var word = evt.target.textContent;
          if (word) {
            Shiny.setInputValue('hover_word_remaining', word);
          }
        }
      });
      
      canvas.addEventListener('mouseout', function(e) {
        Shiny.setInputValue('hover_word_remaining', null);
      });
    }
  ")
  }) 
  
  observeEvent(input$hover_word, {
    if (!is.null(input$hover_word)) {
      word <- input$hover_word$word
      source <- input$hover_word$source
      
      # Handle the hover based on the word and source
      if (source == "removed") {
        # Handle hover on removed word cloud
      } else if (source == "remaining") {
        # Handle hover on remaining word cloud
      }
    }
  })
  
  
  
  
#   output$word_cloud_removed <- renderWordcloud2({
#     req(comparison_done(), removed_posts())
#     
#     if (is.null(removed_posts()$text) || all(removed_posts()$text == "")) {
#       return(div(class = "loading-placeholder", "No text data in removed posts"))
#     }
#     
#     cleaned_text <- text_processor$clean(removed_posts()$text)
#     word_freq <- text_processor$get_freq(cleaned_text) %>%
#       filter(freq > 1) %>%
#       slice(1:100)
#     
#     if (nrow(word_freq) == 0) {
#       return(div(class = "loading-placeholder", "No frequent words in removed posts"))
#     }
#     
#     wc <- wordcloud2(
#       word_freq,
#       size = 0.8,
#       minSize = 0.01,
#       gridSize = 10,
#       color = "#FF6B6B",
#       shape = 'circle'
#     )
#     
#     wc %>% htmlwidgets::onRender(JS("
#     function(el) {
#       var canvas = el.querySelector('canvas');
#       if (!canvas) return;
#       
#       canvas.addEventListener('mousemove', function(evt) {
#         var rect = canvas.getBoundingClientRect();
#         var x = evt.clientX - rect.left;
#         var y = evt.clientY - rect.top;
#         
#         if (evt.target.tagName === 'CANVAS') {
#           var word = evt.target.textContent;
#           Shiny.setInputValue('hover_word_removed', word);
#         }
#       });
#       
#       canvas.addEventListener('mouseout', function(e) {
#         Shiny.setInputValue('hover_word_removed', null);
#       });
#     }
#   "))
#   })
#   
#   output$word_cloud_remaining <- renderWordcloud2({
#     req(comparison_done(), remaining_posts())
#     
#     if (is.null(remaining_posts()$text) || all(remaining_posts()$text == "")) {
#       return(div(class = "loading-placeholder", "No text data in remaining posts"))
#     }
#     
#     cleaned_text <- text_processor$clean(remaining_posts()$text)
#     word_freq <- text_processor$get_freq(cleaned_text) %>%
#       filter(freq > 1) %>%
#       slice(1:100)
#     
#     if (nrow(word_freq) == 0) {
#       return(div(class = "loading-placeholder", "No frequent words in remaining posts"))
#     }
#     
#     wc <- wordcloud2(
#       word_freq,
#       size = 0.8,
#       minSize = 0.01,
#       gridSize = 10,
#       color = "#4ECDC4",
#       shape = 'circle'
#     )
#     
#     wc %>% htmlwidgets::onRender(JS("
#     function(el) {
#       var canvas = el.querySelector('canvas');
#       if (!canvas) return;
#       
#       canvas.addEventListener('mousemove', function(evt) {
#         var rect = canvas.getBoundingClientRect();
#         var x = evt.clientX - rect.left;
#         var y = evt.clientY - rect.top;
#         
#         if (evt.target.tagName === 'CANVAS') {
#           var word = evt.target.textContent;
#           Shiny.setInputValue('hover_word_remaining', word);
#         }
#       });
#       
#       canvas.addEventListener('mouseout', function(e) {
#         Shiny.setInputValue('hover_word_remaining', null);
#       });
#     }
#   "))
#   })
#   
#   # Add CSS
#   tags$style(HTML("
#   .loading-placeholder {
#     height: 400px;
#     display: flex;
#     align-items: center;
#     justify-content: center;
#     color: #666;
#     font-style: italic;
#   }
#   .word-highlight {
#     font-weight: 900 !important;
#     font-size: 1.5em !important;
#     fill: #FFA500 !important;
#     cursor: pointer;
#   }
# "))
#   
#   # Observers for hover effects
#   observeEvent(input$hover_word_removed, {
#     if (is.null(input$hover_word_removed)) {
#       runjs('
#       document.querySelector("#word_cloud_removed").querySelectorAll(".word-highlight").forEach(el => {
#         el.classList.remove("word-highlight");
#       });
#     ')
#     } else {
#       runjs(sprintf('
#       document.querySelector("#word_cloud_removed").querySelectorAll(".word-highlight").forEach(el => {
#         el.classList.remove("word-highlight");
#       });
#       document.querySelector("#word_cloud_removed").querySelectorAll("span").forEach(el => {
#         if(el.textContent === "%s") {
#           el.classList.add("word-highlight");
#         }
#       });
#     ', input$hover_word_removed))
#     }
#   }, ignoreNULL = FALSE)
#   
#   observeEvent(input$hover_word_remaining, {
#     if (is.null(input$hover_word_remaining)) {
#       runjs('
#       document.querySelector("#word_cloud_remaining").querySelectorAll(".word-highlight").forEach(el => {
#         el.classList.remove("word-highlight");
#       });
#     ')
#     } else {
#       runjs(sprintf('
#       document.querySelector("#word_cloud_remaining").querySelectorAll(".word-highlight").forEach(el => {
#         el.classList.remove("word-highlight");
#       });
#       document.querySelector("#word_cloud_remaining").querySelectorAll("span").forEach(el => {
#         if(el.textContent === "%s") {
#           el.classList.add("word-highlight");
#         }
#       });
#     ', input$hover_word_remaining))
#     }
#   }, ignoreNULL = FALSE)
  

  
  
  
#   process_text_data <- function(text_data, gram_type = "Uni-gram") {
#     if (is.null(text_data) || all(text_data == "")) return(NULL)
#     
#     # Clean once, use everywhere
#     cleaned_text <- text_processor$clean(text_data)
#     
#     # Dynamic max words based on dataset size
#     total_words <- length(unlist(strsplit(cleaned_text, "\\s+")))
#     max_words <- ifelse(total_words < 50, floor(total_words/2), 100)
#     
#     if (gram_type == "Uni-gram") {
#       # Uni-gram processing
#       corpus <- Corpus(VectorSource(cleaned_text))
#       dtm <- DocumentTermMatrix(corpus)
#       freq <- colSums(as.matrix(dtm))
#       word_freq <- data.frame(word = names(freq), freq = freq) |> 
#         arrange(desc(freq)) |> 
#         filter(freq > 1) |> 
#         slice(1:max_words)
#     } else {
# 
#       # Bi-gram processing with dynamic adjustment
#       text_df <- tibble(text = cleaned_text)
#       word_freq <- text_df |>
#         unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
#         count(bigram, sort = TRUE) |>
#         filter(n > 1) |>
#         rename(word = bigram, freq = n)
# 
#       # Adjust for small datasets
#       available_bigrams <- nrow(word_freq)
#       if (available_bigrams > 0) {
#         word_freq <- word_freq |>
#           slice(1:min(max_words, available_bigrams))
#       }
#     }
#     return(word_freq)
#   }
#   
#   # Replace the existing reactive calculations with:
#   removed_word_freq <- reactive({
#     req(comparison_done(), removed_posts())
#     cleaned <- text_processor$clean(removed_posts()$text)
#     text_processor$get_freq(cleaned, gram_type = "Uni-gram") %>%
#       filter(freq > 1) %>%  # Add your existing filters
#       slice(1:100)          # Add your existing slicing
#   }) 
#   
#   remaining_word_freq <- reactive({
#     req(comparison_done(), remaining_posts())
#     cleaned <- text_processor$clean(remaining_posts()$text)
#     text_processor$get_freq(cleaned, gram_type = "Uni-gram") %>%
#       filter(freq > 1) %>%  # Add your existing filters
#       slice(1:100)          # Add your existing slicing
#   }) 
#   
#   # Word cloud renders with dynamic sizing
#   output$word_cloud_removed <- renderWordcloud2({
#     req(comparison_done(), removed_posts())
#     # Show loading placeholder
#     if(is.null(removed_word_freq())) {
#       return(div(class = "loading-placeholder", "Generating word cloud..."))
#     }
#     
#     if (nrow(removed_word_freq()) == 0) {
#       showNotification("No valid bigrams in removed posts", type = "warning")
#       return(NULL)
#     }
#     
#     wordcloud2(
#       removed_word_freq(),
#       size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
#       minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
#       gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
#       color = "#FF6B6B",
#       shape = 'circle'
#     )
#   })
#   
#   # Remaining posts word cloud - fixed hover issue
#   output$word_cloud_remaining <- renderWordcloud2({
#     req(comparison_done(), remaining_posts())
#     # Show loading placeholder
#     if(is.null(remaining_word_freq())) {
#       return(div(class = "loading-placeholder", "Generating word cloud..."))
#     }
#     
#     if (nrow(remaining_word_freq()) == 0) {
#       showNotification("No valid bigrams in remaining posts", type = "warning")
#       return(NULL)
#     }
#     
#     wordcloud2(
#       remaining_word_freq(),
#       size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
#       minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
#       gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
#       color = "#4ECDC4",
#       shape = 'circle'
#     )
#   })
#   # Add CSS for loading placeholder
#   tags$style(HTML("
#   .loading-placeholder {
#     height: 400px;
#     display: flex;
#     align-items: center;
#     justify-content: center;
#     color: #666;
#     font-style: italic;
#   }
# "))
  
  
  # ===== Topic Modeling ===== #
  # Helper: run LDA on cleaned text
  run_lda <- function(text_data, k) {
    cleaned <- text_processor$clean(text_data)
    if (length(cleaned) < 5) return(NULL)
    corpus <- Corpus(VectorSource(cleaned))
    dtm <- DocumentTermMatrix(corpus)
    dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
    if (nrow(dtm) < 5 || ncol(dtm) < 5) return(NULL)
    tryCatch(LDA(dtm, k = k, control = list(seed = 123, verbose = 0)),
             error = function(e) NULL)
  }
  
  # Helper: prepare the JSON for LDAvis
  prepare_ldavis <- function(lda_model, text_data) {
    cleaned <- text_processor$clean(text_data)
    if (length(cleaned) < 5) return(NULL)
    corpus <- Corpus(VectorSource(cleaned))
    dtm <- DocumentTermMatrix(corpus)
    dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
    if (nrow(dtm) < 5) return(NULL)
    phi   <- as.matrix(posterior(lda_model)$terms)
    theta <- as.matrix(posterior(lda_model)$topics)
    if (anyNA(phi) || anyNA(theta)) return(NULL)
    # Try cmdscale, else PCA fallback
    json <- tryCatch({
      createJSON(phi, theta,
                 doc.length     = as.integer(rowSums(as.matrix(dtm))),
                 vocab          = colnames(phi),
                 term.frequency = as.integer(colSums(as.matrix(dtm))),
                 mds.method     = stats::cmdscale)
    }, error = function(e) {
      createJSON(phi, theta,
                 doc.length     = as.integer(rowSums(as.matrix(dtm))),
                 vocab          = colnames(phi),
                 term.frequency = as.integer(colSums(as.matrix(dtm))),
                 mds.method     = function(x) stats::prcomp(x, scale = TRUE)$x[,1:2])
    })
    json
  }
  
  output$ldavis_output <- renderUI({
    req(comparison_done(), input$num_topics)
    
    # Get selected dataset
    dataset <- if(input$topic_dataset == "Removed Posts") {
      req(removed_posts())
      removed_posts()
    } else {
      req(remaining_posts())
      remaining_posts()
    }
    
    # Validate text data
    if(nrow(dataset) < 5 || all(dataset$text == "")) {
      return(div(class = "alert alert-warning",
                 "Not enough text data for topic modeling (minimum 5 documents required)"))
    }
    
    withProgress(message = 'Generating topics...', value = 0.5, {
      # Create temporary directory
      vis_dir <- tempfile()
      dir.create(vis_dir)
      
      # Run LDA and create visualization
      lda <- run_lda(dataset$text, input$num_topics)
      if(is.null(lda)) {
        return(div(class = "alert alert-danger",
                   "Topic modeling failed - insufficient meaningful text patterns"))
      }
      
      # Prepare LDAvis data
      json <- prepare_ldavis(lda, dataset$text)
      if(is.null(json)) {
        return(div(class = "alert alert-danger",
                   "Visualization failed - unable to process text patterns"))
      }
      
      # Save visualization to temporary directory
      serVis(json, out.dir = vis_dir, open.browser = FALSE)
      
      # Return iframe with visualization
      tags$iframe(
        src = file.path(vis_dir, "index.html"),
        style = "width: 100%; height: 600px; border: none;"
      )
    })
  })
        
  
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
  
  # # Sentiment comparison table
  # output$dataset_comparison_table <- renderTable({
  #   req(comparison_done(), data1(), data2())
  #   
  #   withProgress(message = 'Comparing datasets...', value = 0, {
  #     incProgress(0.3, detail = "Analyzing Dataset 1")
  #     sent1 <- get_sentiment_distribution(data1()$text)
  #     
  #     incProgress(0.6, detail = "Analyzing Dataset 2")
  #     sent2 <- get_sentiment_distribution(data2()$text)
  #     
  #     data.frame(
  #       Category = c("Negative", "Neutral", "Positive"),
  #       Dataset1 = paste0(round(sent1$percentage, 1), "%"),
  #       Dataset2 = paste0(round(sent2$percentage, 1), "%")
  #     )
  #   })
  # })
  
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
  
  # ===== Keyness Analysis Module ===== #
  keyness_analyzer <- list(
    prepare_data = function(removed_posts, remaining_posts) {
      # Create frequency table for keyness analysis
      text_removed <- text_processor$clean(removed_posts$text)
      text_remaining <- text_processor$clean(remaining_posts$text)
      
      # Create a combined dataframe with group indicator
      combined_df <- data.frame(
        text = c(text_removed, text_remaining),
        group = c(rep("removed", length(text_removed)), 
                  rep("remaining", length(text_remaining)))
      )
      
      # Create frequency table using KeynessMeasures
      frequency_table <- frequency_table_creator(
        df = combined_df,
        text_field = "text",
        grouping_variable = "group",
        grouping_variable_target = "removed",
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE
      )
      
      return(frequency_table)
    },
    
    calculate_keyness = function(frequency_table) {
      # Calculate keyness measures
      keyness_measures <- keyness_measure_calculator(
        frequency_table,
        log_likelihood = TRUE,
        ell = TRUE,
        bic = TRUE,
        perc_diff = TRUE,
        log_ratio = TRUE,
        sort = "decreasing",
        sort_by = "log_likelihood"
      )
      
      return(keyness_measures)
    }
  )
  
  # Keyness analysis reactive
  keyness_results <- reactive({
    req(removed_posts(), remaining_posts())
    
    withProgress(message = 'Analyzing key terms...', value = 0.5, {
      # Prepare frequency table
      frequency_table <- keyness_analyzer$prepare_data(removed_posts(), remaining_posts())
      
      # Calculate keyness measures
      full_keyness_measures <- keyness_analyzer$calculate_keyness(frequency_table)
      
      # Create two separate datasets - one for overuse (removed) and one for underuse (remaining)
      overuse_terms <- full_keyness_measures %>%
        filter(word_use == "overuse", log_likelihood > 3.84) %>%  # 95% confidence threshold
        arrange(desc(log_likelihood)) %>%
        slice(1:15)  # Limit to top 15 most significant words
      
      underuse_terms <- full_keyness_measures %>%
        filter(word_use == "underuse", log_likelihood > 3.84) %>%  # 95% confidence threshold
        arrange(desc(log_likelihood)) %>%
        slice(1:15)  # Limit to top 15 most significant words
      
      # Return both datasets in a list
      return(list(
        overuse = overuse_terms,
        underuse = underuse_terms,
        all = full_keyness_measures %>% 
          filter(log_likelihood > 3.84) %>%
          arrange(desc(log_likelihood))
      ))
    })
  })
  
  # Add UI elements for tab selection
  output$keyness_controls <- renderUI({
    req(keyness_results())
    
    tabsetPanel(
      id = "keyness_tabs",
      tabPanel("Removed Posts", value = "removed"),
      tabPanel("Remaining Posts", value = "remaining"),
      tabPanel("Combined View", value = "combined")
    )
  })
  
  # Render the appropriate chart based on selected tab
  output$keyness_plot <- renderHighchart({
    req(keyness_results(), input$keyness_tabs)
    
    # Get the appropriate dataset based on selected tab
    if(input$keyness_tabs == "removed") {
      keyness_data <- keyness_results()$overuse %>%
        mutate(
          color = "#AA0114",
          y = log_likelihood
        )
      title_text <- "Terms Distinctive of Removed Posts"
      
    } else if(input$keyness_tabs == "remaining") {
      keyness_data <- keyness_results()$underuse %>%
        mutate(
          color = "#4472C4",
          y = log_likelihood
        )
      title_text <- "Terms Distinctive of Remaining Posts"
      
    } else { # Combined view
      # Get top 8 from each category for combined view
      overuse_top <- keyness_results()$overuse %>% 
        slice(1:8) %>%
        mutate(
          color = "#AA0114",
          y = log_likelihood
        )
      
      underuse_top <- keyness_results()$underuse %>% 
        slice(1:8) %>%
        mutate(
          color = "#4472C4",
          y = -log_likelihood  # Negative to show below axis
        )
      
      keyness_data <- bind_rows(overuse_top, underuse_top) %>%
        arrange(desc(abs(y)))
      
      title_text <- "Keyness Analysis: Removed vs Remaining Posts"
    }
    
    # Create highchart
    hc <- highchart() %>%
      hc_chart(type = "bar", height = 500) %>%  # Increased height for better readability
      hc_title(text = title_text) %>%
      hc_subtitle(text = paste0("Comparing ", nrow(removed_posts()), " removed posts to ", 
                                nrow(remaining_posts()), " remaining posts")) %>%
      hc_xAxis(
        categories = keyness_data$word,
        labels = list(style = list(fontSize = "11px"))  # Smaller font for word labels
      ) %>%
      hc_yAxis(
        title = list(text = "Log-likelihood"),
        labels = list(format = "{value}"),
        plotLines = if(input$keyness_tabs == "combined") list(list(
          value = 0, color = "#666", width = 1, zIndex = 5
        )) else NULL
      ) %>%
      hc_tooltip(
        formatter = JS("function() {
        var corpus = this.point.y > 0 ? 'Removed' : 'Remaining';
        var ll = Math.abs(this.point.y).toFixed(2);
        var perc = this.point.perc_diff ? this.point.perc_diff.toFixed(2) : 'N/A';
        var ratio = this.point.log_ratio ? this.point.log_ratio.toFixed(2) : 'N/A';
        return '<b>' + this.point.category + '</b><br>' +
               'More frequent in: <b>' + corpus + '</b><br>' +
               'Log-likelihood: ' + ll + '<br>' +
               '%DIFF: ' + perc + '%<br>' +
               'Log Ratio: ' + ratio;
      }")
      ) %>%
      hc_plotOptions(series = list(colorByPoint = TRUE))
    
    # Add data points with custom colors
    hc %>% hc_add_series(
      data = lapply(1:nrow(keyness_data), function(i) {
        list(
          y = keyness_data$y[i],
          color = keyness_data$color[i],
          perc_diff = keyness_data$perc_diff[i],
          log_ratio = keyness_data$log_ratio[i]
        )
      }),
      showInLegend = FALSE
    )
  })
  
  # Update the interpretation based on selected tab
  output$keyness_interpretation <- renderUI({
    req(keyness_results(), input$keyness_tabs)
    
    if(input$keyness_tabs == "removed") {
      # Show interpretation for removed posts
      top_terms <- keyness_results()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), 
                             ", %DIFF: ", round(perc_diff, 1), "%)"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Interpretation: Terms Distinctive of Removed Posts</h5>",
        "<div style='color: #AA0114;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These terms appear significantly more often in removed posts than in remaining posts.<br>",
        "Log-likelihood (LL) indicates statistical significance (values >3.84 are significant at p<0.05).<br>",
        "%DIFF shows the percentage difference in frequency between corpora.",
        "</p>",
        "</div>"
      ))
      
    } else if(input$keyness_tabs == "remaining") {
      # Show interpretation for remaining posts
      top_terms <- keyness_results()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), 
                             ", %DIFF: ", round(abs(perc_diff), 1), "%)"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Interpretation: Terms Distinctive of Remaining Posts</h5>",
        "<div style='color: #4472C4;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These terms appear significantly more often in remaining posts than in removed posts.<br>",
        "Log-likelihood (LL) indicates statistical significance (values >3.84 are significant at p<0.05).<br>",
        "%DIFF shows the percentage difference in frequency between corpora.",
        "</p>",
        "</div>"
      ))
      
    } else {
      # Show combined interpretation
      top_removed <- keyness_results()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), 
                             ", %DIFF: ", round(perc_diff, 1), "%)"))
      
      top_remaining <- keyness_results()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), 
                             ", %DIFF: ", round(abs(perc_diff), 1), "%)"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Interpretation Guide</h5>",
        "<div style='columns: 2;'>",
        "<div style='color: #AA0114;'>",
        "<strong>Distinctive in Removed Posts (", nrow(removed_posts()), " posts):</strong><br>",
        paste("- ", top_removed$info, collapse = "<br>"),
        "</div>",
        "<div style='color: #4472C4; margin-left: 30px;'>",
        "<strong>Distinctive in Remaining Posts (", nrow(remaining_posts()), " posts):</strong><br>",
        paste("- ", top_remaining$info, collapse = "<br>"),
        "</div>",
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "Keyness measures how distinctive a term is in one corpus compared to another.<br>",
        "Log-likelihood (LL) indicates statistical significance (values >3.84 are significant at p<0.05).<br>",
        "%DIFF shows the percentage difference in frequency between corpora.<br><br>",
        "<strong>Note:</strong> With a large imbalance between corpus sizes (", nrow(removed_posts()), " vs ", 
        nrow(remaining_posts()), "), extremely high %DIFF values are expected for terms appearing almost exclusively in the smaller corpus.",
        "</p>",
        "</div>"
      ))
    }
  })
  
}

# ======================= #
#       APPLICATION       #
#       LAUNCHER          #
# ======================= #
shinyApp(ui, server)