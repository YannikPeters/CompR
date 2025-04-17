**Word Frequency Code Explanation (Unigram and Bigram for Removed & Remaining Posts)**

---

### 1. **Text Cleaning**
**Function:** `text_processor$clean`

- **Input:** Raw text from posts
- **Purpose:** Clean the text to prepare for analysis
- **Steps Involved:**
  - Convert text to lowercase: `tolower`
  - Remove numbers: `removeNumbers`
  - Remove punctuation: `removePunctuation`
  - Remove stop words (common words): `stopwords(source = "smart")`
  - Strip whitespace: `stripWhitespace`
  
```r
cleaned <- text_processor$clean(removed_posts()$text)
```

---

### 2. **Word Frequency Calculation**
**Function:** `text_processor$get_freq`

- **Input:** Cleaned text
- **Parameter:** `gram_type` ("Uni-gram" or "Bi-gram")
- **Purpose:** Calculate word frequency based on the n-gram type

#### If Uni-gram:
```r
corpus <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
```
- Count individual words
- Output: Data frame with columns `word` and `freq`

#### If Bi-gram:
```r
tibble(text = text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
```
- Split text into two-word phrases (bi-grams)
- Count frequency of each phrase

---

### 3. **Process Word Frequencies for Word Cloud**
**Reactive Variables:**

#### a. **Removed Posts**
```r
removed_word_freq <- reactive({
  cleaned <- text_processor$clean(removed_posts()$text)
  text_processor$get_freq(cleaned, input$word_cloud_gram) %>%
    filter(freq > 1) %>%
    slice(1:100)
})
```
- Clean the removed posts' text
- Get frequency of unigrams or bigrams
- Filter words that appear more than once
- Take top 100 frequent words

#### b. **Remaining Posts**
```r
remaining_word_freq <- reactive({
  cleaned <- text_processor$clean(remaining_posts()$text)
  text_processor$get_freq(cleaned, input$word_cloud_gram) %>%
    filter(freq > 1) %>%
    slice(1:100)
})
```
- Same process applied to remaining posts

---

### 4. **Render Word Clouds**
**Functions:** `renderWordcloud2`

#### a. **Removed Posts Word Cloud**
```r
output$word_cloud_removed <- renderWordcloud2({
  wordcloud2(removed_word_freq(), ...)
})
```
- Displays most common words or bigrams in removed posts

#### b. **Remaining Posts Word Cloud**
```r
output$word_cloud_remaining <- renderWordcloud2({
  wordcloud2(remaining_word_freq(), ...)
})
```
- Displays most common words or bigrams in remaining posts

**Customization:**
- `size`, `minSize`, `gridSize`: Adjust size based on gram type
- `color`: Red for removed, teal for remaining
- `shape`: Circle

---

### Summary of Key Variables/Functions
| Name                     | Purpose                                         |
|--------------------------|-------------------------------------------------|
| `text_processor`         | Holds cleaning and frequency functions          |
| `clean()`                | Cleans raw text                                |
| `get_freq()`             | Calculates frequency of unigrams or bigrams    |
| `removed_word_freq`      | Word frequencies from removed posts            |
| `remaining_word_freq`    | Word frequencies from remaining posts          |
| `renderWordcloud2()`     | Displays the word cloud                        |
| `input$word_cloud_gram` | Chooses between Uni-gram or Bi-gram            |

---

This document breaks down each step and variable used in calculating and visualizing word frequency for both removed and remaining posts.

