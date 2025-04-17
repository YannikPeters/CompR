**Word Frequency Code - Step-by-Step Explanation**

This document explains the process involved in generating word frequency plots for both **removed** and **remaining** posts using `renderHighchart`. The logic is the same for both, with slight differences in the dataset.

---

### Step-by-Step: Word Frequency for Removed Posts

**Function:** `output$word_freq_plot_removed <- renderHighchart({ ... })`

1. **Ensure comparison is done and data is available:**
   ```r
   req(comparison_done(), removed_posts())
   ```
   - Makes sure the compare button was clicked.
   - Makes sure `removed_posts()` data is ready.

2. **Extract text column from removed posts:**
   ```r
   text_data <- removed_posts()$text
   ```
   - Pulls the `text` column from the removed posts data.

3. **Check if text data exists:**
   ```r
   if (is.null(text_data) || length(text_data) == 0) {...}
   ```
   - Displays an error notification if there's no text.

4. **Clean the text using text processor:**
   ```r
   cleaned_text <- text_processor$clean(text_data)
   ```
   - Applies the cleaning steps:
     - Lowercasing
     - Removing numbers
     - Removing punctuation
     - Removing stopwords
     - Removing whitespace

5. **Get word frequency:**
   ```r
   word_freq <- text_processor$get_freq(cleaned_text)
   ```
   - Uses uni-gram mode by default.
   - Counts how many times each word appears.

6. **Filter words used more than once & take top 100:**
   ```r
   filter(freq > 1) %>% slice(1:100)
   ```
   - Only keeps words with freq > 1.
   - Limits the result to top 100 words.

7. **Create highchart bar plot:**
   ```r
   highchart() %>% hc_chart(type = "bar") ...
   ```
   - Builds the bar chart using the top words and their frequencies.
   - Adds scroll bar if more than 20 items.
   - Uses red color `#AA0114` for removed posts.

---

### Step-by-Step: Word Frequency for Remaining Posts

**Function:** `output$word_freq_plot_remaining <- renderHighchart({ ... })`

1. **Check data is ready:**
   ```r
   req(comparison_done(), data1(), data2())
   ```

2. **Get matching (remaining) posts:**
   ```r
   remaining <- data1() %>%
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
   ```
   - Finds posts with matching IDs between Dataset 1 and Dataset 2.

3. **Extract `text` column:**
   ```r
   text_data <- remaining$text
   ```

4. **Check if text exists:**
   - Same as removed section.

5. **Clean the text:**
   ```r
   cleaned_text <- text_processor$clean(text_data)
   ```

6. **Get word frequency:**
   ```r
   word_freq <- text_processor$get_freq(cleaned_text)
   ```

7. **Filter top 100 with freq > 1:**
   - Same as removed section.

8. **Create highchart:**
   - Similar chart but with different color: `#4472c4` (blue).

---

### Text Processing Module

**Defined functions used in both sections:**

- `text_processor$clean(text)`
  - Uses `tm_map()` functions from `tm` to clean the text.
- `text_processor$get_freq(text, gram_type)`
  - Default is Uni-gram (single words): Uses `DocumentTermMatrix()`
  - If Bi-gram: Uses `unnest_tokens(..., token = 'ngrams', n = 2)` to get 2-word combinations.

---

### Summary

Both sections follow the same logic:
1. Get text
2. Clean it
3. Get frequency (uni-gram or bi-gram)
4. Filter top 100 words
5. Plot with `highchart()`

The main difference is **which dataset** is used: `removed_posts()` vs `remaining`.

Colors used:
- Removed: Red `#AA0114`
- Remaining: Blue `#4472c4`

