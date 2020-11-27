---
title: "Peter Pan and Sentiment Analysis"
author: "Ngoc Hoang"
date: "11/28/2020"
output: html_document
---

# Import packages and invoke libraries
install.packages(c("tidytext","textdata","gutenbergr","ggplot2","tidyr","janeaustenr","stringr","devtools","curl"))
library(ggplot2)
library(tidyr)
library(tidytext)
library(textdata)
get_sentiments("afinn") # Needs approval from console
get_sentiments("nrc")
get_sentiments("bing")
library(dplyr)
library(stringr)

# Import the book using the Gutenberg package
# Make it into a tibble
library(gutenbergr)
library(curl)
pg_text <- gutenberg_download(c(16)) 
pg_text <- select(pg_text, text)
pg_index <- mutate(pg_text, index = (as.integer(rownames(pg_text)) %/% 40), book="Peter Pan")
pg_text <- mutate(pg_text, linenumber = as.integer(rownames(pg_text)), book="Peter Pan")
tidy_pg <- pg_text %>%
  unnest_tokens(word, text)

# Compute sentiment scores using three lexicons
afinn <- tidy_pg %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 40) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "afinn")

bing_and_nrc <- bind_rows(
  tidy_pg %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "bing"),
  tidy_pg %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "nrc")) %>%
  count(method, index = linenumber %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Visualize
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Jitter visualization using the 'nrc' lexicon
pg_sentiment2 <- tidy_pg %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, index = linenumber %/% 160, sentiment) # %>%
ggplot(pg_sentiment2, aes(index, sentiment, colour = sentiment)) +
  geom_jitter(position=position_jitter(0.25)) 

# Get general trends from smoothened plots
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, color = sentiment)) +
  geom_smooth(inherit.aes = TRUE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Most common positive and negative words
# Count
bing_word_counts <- tidy_pg %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# Visualize
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# ------------------------------------------------------
#Scraped code
# Append the 'bing' sentiment lexicon to chunks of 160 lines at a time
library(tidyr)
pg_sentiment <- tidy_pg %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = linenumber %/% 160, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Use a point-based plot to visualize
library(ggplot2)
ggplot(pg_sentiment, aes(index, sentiment, colour = sentiment)) +
  geom_point(position=position_jitter(0.3)) 

# Plot the general trend in sentiment (still the 'bing' lexicon)
ggplot(pg_sentiment, aes(index, sentiment, colour = sentiment)) +
     geom_smooth(data = pg_sentiment, inherit.aes = TRUE)

# Look into the general sentiment of sections of 160 lines each
peterpan_sentiment <- tidy_pg %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 160, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(peterpan_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  labs (x = "Segment of Peter Pan", y = "Sentiment score")

# Get words classified as a certain 'nrc' emotion
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
tidy_pg %>%
  filter(book == "Peter Pan") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Get words rated a certain 'afinn' score
afinn_verypositive <- get_sentiments("afinn") %>% 
  filter(value == 5)
tidy_pg %>%
  filter(book == "Peter Pan") %>%
  inner_join(afinn_verypositive) %>%
  count(word, sort = TRUE)

# Index things
index_50 <- pg_index %>% 
  filter(index < 2)
index_50