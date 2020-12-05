# Install GutenbergR and essential packages
install.packages("gutenbergr")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")
install.packages("curl")

# Invoke the installed packages
library(gutenbergr)
library(tidytext)
library(dplyr)
library(stringr)
library(curl)

# Corpus being used:
# Peter and Wendy - J. M. Barrie: #16
# The Wonderful Wizard of Oz - L. Frank Baum: #55
# Alice's Adventures in Wonderland - Lewis Carroll: #11
scifi <- gutenberg_download(c(16, 55, 11))

# Eliminate stopwords
data(stop_words)
tidy_scifi <- scifi %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Find most frequent words in corpus
tidy_scifi %>%
  count(word, sort = TRUE)

# Peter and Wendy - J. M. Barrie
scifi1 <- gutenberg_download(16)
tidy_scifi1 <- scifi1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_scifi1 %>%
  count(word, sort = TRUE)

# The Wonderful Wizard of Oz - L. Frank Baum
scifi2 <- gutenberg_download(55)
tidy_scifi2 <- scifi2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_scifi2 %>%
  count(word, sort = TRUE)

# Alice's Adventures in Wonderland - Lewis Carroll
scifi3 <- gutenberg_download(11)
tidy_scifi3 <- scifi3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_scifi3 %>%
  count(word, sort = TRUE)

# Install new packages
install.packages("tidyr")
install.packages("scales")
library(tidyr)

# Bind the data frames and calculate proportion of each word
frequency <- bind_rows(mutate(tidy_scifi1, author = "Peter_Pan"),
                       mutate(tidy_scifi2, author = "The_Wonderful_Wizard_of_Oz"), 
                       mutate(tidy_scifi3, author = "T_Alice_in_Wonderland")) %>% # For some reason I cannot use the proper name for Alice
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `The_Wonderful_Wizard_of_Oz`:`T_Alice_in_Wonderland`)

# Plot the results
install.packages("ggplot2")
library(ggplot2)
library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Peter_Pan`, color = abs(`Peter_Pan` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Peter_Pan", x = NULL)