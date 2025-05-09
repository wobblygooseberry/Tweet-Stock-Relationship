# Dataset used: stock-market-tweets-data.csv (~170 MB)
# Source: https://huggingface.co/datasets/StephanAkkerman/stock-market-tweets-data

library(tidyverse)
library(vader)
library(data.table)
library(readr)

data <- as_tibble(fread("stock-market-tweets-data.csv"))
data$symbols <- ""  

data <- data %>%
  mutate(ticker_symbol = case_when(
    str_detect(text, regex("AAPL|Apple", ignore_case = TRUE)) ~ "AAPL",
    str_detect(text, regex("AMZN|Amazon", ignore_case = TRUE)) ~ "AMZN",
    TRUE ~ NA_character_
  ))

new_data <- data %>%
  filter(ticker_symbol %in% c("AAPL", "AMZN")) %>%
  select(ticker_symbol, text, created_at)


sentiment_scores <- as_tibble(vader_df(new_data$text))

sentiment_scores_clean <- sentiment_scores %>%
  filter(word_scores != "ERROR") %>%
  select(compound)

cleaned_new_data_sample <- new_data_sample %>%
  filter(sentiment_scores$word_scores != "ERROR")

sentiment_data <- bind_cols(cleaned_new_data_sample, sentiment_scores_clean)

positive_sentiment_data <- sentiment_data %>%
  filter(compound >= 0.05)

negative_sentiment_data <- sentiment_data %>%
  filter(compound <= -0.05)

neutral_sentiment_data <- sentiment_data %>%
  filter(compound > -0.05 & compound < 0.05)

aapl_sentiment_data <- positive_sentiment_data %>%
  filter(ticker_symbol == "AAPL")

amzn_sentiment_data <- positive_sentiment_data %>%
  filter(ticker_symbol == "AMZN")

aapl_neg_sentiment_data <- negative_sentiment_data %>%
  filter(ticker_symbol == "AAPL")

amzn_neg_sentiment_data <- negative_sentiment_data %>%
  filter(ticker_symbol == "AMZN")

aapl_neu_sentiment_data <- neutral_sentiment_data %>%
  filter(ticker_symbol == "AAPL")

amzn_neu_sentiment_data <- neutral_sentiment_data %>%
  filter(ticker_symbol == "AMZN")

fwrite(aapl_sentiment_data, "AAPL_sentiment.csv")

fwrite(amzn_sentiment_data, "AMZN_sentiment.csv")

fwrite(aapl_neg_sentiment_data, "AAPL_neg_sentiment.csv")

fwrite(amzn_neg_sentiment_data, "AMZN_neg_sentiment.csv")

fwrite(aapl_neu_sentiment_data, "AAPL_neu_sentiment.csv")

fwrite(amzn_neu_sentiment_data, "AMZN_neu_sentiment.csv")


