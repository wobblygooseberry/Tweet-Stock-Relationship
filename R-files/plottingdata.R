library(dplyr)
library(ggplot2)
library(readr)

aapl_sentiment_data <- read_csv("AAPL_sentiment.csv")
amzn_sentiment_data <- read_csv("AMZN_sentiment.csv")

daily_aapl_counts <- aapl_sentiment_data %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(date) %>%
  summarise(tweet_count = n())

daily_amzn_counts <- amzn_sentiment_data %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(date) %>%
  summarise(tweet_count = n())


aapl_tweets <- ggplot(daily_aapl_counts, aes(x = as.Date(date), y = tweet_count)) +
  geom_line() +
  geom_point(color = "blue", size = 3) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    limits = c(0, max(daily_aapl_counts$tweet_count)),
    breaks = seq(0, max(daily_aapl_counts$tweet_count), by = 250)
  ) +
  labs(
    title = "AAPL Tweets Over Time (4/9/2020 - 7/16/2020)",
    x = "Day of Year 2020",
    y = "Number of Tweets"
  )


amzn_tweets <- ggplot(daily_amzn_counts, aes(x = as.Date(date), y = tweet_count)) +
  geom_line() +
  geom_point(color = "blue", size = 3) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    limits = c(0, max(daily_amzn_counts$tweet_count)),
    breaks = seq(0, max(daily_amzn_counts$tweet_count), by = 250)) +
  labs(
    title = "AMZN Tweets Over Time (4/9/2020 - 7/16/2020)",
    x = "Day of Year 2020",
    y = "Number of Tweets"
  )

aapl_stock_price <- tibble(
  date = as.Date(c("2020-04-09", "2020-04-13", "2020-04-14", "2020-04-15", "2020-04-16", "2020-04-17",
                   "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24", "2020-04-27",
                   "2020-04-28", "2020-04-29", "2020-04-30", "2020-05-01", "2020-05-04", "2020-05-05",
                   "2020-05-06", "2020-05-07", "2020-05-08", "2020-05-11", "2020-05-12", "2020-05-13",
                   "2020-05-14", "2020-05-15", "2020-05-18", "2020-05-19", "2020-05-20", "2020-05-21",
                   "2020-05-22", "2020-05-26", "2020-05-27", "2020-05-28", "2020-05-29", "2020-06-01",
                   "2020-06-02", "2020-06-03", "2020-06-04", "2020-06-05", "2020-06-08", "2020-06-09",
                   "2020-06-10", "2020-06-11", "2020-06-12", "2020-06-15", "2020-06-16", "2020-06-17",
                   "2020-06-18", "2020-06-19", "2020-06-22", "2020-06-23", "2020-06-24", "2020-06-25",
                   "2020-06-26", "2020-06-29", "2020-06-30", "2020-07-01", "2020-07-02", "2020-07-06",
                   "2020-07-07", "2020-07-08", "2020-07-09", "2020-07-10", "2020-07-13", "2020-07-14",
                   "2020-07-15", "2020-07-16"
  )),
  price = c(65.04, 66.31, 69.66, 69.03, 69.57, 68.63,
            67.21, 65.13, 67.00, 66.74, 68.67, 68.72,
            67.61, 69.83, 71.30, 70.15, 71.14, 72.21,
            72.96, 73.71, 75.47, 76.65, 75.78, 74.86,
            75.32, 74.88, 76.64, 76.20, 77.68, 77.10,
            77.60, 77.07, 77.41, 77.44, 77.37, 78.32,
            78.68, 79.11, 78.43, 80.67, 81.14, 83.71,
            85.86, 81.74, 82.44, 83.46, 85.67, 85.55,
            85.59, 85.10, 87.33, 89.19, 87.62, 88.78,
            86.05, 88.03, 88.77, 88.60, 88.60, 90.97,
            90.69, 92.80, 93.20, 93.36, 92.93, 94.47,
            95.12, 93.95)
)

amzn_stock_price <- tibble(
  date = as.Date(c(
    "2020-04-09", "2020-04-13", "2020-04-14", "2020-04-15", "2020-04-16", "2020-04-17",
    "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24", "2020-04-27",
    "2020-04-28", "2020-04-29", "2020-04-30", "2020-05-01", "2020-05-04", "2020-05-05",
    "2020-05-06", "2020-05-07", "2020-05-08", "2020-05-11", "2020-05-12", "2020-05-13",
    "2020-05-14", "2020-05-15", "2020-05-18", "2020-05-19", "2020-05-20", "2020-05-21",
    "2020-05-22", "2020-05-26", "2020-05-27", "2020-05-28", "2020-05-29", "2020-06-01",
    "2020-06-02", "2020-06-03", "2020-06-04", "2020-06-05", "2020-06-08", "2020-06-09",
    "2020-06-10", "2020-06-11", "2020-06-12", "2020-06-15", "2020-06-16", "2020-06-17",
    "2020-06-18", "2020-06-19", "2020-06-22", "2020-06-23", "2020-06-24", "2020-06-25",
    "2020-06-26", "2020-06-29", "2020-06-30", "2020-07-01", "2020-07-02", "2020-07-06",
    "2020-07-07", "2020-07-08", "2020-07-09", "2020-07-10", "2020-07-13", "2020-07-14",
    "2020-07-15", "2020-07-16"
  )),
  price = c(102.14, 108.44, 114.17, 115.38, 120.41, 118.75,
            119.68, 116.41, 118.17, 119.97, 120.51, 118.80,
            115.70, 118.64, 123.70, 114.30, 115.80, 115.89,
            117.56, 118.38, 118.98, 120.45, 117.85, 118.40,
            119.44, 120.49, 121.31, 122.47, 124.90, 122.34,
            121.84, 121.09, 120.52, 120.06, 122.12, 123.55,
            123.62, 123.92, 123.03, 124.15, 126.20, 130.04,
            132.37, 127.90, 127.25, 128.63, 130.76, 132.05,
            132.70, 133.75, 135.69, 138.22, 136.72, 137.73,
            134.64, 134.02, 137.94, 143.93, 144.51, 152.85,
            150.01, 154.06, 159.13, 160.00, 155.20, 154.20,
            150.44, 149.99)
)

aapl_stock_plot <- ggplot(aapl_stock_price, aes(x = as.Date(date), y = price)) +
  geom_line() +
  geom_point(color = "blue", size = 3) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    limits = c(0, max(aapl_stock_price$price)),
    breaks = seq(0, max(aapl_stock_price$price), by = 20)) +
  labs(
    title = "AAPL: Price Over Time (4/9/2020 - 7/16/2020)",
    x = "Day of Year 2020",
    y = "Price (USD)"
  )
  
amzn_stock_plot <- ggplot(amzn_stock_price, aes(x = as.Date(date), y = price)) +
  geom_line() +
  geom_point(color = "blue", size = 3) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    limits = c(0, max(amzn_stock_price$price)),
    breaks = seq(0, max(amzn_stock_price$price), by = 40)) +
  labs(
    title = "AMZN: Price Over Time (4/9/2020 - 7/16/2020)",
    x = "Day of Year 2020",
    y = "Price (USD)"
  )

aapl_combined <- inner_join(daily_aapl_counts, aapl_stock_price, by = "date")

aapl_c_plot <- ggplot(aapl_combined, aes(x = tweet_count, y = price)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_text(aes(label = format(date, "%b %d")), vjust = -1, size = 3) +  
  scale_y_continuous(
    limits = c(0, max(aapl_combined$price)),
    breaks = seq(0, max(aapl_combined$price), by = 20)) +
  labs(
    title = "AAPL: Tweets vs. Stock Price (4/9/2020 - 7/16/2020)",
    x = "Number of Positive Tweets",
    y = "Stock Price (USD)"
  ) 

amzn_combined <- inner_join(daily_amzn_counts, amzn_stock_price, by = "date")

amzn_c_plot <- ggplot(amzn_combined, aes(x = tweet_count, y = price)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_text(aes(label = format(date, "%b %d")), vjust = -1, size = 3) + 
  scale_y_continuous(
    limits = c(0, max(amzn_combined$price)),
    breaks = seq(0, max(amzn_combined$price), by = 40)) +
  labs(
    title = "AMZN: Tweets vs. Stock Price (4/9/2020 - 7/16/2020)",
    x = "Number of Positive Tweets",
    y = "Stock Price (USD)"
  )

aapl_lm <- lm(tweet_count ~ price, data = aapl_combined)

amzn_lm <- lm(tweet_count ~ price, data = amzn_combined)





