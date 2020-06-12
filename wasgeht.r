library("tidyr")
library("dplyr")
library("ggplot2"); #theme_set(theme_minimal())
library("lubridate")
library("rwhatsapp")
library("tidytext")
library("reshape2")
library("stopwords")

chat <- rwa_read(file.choose())

remove <- c((stopwords),
               "media",
               "weggelassen",
               "bild",
               "i'm",
               "im",
               "ur",
               "1",
               "2",
               "3",
               "4",
               "im",
               "i'll",
               "audio",
               "video",
               "ab",
               "gif")

remove.indo <- c((stopwords),
                   "tp",
                   "dia",
                   "gua",
                   "dan"
                   )

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")