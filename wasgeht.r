library("tidyr")
library("dplyr")
library("ggplot2"); #theme_set(theme_minimal())
library("lubridate")
library("rwhatsapp")
library("tidytext")
library("reshape2")
library("stopwords")

chatlog <- rwa_read(file.choose())
#prompts user to choose a textfile

trash <- c((stopwords),
               "media",
               "weggelassen",
               "i'm",
               "im",
               "ur",
               "i'll",
               "audio",
               "video",
               "gif")
#removes common english words + whatsapp terminology

trash.indo <- c((stopwords),
                   "tp",
                   "dia",
                   "gua",
                   "dan"
                   )
#removes common indonesian words

chats <- chatlog %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chats <- chats %>%
  na.omit(chats)
  filter(!word %in% trash, trash.indo)
#removes na values + words in trash, trash.indo

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")
#displays plot for messages per day
