install.packages(c("tidyr","dplyr","ggplot2","lubridate","rwhatsapp","tidytxt","stopwords","ggimage"))
library(c("tidyr","dplyr","ggplot2","lubridate","rwhatsapp","tidytxt","stopwords","ggimage"))


chatlog <- rwa_read(file.choose())
#prompts user to choose a textfile

trash <- c((stopwords),
           "media",
           "bild",
           "weggelassen",
           "i'm",
           "im",
           "ur",
           "i'll",
           "1","2","3","4",
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
  na.omit(chats) %>%
  filter(!word %in% trash) %>%
  filter(!word %in% trash.indo)
#removes na values + words in trash, trash.indo

#---- messages per day ----
chats %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")

#---- messages per week ----
chats <- chats %>%
  mutate(week.format = 
           wday(as.Date(chats$time), 
           label = TRUE, 
           week_start = getOption("lubridate.week.start", 1)))
  
weeks <- chats %>% 
  group_by(week.format) %>% 
  summarise(count = n())
  ggplot(weeks, aes(
    x = week.format, 
    y = count, 
    fill = week.format)
    ) +
  geom_bar(stat = "identity")+
  xlab("Days of the week")+
  ylab("Messages")+
  theme(legend.position="none")+
  coord_flip()+
  ggtitle("Days most active")

#---- time most active, GMT+7 ----
chats <- chats %>%
  mutate(
    hour.format = hour(chats$time), 
    label = TRUE
    )
  
hourz <- chats %>%
  group_by(hour.format) %>%
  summarize(count=n())
  
ggplot(hourz, aes(
    x = hour.format, 
    y = count, 
    fill = hour.format)
  )+
  geom_bar(stat = "identity") +
  xlab("time of day") +
  ylab("total messages") +
  ggtitle("Hours most active")


#---- total amount of messages sent per person----
chats %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")

#---- 6 most used emojis ----
emoji.url <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", tolower(hex_runes1), ".png"))

chats %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji.url, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#---- wordcloud ----
library(wordcloud)

chats%>%
  count(word) %>%
  with(wordcloud(
    word, n, 
    colors = c("#EF767A", "#FFE347","#7D7ABC","#6457A6","#F2B5D4"), 
    scale=c(3,0.5), 
    max.words = 125)
    )

