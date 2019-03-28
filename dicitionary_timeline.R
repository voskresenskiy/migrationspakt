library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(scales)
library(bbplot)

dict = data.frame()
names = c('afd_youtube', 'afd_twitter', 'afd_add','afd_infl', 'rnis', 'mm', 'undecided')

for (i in c(1:7)){
  df = read_excel('dict_new.xlsx', sheet = i, col_names = T)
  df = as.data.frame(apply(df, 2, function(x) as.character(x)))
  df$type = rep(names[i], nrow(df))
  dict = bind_rows(dict, df)
}


migr_df = bind_rows(migr_df1, migr_df2)
migr_df$title = str_to_lower(migr_df$title)
migr_df$description = str_to_lower(migr_df$description)
migr_df = migr_df %>%
  filter(str_detect(title, 'migrationspakt|global compact|pakt|globale')|
           str_detect(description, 'migrationspakt|global compact|globale'))

migr_df$channelTitle = str_to_lower(migr_df$channelTitle)
dict$`YouTube channel name` = str_to_lower(dict$`YouTube channel name`)
dict$`YouTube link` = str_replace_all(dict$`YouTube link`, 'https://www.youtube.com/channel/', '')
dict$`YouTube link` = str_replace_all(dict$`YouTube link`, '\\/.+', '')

dict_types = dict %>% 
  select('yt_name' = `YouTube channel name`,'yt_id' = `YouTube link`, type) 


migr_df = left_join(migr_df, dict_types[,c(1,3)], by = c("channelTitle" = "yt_name"))
migr_df = left_join(migr_df, dict_types[,c(2,3)], by = c("channelId" = "yt_id"))
migr_df$type = ifelse(!is.na(migr_df$type.x), migr_df$type.x,
                      ifelse(is.na(migr_df$type.x) & !is.na(migr_df$type.y), migr_df$type.y, NA))

migr_df$type.x = NULL
migr_df$type.y = NULL
migr_df$type = ifelse(str_detect(migr_df$type, 'afd'), 'afd', migr_df$type)
migr_df = unique(migr_df)

migr_df %>%
  group_by(type) %>%
  summarise(amount = n())

migr_df %>%
  filter(!is.na(type) & type!= 'undecided') %>%
  group_by(type,month=floor_date(as.Date(publishedAt), "month")) %>%
  summarise(amount=n()) %>%
  ggplot() +
  geom_line(aes(x = month, y = amount, group = type, color = type), size = 1) +
  geom_text(aes(x = month, y = amount + 1, label = amount), size = 5) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%m-%Y")) +
  bbc_style() +
  theme(axis.title = element_text(size = 15)) + 
  labs(x = "", y = "# of videos") 

migr_df_filt = migr_df %>%
  filter(!is.na(type) & type!= 'undecided')
