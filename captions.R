get_captions_modified = function(x){
  captions = list_caption_tracks(video_id = x) 
  if(c('en') %in% captions$language){
    lang = 'en'
  } else {
    lang = head(captions$language, 1)[[1]]
  }
  y = GET(str_c('http://video.google.com/timedtext?lang=', lang, '&v=', x))
  y = rawToChar(as.raw(strtoi(y$content, 16L)))
  y = y %>% str_replace_all('<.*?>|[0-9]+|[[:punct:]]|\n', ' ') %>%
    str_replace_all('amp|quot', "") 
  if(str_length(y) >= 5){
    return(y) 
  } else {
    page = str_c('https://www.diycaptions.com/php/get-automatic-captions-as-txt.php?id=',x, '&language=asr')
    page = read_html(page)
    y = page %>%
      html_nodes(xpath = "/html/body/div/div/div") %>%
      html_text()
    y = str_replace_all(y, '\\[Musik]|\t|\\[|\\]', '')
    if(str_length(y) >= 5){
      return(y) 
    } else {
      return('none')
    }
  }
}