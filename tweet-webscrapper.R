getSource <- function(vector){
  require(tidyverse)
  source <- vector %>%
  {gsub("^https://|^https://www.|^http://www.|http://", "", .)}%>%
  { gsub("\\>.com.*","", .) } %>%
  { gsub("\\>.org.*","", .) } %>%
  { gsub("\\>.co.uk.*","", .) }
  return(source)
} ## function returns the news source


getUrl <- function(df){
  require(tidyverse)
  df %>%
    mutate(url = stringr::str_extract_all(text, "https://t.co/[a-z,A-Z,0-9]*")) %>%
    tidyr::unnest(url) %>% rowwise() %>%
    mutate(full_url = ifelse(stringr::str_detect(url,"https://t.co/[a-z,A-Z,0-9]*", negate = F) == T, 
                             {httr::GET(url) %>% magrittr::use_series("url")}, 
                             NA)) %>% group_by(full_url) %>% 
    distinct() %>% select(-url) %>% 
    mutate(source = getSource (full_url))
}


getText <- function(url_vector, source_vector){
  require(rvest)
  require(tidyverse)
  source_vector <- test$source %>%
  {grep("twitter", ., value = TRUE, invert = T)}%>%
  { grep("youtube", ., value = TRUE, invert = T) }
  url_vector <- url_vector %>% {grep("twitter", ., value = TRUE, invert = T)} %>% 
  {grep("youtube", ., value = TRUE, invert = T)}
  url_count <-  1:length(url_vector)
  article.text.full <- {}
  for (i in url_count) {
    article.text  <- data_frame(text = read_html(url_vector[i]) %>% 
                                  html_nodes("p") %>% 
                                  html_text())
    
    Article.withID <- cbind(article.text, source_vector[i])
    Article.withID <- as.tibble(Article.withID)
    
    
    article.text.full <- bind_rows(article.text.full, Article.withID) 
  }
  return(article.text.full)
  
}
