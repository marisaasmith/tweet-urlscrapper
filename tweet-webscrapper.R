

getSource <- function(df){
  df %>% 
    mutate(source=full_url %>%
             stringr::str_replace("^(https?:\\/\\/)?(www\\.)?", "") %>%
             stringr::str_extract("([\\da-z\\.-]+)\\.([a-z\\.]{2,6})") %>%
             stringr::str_replace("^[am]{1}\\.", ""))
}

getUrl <- function(df){
  require(tidyverse)
  df %>%
    mutate(url = stringr::str_extract_all(text, "https://t.co/[a-z,A-Z,0-9]*")) %>%
    tidyr::unnest(url) %>% dplyr::rowwise() %>%
    mutate(full_url = ifelse(stringr::str_detect(url,"https://t.co/[a-z,A-Z,0-9]*", negate = F) == T, 
                             {httr::GET(url) %>% magrittr::use_series("url")}, 
                             NA)) %>% group_by(full_url) %>% 
    distinct() %>% select(-url) %>% getSource()
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
