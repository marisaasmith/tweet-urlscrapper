## tweet-urlscrapper

This R code is useful for examing news shared by twitter users. The functions provided: 

* Expands shortened urls embedded in tweets
* Provides the news source
* Scraps web for text from the news article 

You may apply functions to vector objects or a data frame of tweets. Code was created consider the packages `rtweet` and `streamR`. However, the functions work on any data frame. If using a dataframe, tweets must be stored in a column entitled *text*. 

`getURL` creates a data frame containing all of the urls shared the corresponding tweet. Since some tweets contain multiple urls, the data frame may contain duplicate tweets (but it will not duplicate matches of the tweet and the url). 

&nbsp;

##### Example for url and source detection

Tweets were collected using `rtweet`. Sample of 87 tweets containing the keywords *Trump* or *Obama*. 

```{r}
tweets <- read.csv("https://raw.githubusercontent.com/marisaasmith/tweet-urlscrapper/master/tweets-example.csv", header=TRUE)
```

Run `fullUrl`, `getSource`, and `getUrl` functions

```{r}

fullUrl <- function(vector){
  require(dplyr)
  x <- c()
  for (i in 1:length(vector)) { 
    x[i] <- httr::GET(url = vector[i]) %>% 
      magrittr::use_series("url")
  }
  return(x)
} ## function returns vector of full urls converted from shortened urls 


getSource <- function(vector){
  require(dplyr)
  source <- vector %>%
  {gsub("^https://|^https://www.|^http://www.", "", .)}%>%
  { gsub("\\>.com.*","", .) } %>%
  { gsub("\\>.org.*","", .) } %>%
  { gsub("\\>.co.uk.*","", .) }
  return(source)
} ## function returns the news source


getUrl <- function(df){
  require(dplyr)
  df %>%
    mutate(url = stringr::str_extract_all(text, "https://t.co/[a-z,A-Z,0-9]*")) %>%
    tidyr::unnest(url) %>%
    mutate(full.url = fullUrl(url)) %>%
    group_by(full.url) %>% 
    distinct() %>% select(-url) %>%
    mutate(source = getSource (full.url))
}
```

`getUrl` adds column of expanded urls (*full.url*) and the source (*source*)

```{r}
tweets <- getUrl(tweets)
```
&nbsp;

##### Example for web scrapping 

`getText` uses `rvest` to shared scrap news articles. **NOTE:** The code uses generic xPath 'p' for webscrapping. Depending on the website, you may want to change the path. I recommend the Google Chrome <a href="https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb?hl=en">selector gadget</a>


```{r}
getText <- function(url_vector, source_vector){
  require(rvest)
  require(tidyverse)
  source_vector <- grep("twitter", source_vector, value = TRUE, invert = T)
  url.count <- grep("twitter", url_vector, value = TRUE, invert = T)
  url.count <-  1:length(url.count)
  article.text.full <- {}
  for (i in url.count) {
    article.text  <- data_frame(text = read_html(v[i]) %>% 
                                  html_nodes("p") %>% 
                                  html_text())
    
    Article.withID <- cbind(article.text, source_vector[i])
    Article.withID <- as.tibble(Article.withID)
    
    
    article.text.full <- bind_rows(article.text.full, Article.withID) 
  }
  return(article.text.full)
  
}
```

`getText` returns a data frame of text and the corresponding source (excluding twitter sources)

```{r}
getText(tweets$full.url, tweets$source)
```
&nbsp;
&nbsp;
&nbsp;
&nbsp;

