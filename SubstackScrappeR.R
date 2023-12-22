library(tidyverse)
library(RSelenium)

# Start the web driver and client
driver <- rsDriver(browser = c("firefox"), port = 4445L, chromever = "114.0.5735.90")
remote_driver <- driver[["client"]]

# Send to browser the web site address and scroll down x pages 
remote_driver$navigate("https://betonit.substack.com/archive")
scroll_d <- remote_driver$findElement("css", "body")

for (i in 1:10) {
  scroll_d$sendKeysToElement(list(key = "end"))
  Sys.sleep(2)
}

# Find all the titles   
titles_elem <- remote_driver$findElements(using = "css", value = ".frontend-pencraft-Text-module__clamp-3--R6uR3")
titles <- unlist(lapply(titles_elem, function(x) x$getElementText()))

# Find all the links
links <- list()

for (i in 1:length(titles_elem)) {
  l <- titles_elem[[i]]$getElementAttribute("href")
  links <- c(links, l)
}

links2 <- unlist(links)

# Get text bodies from all the links
articles <- list()

for (i in 1:length(links2)) {
  remote_driver$navigate(links2[i])
  Sys.sleep(3)
  a <- remote_driver$findElement(using = "css", value = ".markup")$getElementText() %>% 
    unlist(.) %>%
    str_replace_all(., "[\r\n]", " ")
  articles <- c(articles, a)
}

articles2 <- unlist(articles)

# Count words and find summary
bet_on_it <- data.frame(title = titles, link = links2, article = articles2)
bet_on_it2 <- bet_on_it %>%
  mutate(words = str_count(string = article, pattern = '\\w+')) %>%
  filter(words > 100)

bet_summarize <- bet_on_it2 %>%
  summarise(word_mean = round(mean(words), 0),
            word_median = round(median(words), 0),
            word_sd = round(sd(words), 0))

# Write to file
write_csv2(bet_on_it2, "Articles from BetOnIt.csv")
