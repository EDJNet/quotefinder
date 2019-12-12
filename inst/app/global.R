library("dplyr")
library("tidyr")
library("purrr")
library("scales")
library("ggplot2")
library("shiny")
library("tidytext")
library("wordcloud")
library("wordcloud2")
library("stringr")
library("reshape2")
library("stopwords")
library("syuzhet")
library("colourpicker")
library("shinyWidgets")
library("RColorBrewer")
library("shinydashboard")
library("shinycustomloader")
library("DT")
library("webshot")

extrafont::loadfonts()

# qf data
dataset <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "dataset.rds"))
hashtagsList <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "tweets_hashtags_list.rds"))
trendingHashtags <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "tweets_trending_hashtags_list.rds"))
lang <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "tweets_lang_list.rds"))
EPGroupShort <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "EPGroupShort.rds"))
countries <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "countries.rds"))
palettes <- readRDS(file = file.path(.quotefinder.path,"qf_data", "tweets_processed", "palettes.rds"))
# castarter dataset
castarter_dataset <- readRDS(file = file.path(.quotefinder.path,"qf_data", "castarter_dataset", "castarter_dataset.rds"))

available_websites <- unique(castarter_dataset[["website"]])

langTable <- left_join(x = tibble::tibble(lang = unlist(lang)),
                       y = readRDS(file.path(.quotefinder.path, "qf_data", "tweets_processed", "langCode.rds")) %>%
                         rename(lang = alpha2), by = "lang") %>% 
  mutate(English = stringr::str_extract(string = English,
                                        pattern = regex("[[:alnum:]]+")))



embed_tweet_js <- function(id, i) {
  HTML(paste0('<div id="tweetcontainer', i, '"></div>',
              "<script>twttr.widgets.createTweet('", id, "',
  document.getElementById('tweetcontainer", i, "'),
  {
    theme: 'light'
  }
); </script>"))
}

embed_profile <- function(screen_name) {
  HTML(paste0('<a class="twitter-timeline" data-height="600" href="https://twitter.com/', screen_name, '">Tweets by ', screen_name, '</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'))
}



pal <- brewer.pal(9,"Blues")
pal <- pal[-(1:5)]


# European formatting of large numbers
point <- scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)

## function to give wordcloud2 click interactivity
## from https://github.com/Lchiffon/wordcloud2/issues/25
wc2ClickedWord = function(cloudOutputId, inputId) {
  shiny::tags$script(shiny::HTML(
    sprintf("$(document).on('click', '#%s', function() {", cloudOutputId),
    'word = document.getElementById("wcSpan").innerHTML;',
    sprintf("Shiny.onInputChange('%s', word);", inputId),
    "});"
  ))
}

# wordcloud2 html output fix from https://github.com/Lchiffon/wordcloud2/issues/20
simpleFixWc2 <- function(inputFile, outputFile){
  a = readLines(inputFile)
  output = paste(a, collapse = "\n")
  output = gsub(">\n\n</div>","></div>",output)
  writeLines(output, outputFile)
  invisible(NULL)
}


# Enable bookmarking
enableBookmarking(store = "server")
