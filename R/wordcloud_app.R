#' Starts wordcloud shiny app
#'
#' Starts wordcloud shiny app
#'
#' @export
#' @examples
#' \dontrun{
#' AnalyseDataset()
#' }

qf_wordcloud_app <- function(shiny_port = 3939,  shiny_host = "0.0.0.0") {
  if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
    stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
  }
  if (requireNamespace("tidytext", quietly = TRUE)==FALSE) {
    stop("You need to install the `tidytext` package with `install.packages('tidytext')` to use this function.")
  }
  if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
    stop("You need to install the `DT` package with `install.packages('DT')` to use this function.")
  }
 
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
  
  # install phantomjs at first run to enable downloading png wordclouds
  # webshot::install_phantomjs()
  
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
  
  dataset_global_file_time <- fs::file_info(path = fs::path("qf_data", "tweets_processed", "dataset.rds")) %>% 
    dplyr::pull(modification_time)
  
  dataset <- readRDS(file = file.path("qf_data", "tweets_processed", "dataset.rds"))
  hashtagsList <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_hashtags_list.rds"))
  trendingHashtags <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_trending_hashtags_list.rds"))
  lang <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_lang_list.rds"))
  EPGroupShort <- readRDS(file = file.path("qf_data", "tweets_processed", "EPGroupShort.rds"))
  countries <- readRDS(file = file.path("qf_data", "tweets_processed", "countries.rds"))
  palettes <- readRDS(file = file.path("qf_data", "tweets_processed", "palettes.rds"))
  
  
  shiny::shinyApp(ui = shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "EdjNet QuoteFinder"),
    shinydashboard::dashboardSidebar(sidebarMenu(
      menuItem("Twitter", tabName = "TwitterMEP", icon = icon("twitter")),
      shiny::HTML("<hr><div class='col-sm-12'><p><b>The QuoteFinder lets you explore</b></p></div>"),
      infoBox(title = "tweets", value = point(nrow(dataset)), icon = icon("twitter"), width = 12, color = "blue", fill = TRUE),
      infoBox(title = "by", value = paste(length(unique(dataset$screen_name)), "MEPs"), icon = icon("users"), width = 12, color = "blue", fill = TRUE)),
      infoBox(title = "posted since", value = min(dataset$date), icon = icon("calendar"), width = 12, color = "blue", fill = TRUE),
      disable = TRUE
    ),
    dashboardBody(
      #  wc2ClickedWord(cloudOutputId = "wordcloud2", inputId = "selected_word"),
      
      tabItems(
        
        ##### TwitterMEP tab ####
        tabItem(tabName = "TwitterMEP",
                
                #### Box 1: Wordcloud ####
                tabBox(id = "wordcloud_plot",
                       tabPanel("Wordcloud",
                                uiOutput("warning"),
                                wordcloud2Output("wordcloud2"),
                                splitLayout(
                                  shiny::sliderInput(inputId = "sizeVarWC2",
                                                     label = "Wordcloud size",
                                                     min = 0.1, 
                                                     max = 2,
                                                     value = 0.5,
                                                     sep = "."
                                  ),
                                  shiny::sliderInput(inputId = "MaxWords",
                                                     label = "Max number of words",
                                                     min = 0L, 
                                                     max = 1000L,
                                                     value = 200L,
                                                     sep = ".", width = "95%"
                                  )
                                ),
                                splitLayout(shiny::uiOutput(outputId = "colourMost_UI"),
                                            shiny::uiOutput(outputId = "colourLeast_UI"),
                                            cellWidths = "50%",
                                            tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
                                splitLayout(downloadButton("downloadHtml", "Download Html"),
                                            downloadButton("downloadPng", "Download PNG"))
                                
                                # , 
                                # HTML("<b>Tip</b>: by clicking on a term in the wordcloud, only tweets including it are shown in the table below.")
                       ),
                       
                       #### Box1, Tab2: Barcharts #####
                       tabPanel("Barcharts",
                                plotOutput('barchartGG'),
                                shiny::sliderInput(inputId = "MaxWordsInBarchart",
                                                   label = "Max number of words",
                                                   min = 0L, 
                                                   max = 30L,
                                                   value = 20L,
                                                   sep = ".",
                                                   width = "95%",
                                                   round = TRUE
                                ),
                                downloadButton("downloadTidyWordCount", "Download as spreadsheet")),
                       tabPanel("EP group comparison",
                                plotOutput("barchartComparisonGG"),
                                radioButtons(inputId = "wcOrBarchartComparison",
                                             label = "How would you like to compare?",
                                             choices = as.list(c("Comparison wordcloud",
                                                                 "Commonality wordcloud",
                                                                 "Comparison barchart")))
                       )
                       # ,
                       # tabPanel("Classic wordcloud",
                       #          plotOutput(outputId = "wordcloud"))
                ),
                
                #### Box 2: Date and language ####
                box(
                  shiny::radioButtons(inputId = "dateRangeRadio",
                                      label = "Select date range",
                                      choices = as.list(c("Last week", "Last month", "Last three months", "Custom range")),
                                      selected = "Last month", inline = TRUE),
                  conditionalPanel(
                    condition = "input.dateRangeRadio == 'Custom range'",
                    shiny::dateRangeInput(inputId = 'dateRange',
                                          weekstart = 1,
                                          label = "Select date range",
                                          start = min(dataset$date), end = max(dataset$date)
                    )
                  )
                  ,
                  HTML("<b>Filter tweets by language</b>"),
                  shiny::checkboxInput(inputId = "anyLanguage",
                                       label = "Any language",
                                       value = FALSE),
                  conditionalPanel("input.anyLanguage == false",
                                   shiny::selectInput(inputId = "language",
                                                      label = NULL,
                                                      choices = lang,
                                                      selected = "en")),
                  conditionalPanel("input.anyLanguage == true",
                                   shiny::checkboxInput(inputId = "colourLanguage",
                                                        label = "A different colour for each language?",
                                                        value = TRUE))
                  ,
                  uiOutput(outputId = "hashtags_UI"),
                  uiOutput(outputId = "trendingHashtagsTitle"),
                  uiOutput(outputId = "trendingHashtags"),
                  conditionalPanel("input.anyLanguage == false",
                                   shiny::radioButtons(inputId = "sentimentL",
                                                       label = "Type of wordcloud",
                                                       choices = c("Unified",
                                                                   "Sentiment by tweet",
                                                                   "Sentiment by word"),
                                                       inline = TRUE)
                  )
                ),
                
                #### Box 3: Wordcloud filters ####
                
                tabBox(title = "Search and filter",
                       id = "wordcloud_filters",
                       tabPanel("By string",
                                textInput(inputId = 'string', label = NULL)
                                # splitLayout(textInput(inputId = 'string', label = NULL),
                                #             #actionButton("filter", "Filter", icon = icon("filter", class = "font-awesome")),
                                #             cellWidths = c("75%", "25%"))
                       )
                       ,
                       tabPanel("By EP group",
                                shiny::checkboxGroupInput(inputId = "EPgroup",
                                                          label = "Choose group",
                                                          choices = EPGroupShort,
                                                          inline = TRUE)
                                #,actionButton("filterByGroup", "Filter", icon = icon("filter", class = "font-awesome"))
                       ),
                       tabPanel("By MEP", shiny::uiOutput(outputId = "MEPfilter_UI")),
                       tabPanel("By country", shiny::uiOutput(outputId = "country_filter_UI"))
                       #  ,
                       # tabPanel("By sentiment", shiny::checkboxGroupInput(inputId = "sentimentFilter",
                       #                                                      label = "Choose sentiment",
                       #                                                      choices = as.list(c("Positive", "Negative", "Neutral")),
                       #                                                      inline = TRUE))
                ), 
                box(actionButton(inputId = "reset",
                                 label = "Reset filters",
                                 icon = icon(name = "recycle", lib = "font-awesome")),
                    bookmarkButton(label = "Get direct link with current settings")
                ),
                
                #### Box 4: infobox ####
                box(title = NULL,
                    shiny::htmlOutput(outputId = "HeaderInfoBox"),
                    infoBoxOutput(outputId = "TweetsNr"),
                    infoBoxOutput(outputId = "MEPsNr"),
                    infoBoxOutput(outputId = "DaysNr"),width = 12
                )
                ,
                #### Box 5: table ####
                fluidRow(DT::dataTableOutput(outputId = "table"))
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    if (dataset_global_file_time < fs::file_info(path = fs::path("qf_data", "tweets_processed", "dataset.rds")) %>% 
        dplyr::pull(modification_time)) {
      
      dataset <- readRDS(file = file.path("qf_data", "tweets_processed", "dataset.rds"))
      hashtagsList <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_hashtags_list.rds"))
      trendingHashtags <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_trending_hashtags_list.rds"))
      lang <- readRDS(file = file.path("qf_data", "tweets_processed", "tweets_lang_list.rds"))
      EPGroupShort <- readRDS(file = file.path("qf_data", "tweets_processed", "EPGroupShort.rds"))
      countries <- readRDS(file = file.path("qf_data", "tweets_processed", "countries.rds"))
      palettes <- readRDS(file = file.path("qf_data", "tweets_processed", "palettes.rds"))
      
      langTable <- dplyr::left_join(x = tibble::tibble(lang = unlist(lang)),
                                    y = readRDS(file.path("qf_data", "tweets_processed", "langCode.rds")) %>% rename(lang = alpha2), by = "lang") %>% 
        mutate(English = stringr::str_extract(string = English, pattern = regex("[[:alnum:]]+")))
      
      
      
    }
  

    randomString <- stringi::stri_rand_strings(n=1, length=16)
    
    #### Reset ####
    
    observeEvent(input$reset, {
      updateTextInput(session = session, inputId = "string", value = "")
      updateCheckboxGroupInput(session = session, inputId = "EPgroup", selected = character(0))
      updateSelectizeInput(session = session, inputId = "MEPfilter", selected = character(0))
      updateSelectizeInput(session = session, inputId = "selectedHashtag", selected = "All tweets")
      updateSelectizeInput(session = session, inputId = "countryFilter", selected = character(0))
    })
    
    
    
    observeEvent(input$wordcloud_plot, {
      if (input$wordcloud_plot=="EP group comparison") {
        updateTabsetPanel(session = session,
                          inputId = "wordcloud_filters",
                          selected = "By EP group")
        shiny::updateCheckboxGroupInput(session = session,
                                        inputId = "EPgroup",
                                        selected = c("S&D", "EPP"))
      }
    })
    
    #### Reactive ####
    
    currentDataset <- reactive({
      # filter date
      if (input$dateRangeRadio=="Last week") {
        dataset <- dataset %>% 
          filter(date>Sys.Date()-7)
      } else if (input$dateRangeRadio=="Last month") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-31)
      } else if (input$dateRangeRadio=="Last three months") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-91)
      } else {
        dataset <- dataset %>%
          filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
      }
      # filter by language
      if (input$anyLanguage==FALSE) {
        dataset <- dataset %>% 
          filter(lang==input$language)
      }
      
      # filter by country
      if (is.null(input$countryFilter)==FALSE) {
        dataset <- dataset %>% 
          filter(stringr::str_detect(string = country, pattern = paste(input$countryFilter, collapse = "|")))
      }
      
      #filter hashtag
      if(is.null(input$selectedHashtag)){
        
      } else if(input$selectedHashtag=="All tweets") {
        
      } else {
        dataset <- dataset %>% 
          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x))))
      }
      
      if (input$string!="") {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = text, pattern = stringr::regex(pattern = input$string, ignore_case = TRUE)))
      }
      
      if (is.null(input$EPgroup)==FALSE) {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
      }
      
      if (is.null(input$MEPfilter)==FALSE) {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = screen_name, pattern = paste(input$MEPfilter, collapse = "|")))
      }
      dataset
    })
    
    currentHashtags <- reactive({
      
      if (input$anyLanguage==FALSE) {
        dataset <- dataset %>% 
          filter(lang==input$language)
      }
      
      if (input$dateRangeRadio=="Last week") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-7)
      } else if (input$dateRangeRadio=="Last month") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-31)
      } else if (input$dateRangeRadio=="Last three months") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-91)
      } else {
        dataset <- dataset %>%
          filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
      }
      if (is.null(input$EPgroup)==FALSE) {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
      }
      
      currentHashtagsDF <- dataset %>%
        select(screen_name, hashtags) %>%
        unnest() %>%
        na.omit() %>% 
        group_by(hashtags) %>%
        add_count(sort = TRUE) %>% 
        rename(nTotalOrig = n) %>% 
        mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
        group_by(hashtagsLower) %>%
        add_tally() %>%
        ungroup() %>% 
        rename(nTotal = n) %>% 
        group_by(hashtags, nTotal) %>% 
        distinct(screen_name, .keep_all = TRUE) %>% 
        add_count() %>% 
        rename(nMepPerHashtag = n) %>% 
        select(-screen_name) %>% 
        arrange(desc(nMepPerHashtag), desc(nTotal)) %>% 
        ungroup() %>% 
        distinct(hashtagsLower, .keep_all = TRUE) %>% 
        mutate(hashtagString = paste0("#", hashtags, " (", nMepPerHashtag, " MEPs, ", nTotal, " tweets)"))
      currentHashtagsList <- as.list(currentHashtagsDF$hashtags)
      names(currentHashtagsList) <- currentHashtagsDF$hashtagString
      
      currentHashtagsList
    })
    
    currentTrendingHashtags <- reactive({
      if (input$anyLanguage==TRUE)  {
        as.character(trendingHashtags$AnyLanguage)
      } else {
        as.character(unlist(trendingHashtags[names(trendingHashtags)==input$language]))
      }
    })
    
    currentMEPs <- reactive({
      
      if (input$dateRangeRadio=="Last week") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-7)
      } else if (input$dateRangeRadio=="Last month") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-31)
      } else if (input$dateRangeRadio=="Last three months") {
        dataset <-  dataset %>% 
          filter(date>Sys.Date()-91)
      } else {
        dataset <- dataset %>%
          filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
      }
      
      if (is.null(input$EPgroup)==FALSE) {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
      }
      
      # filter language
      if (input$anyLanguage==FALSE) {
        dataset <- dataset %>% 
          filter(lang==input$language)
      }
      
      #filter hashtag
      if(is.null(input$selectedHashtag)){
        
      } else if(input$selectedHashtag=="All tweets") {
        
      } else {
        dataset <- dataset %>% 
          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x))))
      }
      
      if (input$string!="") {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = text, pattern = stringr::regex(pattern = input$string, ignore_case = TRUE)))
      }
      
      if (is.null(input$EPgroup)==FALSE) {
        dataset <- dataset %>%
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
      }
      
      temp <- dataset %>% distinct(fullName, screen_name) %>% filter(is.na(fullName)==FALSE) %>% arrange(fullName)
      currentMEPsList <- structure(as.list(temp$screen_name), names = as.character(temp$fullName))
      currentMEPsList
      
    })
    
    
    #### UI ####
    
    output$warning <- renderUI({
      if (input$sentimentL=="Sentiment by tweet") {
        HTML(text = "<b>Warning</b>: sentiment by tweet currently works with English tweets only")
      }
    })
    
    output$hashtags_UI <- renderUI({
      shiny::selectizeInput(inputId = "selectedHashtag",
                            label = "Select hashtag",
                            choices = c(list("All tweets"),
                                        currentHashtags()))
    })
    
    output$MEPfilter_UI <- renderUI({
      shiny::selectizeInput(inputId = "MEPfilter", label = "Filter by MEP",
                            choices = currentMEPs(),
                            multiple = TRUE)
    })
    
    output$country_filter_UI <- renderUI({
      shiny::selectizeInput(inputId = "countryFilter", label = "Filter by country",
                            choices = as.list(unique(dataset$country) %>% sort()),
                            multiple = TRUE)
    })
    
    output$MaxWords_UI <- renderUI({
      shiny::sliderInput(inputId = "MaxWords",
                         label = "Maximum number of words in the wordcloud",
                         min = 1, 
                         max = 1000
      )
    })
    
    output$colourMost_UI <- renderUI({
      if (input$sentimentL=="Sentiment by word") {
        colourInput(inputId = "colourPositive", label = "Colour for positive terms", value = "#00BFC4", showColour = "both")
      } else if (input$anyLanguage==TRUE&input$colourLanguage==TRUE) {
        pickerInput(
          inputId = "multilingual_palette", label = "Choose a palette:",
          choices = palettes$colors_pal, selected = "Set1", width = "200%",
          choicesOpt = list(
            content = sprintf(
              "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
              unname(palettes$background_pals), palettes$colortext_pals, names(palettes$background_pals)
            )
          )
        )
      } else {
        colourInput(inputId = "colourMost", label = "Colour for most frequent terms", value = "#08306B", showColour = "both")
      } 
    })
    
    output$colourLeast_UI <- renderUI({
      if (input$sentimentL=="Sentiment by word") {
        colourInput(inputId = "colourNegative", label = "Colour for negative terms", value = "#F8766D", showColour = "both")
      } else if (input$anyLanguage==TRUE&input$colourLanguage==TRUE) {
        # leave empty
      }
      else {
        colourInput(inputId = "colourLeast", label = "Colour for least frequent terms", value = "#4292C6", showColour = "both")  
      }
    })
    
    observe({
      updateActionButton(session = session, inputId = "trendingHashtag_1", label = currentTrendingHashtags()[1])
      updateActionButton(session = session, inputId = "trendingHashtag_2", label = currentTrendingHashtags()[2])
      updateActionButton(session = session, inputId = "trendingHashtag_3", label = currentTrendingHashtags()[3])
      updateActionButton(session = session, inputId = "trendingHashtag_4", label = currentTrendingHashtags()[4])
      updateActionButton(session = session, inputId = "trendingHashtag_5", label = currentTrendingHashtags()[5])
      updateActionButton(session = session, inputId = "trendingHashtag_6", label = currentTrendingHashtags()[6])
      updateActionButton(session = session, inputId = "trendingHashtag_7", label = currentTrendingHashtags()[7])
      updateActionButton(session = session, inputId = "trendingHashtag_8", label = currentTrendingHashtags()[8])
    })
    output$trendingHashtagsTitle <- renderUI({
      if (length(currentTrendingHashtags())>0) {
        HTML("<b>Trending hashtags</b><br />")
      }
    })
    
    output$trendingHashtags <- renderUI({
      lapply(seq_along(currentTrendingHashtags()[1:8]), function(x){
        do.call(actionButton, list(inputId = paste("trendingHashtag", x, sep = "_"), label = currentTrendingHashtags()[x]))
      }
      )
    })
    
    observeEvent(eventExpr = input$trendingHashtag_1, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[1], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_2, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[2], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_3, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[3], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_4, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[4], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_5, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[5], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_6, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[6], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_7, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[7], pattern = "#"))])
    })
    observeEvent(eventExpr = input$trendingHashtag_8, {
      updateSelectizeInput(session = session,
                           inputId = "selectedHashtag",
                           selected = currentHashtags()[tolower(as.character(unlist(currentHashtags())))==tolower(stringr::str_remove(string = currentTrendingHashtags()[8], pattern = "#"))])
    })
    #### Subset date range ####
    
    observe({
      if (input$dateRangeRadio=="Last week") {
        startDate <- Sys.Date()-7
      } else if (input$dateRangeRadio=="Last month") {
        startDate <- Sys.Date()-31
      } else if (input$dateRangeRadio=="Last three months") {
        startDate <- Sys.Date()-91
      }
      
      if (input$dateRangeRadio=="Custom range") {
        updateDateRangeInput(session, "dateRange",
                             start = min(dataset$date),
                             end = Sys.Date())
      }
    })
    
    
    #### Wordcloud ####
    
    output$wordcloud <- renderPlot(expr = {
      # reload if dateRange or hashtag is changed
      input$dateRange
      input$dateRangeRadio
      input$selectedHashtag
      
      # If tab is "By hashtag"
      if (input$wordcloud_filters=="By hashtag") {
        if (is.null(input$selectedHashtag)==FALSE) {
          par(mar = rep(0, 4))
          if (input$selectedHashtag=="All tweets") {
            temp <- dataset %>% 
              filter(lang==input$language) %>% 
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              # remove stopwords, if list for the relevant language is available, otherwise do nothing
              when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                     anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                   ~ .)
          } else {
            temp <- dataset %>% 
              filter(lang==input$language) %>% 
              filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x)))) %>%
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              # remove stopwords, if list for the relevant language is available, otherwise do nothing
              when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                     anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                   ~ .)
          }
          if (input$sentimentL=="Sentiment by word") {
            # for log
            message(paste(Sys.time(), "WordcloudSentimentCreated", input$language, sep = "-"))
            par(mar = rep(0, 4))
            temp %>% 
              # nrc sentiment, removing words that are both positive and negative
              inner_join(y = syuzhet::get_sentiment_dictionary(dictionary = "nrc",
                                                               lang = langTable %>% filter(lang=="en") %>% pull(English) %>% tolower()) %>%
                           dplyr::filter(sentiment=="negative"|sentiment=="positive") %>% add_count(word) %>% filter(n==1) %>% select(-n), by = "word") %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                               max.words = 100, scale = c(3, 1), family = "Carlito", font = 1)
          } else {
            # for log
            message(paste(Sys.time(), "WordcloudUnifiedCreated", input$language, sep = "-"))
            par(mar = rep(0, 4))
            temp %>% 
              count(word) %>% 
              with(wordcloud(word, n, scale = c(3, 1), max.words = 100, min.freq = 1,
                             random.order = FALSE, family = "Carlito", font = 1, colors = pal))
            
          }
        }
        ##### By EP Group #####
      } else if (input$wordcloud_filters=="By EP group") {
        par(mar = rep(0, 4))
        temp <- dataset %>% 
          filter(lang==input$language) %>% 
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|"))) %>%
          select(clean_text, country, GroupShort) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          # remove stopwords, if list for the relevant language is available, otherwise do nothing
          when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                 anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
               ~ .)
        if (length(input$EPgroup)==1) {
          
        } else if (length(input$EPgroup)>1) {
          par(mar = rep(0, 4))
          temp %>% 
            count(word, GroupShort, sort = TRUE) %>% 
            acast(word ~ GroupShort, value.var = "n", fill = 0) %>%
            comparison.cloud(
              #colors = c("#F8766D", "#00BFC4"),
              max.words = 100, scale = c(3, 1), family = "Carlito", font = 1)
        }
      }
    }, execOnResize = TRUE)
    
    #### Wordcloud 2 ####
    
    wc2 <- reactive({
      
      if (is.null(input$colourMost)==FALSE) {
        createPalette <- colorRampPalette(colors = c(input$colourMost, input$colourLeast))
      } else if(input$anyLanguage==TRUE&input$colourLanguage==TRUE) {
        ### 
      } else {
        createPalette <- colorRampPalette(colors = c("#08306B", "#4292C6"))
      }
      
      if (is.null(input$selectedHashtag)==FALSE) {
        
        ##### Wordcloud2 sentiment or unified #####
        if (input$sentimentL=="Sentiment by word") {
          if (input$language=="en") {
            sentimentDictionary <- tidytext::get_sentiments("bing")
          } else {
            sentimentDictionary <-
              syuzhet::get_sentiment_dictionary(dictionary = "nrc",
                                                lang = langTable %>%
                                                  filter(lang==input$language) %>%
                                                  pull(English) %>%
                                                  tolower()) %>%
              filter(sentiment=="negative"|sentiment=="positive") %>%
              add_count(word) %>%
              filter(n==1) %>%
              select(word, sentiment)
          }
          
          dataset <- currentDataset() %>% 
            select(clean_text) %>% 
            unnest_tokens(input = clean_text, output = word) %>% 
            # remove stopwords, if list for the relevant language is available, otherwise do nothing
            when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "snowball")) ~
                   anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "snowball")), by = "word"),
                 is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                   anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                 ~ .) %>% 
            # nrc sentiment, removing words that are both positive and negative
            inner_join(y = sentimentDictionary, by = "word") %>%
            count(word, sentiment, sort = TRUE) %>% 
            slice(1:input$MaxWords) %>% 
            mutate(colour = if_else(condition = sentiment=="negative",
                                    true = if (is.null(input$colourNegative)) "#F8766D" else input$colourNegative,
                                    false = if (is.null(input$colourPositive)) "#00BFC4" else input$colourPositive)) %>%
            select(-sentiment)
          
        } else if (input$sentimentL=="Sentiment by tweet") {
          
          dataset <- currentDataset() %>% 
            select(clean_text) %>%
            mutate(sentiment = syuzhet::get_sentiment(char_v = clean_text, language = input$language)) %>% 
            # remove neutral
            filter(sentiment!=0) %>% 
            mutate(sentiment = if_else(condition = sentiment>0, true = "positive", false = "negative")) %>% 
            tidytext::unnest_tokens(input = clean_text, output = word) %>% 
            # remove stopwords, if list for the relevant language is available, otherwise do nothing
            when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "snowball")) ~
                   anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "snowball")), by = "word"),
                 is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                   anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                 ~ .) %>% 
            count(word, sentiment, sort = TRUE) %>% 
            slice(1:input$MaxWords) %>% 
            mutate(colour = if_else(condition = sentiment=="negative",
                                    true = if (is.null(input$colourNegative)) "#F8766D" else input$colourNegative,
                                    false = if (is.null(input$colourPositive)) "#00BFC4" else input$colourPositive)) %>%
            select(-sentiment)
          
        } else {
          
          if (input$anyLanguage==TRUE) {
            dataset <- currentDataset() %>% 
              select(clean_text, lang) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              mutate(stopword = FALSE)
            
            for (i in stopwords::stopwords_getlanguages(source = "stopwords-iso")) {
              if (length(dataset$stopword[dataset$lang==i])!=0) {
                dataset$stopword[dataset$lang==i] <- is.element(el = dataset$word[dataset$lang==i], set = c("via", "retweeted", stopwords::stopwords(language = i, source = "stopwords-iso")))
              }
            }
            if (input$colourLanguage==TRUE) {
              dataset <- dataset %>%
                filter(stopword==FALSE) %>% 
                group_by(lang) %>% 
                count(word, sort = TRUE) %>% 
                add_tally(wt = n) %>% 
                arrange(desc(n)) %>% 
                ungroup() %>% 
                mutate(lang = factor(x = lang, levels = unique(lang)))
              
              dataset$LangRank <- dataset %>% group_indices(., factor(lang, levels = unique(lang)))
              
              if (is.null(input$multilingual_palette)) {
                MaxLang <- 9
              } else {
                MaxLang <- brewer.pal.info$maxcolors[rownames(brewer.pal.info)==input$multilingual_palette]
              }
              
              dataset <- dataset %>% 
                filter(LangRank<(MaxLang+1)) %>% 
                ungroup() %>% 
                top_n(n = input$MaxWords, wt = n) %>% 
                arrange(LangRank) %>% 
                left_join(data_frame(LangRank = if (is.null(input$multilingual_palette)) 1:9 else if(brewer.pal.info$category[rownames(brewer.pal.info)==input$multilingual_palette]=="seq") MaxLang:1 else 1:MaxLang, colour = brewer.pal(n = MaxLang, name = if (is.null(input$multilingual_palette)) "Set1" else input$multilingual_palette)), by = "LangRank") %>% 
                select(word, n, colour) %>% 
                arrange(desc(n))
              
              
            } else if (input$colourLanguage==FALSE) {
              dataset <- dataset %>%
                filter(stopword==FALSE) %>% 
                count(word, sort = TRUE) %>%
                slice(1:input$MaxWords) %>% 
                mutate(colour = createPalette(n()))
            }
          } else {
            dataset <- currentDataset() %>%
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              # remove stopwords, if list for the relevant language is available, otherwise do nothing
              when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                     anti_join(., data_frame(word = c("via", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
                   ~ .) %>% 
              count(word, sort = TRUE) %>%
              slice(1:input$MaxWords) %>% 
              mutate(colour = createPalette(n()))
          }
        }
        
        # add for log
        
        if (input$sentimentL=="Sentiment by word") {
          message(paste(Sys.time(), "Wordcloud2SentimentCreated", input$language, sep = "-"))
        } else {
          message(paste(Sys.time(), "Wordcloud2UnifiedCreated", input$language, sep = "-"))
        }
        
        dataset 
      }
      
      
    })
    
    output$wordcloud2 <- renderWordcloud2(
      if (is.null(wc2())==FALSE) wc2() %>% wordcloud2(size = if (is.null(input$sizeVarWC2)) 0.5 else input$sizeVarWC2,
                                                      color = wc2()$colour))
    
    #### Barcharts ####
    
    output$barchartGG <- renderPlot({
      currentDataset() %>%
        select(clean_text) %>% 
        unnest_tokens(input = clean_text, output = word) %>% 
        # remove stopwords, if list for the relevant language is available, otherwise do nothing
        when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
               anti_join(., data_frame(word = c("via", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
             ~ .) %>% 
        count(word, sort = TRUE) %>% 
        head(input$MaxWordsInBarchart) %>% 
        arrange(n) %>% 
        mutate(word = forcats::as_factor(word)) %>% 
        ggplot(mapping = aes(x = word, y = n, fill = "singlecolor")) +
        geom_col() +
        scale_x_discrete("") +
        scale_y_continuous("Number of occurrences") +
        scale_fill_manual(values = "#08306B") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14,face="bold")) +
        guides(fill=FALSE)
    })
    
    
    #### Comparisons ####
    
    output$barchartComparisonGG <- renderPlot({
      
      if (is.null(input$EPgroup)==FALSE&length(input$EPgroup)<5&length(input$EPgroup)>1) {
        tidy_tweets <- currentDataset() %>%
          select(clean_text, GroupShort) %>% 
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|"))) %>% 
          unnest_tokens(output = word, input = clean_text) %>% 
          # remove stopwords, if list for the relevant language is available, otherwise do nothing
          when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                 anti_join(., data_frame(word = c("via", "retweeted", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
               ~ .)
        
        if (input$wcOrBarchartComparison == "Comparison barchart") {
          if (is.null(input$EPgroup)==FALSE&length(input$EPgroup)==2) {
            temp <- tidy_tweets %>% 
              count(word, GroupShort) %>%
              filter(sum(n) >= 5) %>%
              ungroup() %>%
              spread(GroupShort, n, fill = 0) %>%
              mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
              mutate(logratio = log(.[[2]] / .[[3]])) %>%
              arrange(desc(logratio)) %>% 
              group_by(logratio < 0) %>%
              top_n(10, abs(logratio)) %>%
              slice(1:10) %>% 
              ungroup() %>%
              mutate(word = reorder(word, logratio))
            
            temp %>% 
              ggplot(aes(word, logratio, fill = logratio < 0)) +
              geom_col() +
              coord_flip() +
              ylab("Log odds ratio") +
              scale_x_discrete("") +
              scale_fill_discrete(name = "", labels = colnames(temp)[2:3]) +
              theme_minimal() +
              theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=14)) 
          } else {
            ggplot() +
              labs(title = "Comparison barchart requires two EP groups") +
              theme_void() +
              theme(plot.title = element_text(colour = "#7F3D17"))
          }
          
        } else {
          temp <- tidy_tweets %>% 
            group_by(word, GroupShort) %>% 
            count() %>% 
            ungroup() %>%
            spread(GroupShort, n, fill = 0)
          
          rownames(temp) <- temp$word
          
          par(mar = rep(0, 4))
          
          temp <- temp %>% 
            select(-word)
          
          if (input$wcOrBarchartComparison ==  "Commonality wordcloud") {
            temp %>% 
              commonality.cloud(random.order=FALSE, max.words = 100, scale = c(5, 0.5), family = "Carlito", font = 1)
          } else if (input$wcOrBarchartComparison ==  "Comparison wordcloud") {
            temp %>% 
              comparison.cloud(random.order=FALSE, max.words = 100, scale = c(5, 0.5), family = "Carlito", font = 1)   
          }
        }
        
      }  else {
        ggplot() +
          labs(title = "Minimium 2 and maximum 4 EP groups are allowed") +
          theme_void() +
          theme(plot.title = element_text(colour = "#7F3D17"))
      }
      
      
      
    })
    
    #### Download ####
    
    output$downloadTidyWordCount <- downloadHandler(
      filename = function() {
        paste0("QuoteFinderWordCount", ".csv")
      },
      content = function(file) {
        write_csv(x = currentDataset() %>%
                    select(clean_text) %>% 
                    unnest_tokens(input = clean_text, output = word) %>% 
                    # remove stopwords, if list for the relevant language is available, otherwise do nothing
                    when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                           anti_join(., data_frame(word = c("via", "retweeted", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
                         ~ .) %>% 
                    count(word, sort = TRUE),
                  file)
      }
    )
    
    
    output$downloadPng <- downloadHandler(
      filename = paste0(randomString, ".png"),
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(widget = wc2() %>% wordcloud2(size = input$sizeVarWC2*2,
                                                 color = wc2()$colour), file = paste0(randomString, ".html"), selfcontained = FALSE)
        webshot(url = paste0(randomString, ".html"), file = file, delay = if_else(condition = input$MaxWords>500, true = 5, false = 3), vwidth = 1280, vheight = 960)
      },
      contentType = "image/png")
    
    output$downloadHtml <- downloadHandler(
      filename = paste0(randomString, ".html"),
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(widget = wc2() %>% wordcloud2(size = input$sizeVarWC2*2,
                                                 color = wc2()$colour), file = file, selfcontained = TRUE)
        simpleFixWc2(file, file)
      })
    
    
    #### DataTable ####
    
    output$table <- DT::renderDataTable(expr = {
      DT::datatable(data = currentDataset() %>% 
                      arrange(desc(time))%>%
                      select(screen_name, date, text, Link, GroupShort) %>% 
                      head(10000) %>% 
                      rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"),
                    escape = FALSE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15, 20)), rownames=FALSE)
    }, server = TRUE)
    
    ### InfoBox ####
    
    output$HeaderInfoBox <- renderText({
      
      paste0(paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>;"),
             if (is.null(input$selectedHashtag)==TRUE) (" hashtag: <i>All tweets</i>;") else if (input$selectedHashtag=="All tweets") (" hashtag: <i>All tweets</i>;") else paste0(" hashtag: <i>#", input$selectedHashtag, "</i>;"),
             if (input$string!="") paste0(" string: <i>", input$string, "</i>;"),
             if (is.null(input$EPgroup)==FALSE) paste0(" EP group: <i>", paste(input$EPgroup, collapse = ", "), "</i>;"),
             if (is.null(input$countryFilter)==FALSE) paste0(" Country: <i>", paste(input$countryFilter, collapse = ", "), "</i>;"),
             #" selected word: <i>", gsub(":.*","",input$selected_word), 
             "</div>")
    })
    
    output$TweetsNr <- renderInfoBox({
      infoBox(title = "Tweets",
              value =  point(nrow(currentDataset())),
              icon = icon("twitter"),
              color = "blue"
      )
    })
    
    output$MEPsNr <- renderInfoBox({
      
      infoBox(title = "by",
              value = paste(length(unique(currentDataset()$screen_name)), "MEPs"),
              icon = icon("users"), color = "blue", fill = FALSE)
      
    })
    
    output$DaysNr <- renderInfoBox({
      # reload if dateRange is changed
      input$dateRange
      input$dateRangeRadio
      
      if (input$dateRangeRadio=="Last week") {
        infoBox(title = "posted in", value = paste(7, "days"),
                icon = icon("calendar"), color = "blue", fill = FALSE)
      } else if (input$dateRangeRadio=="Last month") {
        infoBox(title = "posted in", value = paste(31, "days"),
                icon = icon("calendar"), color = "blue", fill = FALSE)
      } else if (input$dateRangeRadio=="Last three months") {
        infoBox(title = "posted in", value = paste(91, "days"),
                icon = icon("calendar"), color = "blue", fill = FALSE)
      } else {
        infoBox(title = "posted in", value = paste(as.Date(input$dateRange[2])-as.Date(input$dateRange[1]), "days"),
                icon = icon("calendar"), color = "blue", fill = FALSE)
      }
      
    })
    
  }, options = options('shiny.port'=shiny_port, 'shiny.host'=shiny_host)
  )
}
