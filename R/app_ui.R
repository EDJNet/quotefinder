#' @import shiny
app_ui <- function() {
  available_websites <- unique(castarter_dataset[["website"]])
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = tags$a(href='https://www.europeandatajournalism.eu/', tags$img(src='logo_50_230_w.png'))), # for animated version of the logo, check https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(text = "EU twittersphere explorer",
                                   tabName = "tab_twitter_mep",
                                   icon = shiny::icon("twitter")),
          shinydashboard::menuItem(text = "Quotes from EU institutions",
                                   tabName = "tab_eu_official",
                                   icon = shiny::icon(name = "globe",
                                                      lib = "font-awesome")),
          shinydashboard::menuItem(text = "EU-related trending news",
                                   tabName = "tab_trending_news",
                                   icon = shiny::icon("newspaper",
                                                      lib = "font-awesome"),
                                   badgeLabel = "beta"),
          disable = FALSE,
          id = "tabs"
        )
      ),
      body = shinydashboard::dashboardBody(
        marker::use_marker(),
        waiter::use_waiter(include_js = FALSE),
        shinybusy::add_busy_spinner(spin = "folding-cube", color = "#6B2A8C", height = "120px", width = "120px"),
        tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'),
                  tags$style(
                    ".red{background-color:#FFB8C3;}.blue{background-color:#6ECFEA;}.green{background-color:#a6ce39;}"
                  )),
        shinydashboard::tabItems(
          
          ##### tab_twitter_mep ####
          shinydashboard::tabItem(tabName = "tab_twitter_mep",
                                  #  wc2ClickedWord(cloudOutputId = "wordcloud2", inputId = "selected_word"),
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
                                                        choices = as.list(c("Last week",
                                                                            "Last month",
                                                                            #"Last three months",
                                                                            "Custom range")),
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
                                  fluidRow(DT::dataTableOutput(outputId = "table")),
                                  #### Tweet wall ####
                                  fluidRow(uiOutput(outputId = "selected_tweets_wall"))
          )
          ,
          ##### tab_eu_official ####
          shinydashboard::tabItem(tabName = "tab_eu_official",
                                  shiny::column(width = 3, 
                                  shiny::textInput(inputId = 'term',
                                                   label = 'Search',
                                                   value = "climate, business"),
                                  shinyWidgets::checkboxGroupButtons(
                                    inputId = "website_selector",
                                    label = "Search in:",
                                    choices = available_websites,
                                    selected = available_websites,
                                    individual = TRUE,
                                    checkIcon = list(
                                      yes = icon("ok",
                                                 lib = "glyphicon"),
                                      no = icon("remove",
                                                lib = "glyphicon"))
                                  ),
                                  shiny::HTML("<b>Customise time series</b>"),
                                  shiny::radioButtons(inputId = "rollingType",
                                                      label = NULL,
                                                      choices = c("Average",
                                                                  "Median")),
                                  shiny::radioButtons(inputId = "freq",
                                                      label = "Type of time series",
                                                      choices = c("Number of mentions per day",
                                                                  "Relative frequency")),
                                  shiny::sliderInput(inputId = "rollingDays",
                                                     label = "Apply rolling average for ... days",
                                                     min = 1,
                                                     max = 31,
                                                     step = 2,
                                                     value = 7,
                                                     round = TRUE),
                                  shiny::dateRangeInput(inputId = "dateRange_castarter",
                                                        label = "Date range",
                                                        start = min(castarter_dataset$date),
                                                        end = max(castarter_dataset$date),
                                                        weekstart = 1)),
                                 # shiny::uiOutput(outputId = "dateRangeInput_castarter_UI")),
                                  #shiny::actionButton("go", "Go!")),
                                  shiny::column(width = 9,
                                                shiny::fluidRow(
                                                                shiny::plotOutput("freqPlot",
                                                                                  height = "600px")
                                                              )
                                                ),
                                 shiny::fluidRow(shiny::h3("What are the most frequent words found in all these sentences?")),
                                  shiny::fluidRow(DT::dataTableOutput(outputId = "kwic")),
                                  shiny::fluidRow(shiny::column(width = 3, 
                                                                shiny::sliderInput(inputId = "sizeVarWC2_castarter_eu",
                                                                                   label = "Wordcloud size",
                                                                                   min = 0.1, 
                                                                                   max = 2,
                                                                                   value = 0.5,
                                                                                   sep = "."
                                                                ),
                                                                shiny::sliderInput(inputId = "MaxWords_castarter_eu",
                                                                                   label = "Max number of words",
                                                                                   min = 0L, 
                                                                                   max = 1000L,
                                                                                   value = 200L,
                                                                                   sep = ".", width = "95%"
                                                                ),
                                                                shiny::textAreaInput(inputId = 'wordcloud_eu_castarter_custom_stopwords',
                                                                                     label = 'Words to remove from wordcloud',
                                                                                     value = "eu, european, commission, union, europe")),
                                                  shiny::column(width = 9, 
                                                                wordcloud2Output("wordcloud2_eu_castarter")))
                                  

          ),
          ###### tab_trending_news #########
          shinydashboard::tabItem(tabName = "tab_trending_news",
                                  sidebarLayout(
                                    sidebarPanel = sidebarPanel(
                                      
                                      shiny::fluidRow(
                                        shinyWidgets::pickerInput(
                                          inputId = "emm_language_selector",
                                          label = "Select languages", 
                                          choices = c("af", "ar", "bg", "bs", "ca", "cs", "da", "de", "el", "en", 
                                                      "eo", "es", "et", "fa", "fi", "fr", "ha", "he", "hi", "hr", "hu", 
                                                      "id", "it", "ja", "ka", "kk", "ko", "ku", "lb", "lt", "lv", "mk", 
                                                      "mt", "nl", "pl", "pt", "ro", "ru", "sa", "sk", "sl", "sq", "sr", 
                                                      "sv", "sw", "tr", "uk", "vi", "zh"),
                                          selected = c("en", "de", "fr", "it", "es", "pl", "ro"),
                                          options = list(
                                            `actions-box` = TRUE), 
                                          multiple = TRUE)
                                      ),
                                      shiny::h3("Click on the table below to filter news"),
                                      shiny::fluidRow(
                                        DT::dataTableOutput(outputId = "top_entities_dt"))
                                    ), 
                                    
                                    mainPanel = mainPanel(fluidRow(DT::dataTableOutput(outputId = "emm_table")))
                                  ))
          
        ),
        waiter::show_waiter_on_load(html = waiter::spin_folding_cube())
      ), title = "EDJNet's QuoteFinder",
      skin = "purple"
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'quotefinder')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
