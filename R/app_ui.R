#' @import shiny
app_ui <- function() {
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
                                                      lib = "font-awesome"),
                                   badgeLabel = "beta"),
          shinydashboard::menuItem(text = "EU-related trending news",
                                   tabName = "tab_trending_news",
                                   icon = shiny::icon("newspaper",
                                                      lib = "font-awesome"),
                                   badgeLabel = "forthcoming"),
          disable = FALSE,
          id = "tabs"
        )
      ),
      body = shinydashboard::dashboardBody(
        tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
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
                                  shiny::HTML("<b>Customise time series</b>"),
                                  shiny::radioButtons(inputId = "rollingType",
                                                      label = NULL,
                                                      choices = c("Median",
                                                                  "Average")),
                                  shiny::radioButtons(inputId = "freq",
                                                      label = "Type of time series",
                                                      choices = c("Number of mentions per day",
                                                                  "Relative frequency")),
                                  shiny::sliderInput(inputId = "rollingDays",
                                                     label = "Apply rolling average for ... days",
                                                     min = 1,
                                                     max = 31,
                                                     value = 7,
                                                     round = TRUE),
                                  shiny::uiOutput(outputId = "dateRangeInput_UI")),
                                  #shiny::actionButton("go", "Go!")),
                                  shiny::column(width = 9,
                                                shiny::fluidRow(
                                                                shiny::plotOutput("freqPlot")
                                                              )
                                                ),
                                  shiny::fluidRow(DT::dataTableOutput(outputId = "kwic"))
          )
          
        )
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
