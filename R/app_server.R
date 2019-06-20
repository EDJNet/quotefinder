#' @import shiny
app_server <- function(input, output,session) {
  output$letters <- renderText(paste(head(tweets$screen_name), collapse = ", "))
  output$bla <- shiny::renderText("bla")
  
  
  tsGG <- shiny::eventReactive(input$go, {
    if (input$freq=="Absolute frequency") {
      castarter::ShowAbsoluteTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                dataset = dataset,
                                type = "graph",
                                rollingAverage = input$rollingAverage,
                                startDate = input$dateRange[1],
                                endDate = input$dateRange[2])
      
    } else if (input$freq=="Relative frequency") {
      castarter::ShowRelativeTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                dataset = dataset,
                                type = "graph",
                                rollingAverage = input$rollingAverage,
                                startDate = input$dateRange[1],
                                endDate = input$dateRange[2])
    }
  })
  
  output$freqPlot <- shiny::renderPlot({
    if (input$go==0) {
      
    } else {
      tsGG()
    }
  })
  
  kwic_react <- shiny::eventReactive(input$go, {
    temp <- dataset %>%
      dplyr::filter(date>input$dateRange[1], date<input$dateRange[2]) %>%
      dplyr::filter(stringr::str_detect(string = sentence,
                                        pattern = stringr::regex(ignore_case = TRUE,
                                                                 pattern = paste(as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))), collapse = "|")))) %>%
      dplyr::mutate(Source = paste0("<a target='_blank' href='", link, "'>", title, "</a><br />")) %>%
      dplyr::rename(Sentence = sentence, Date = date) %>%
      dplyr::select(Date, Source, Sentence) %>%
      dplyr::arrange(desc(Date))
    
    if (length(as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))))==1) {
      temp$Sentence <- purrr::map_chr(.x = temp$Sentence,
                                      .f = function (x)
                                        paste(c(rbind(as.character(stringr::str_split(string = x,
                                                                                      pattern = stringr::regex(pattern = as.character(input$term), ignore_case = TRUE), simplify = TRUE)),
                                                      c(paste0("<span style='background-color: #FFFF00'>",
                                                               as.character(stringr::str_extract_all(string = x,
                                                                                                     pattern = stringr::regex(as.character(input$term),
                                                                                                                              ignore_case = TRUE),
                                                                                                     simplify = TRUE)),
                                                               "</span>"), ""))),
                                              collapse = ""))
    }
    temp
  })
  
  # Renders table at the bottom of the main tab
  output$kwic <- DT::renderDataTable(expr = kwic_react(),
                                     server = TRUE,
                  options = list(pageLength = 3,
                                 lengthMenu = c(3, 5, 10, 15, 20)),
                  escape = FALSE,
                  rownames = FALSE)
  
  output$dateRangeInput_UI <- renderUI({shiny::dateRangeInput(inputId = "dateRange",
                                                              label = "Date range",
                                                              start = min(dataset$date),
                                                              end = max(dataset$date),
                                                              weekstart = 1)})

  
  #  tweets_r <- callModule(mod_qf_show_tweets_server, "qf_show_tweets_ui_1", tweets_r)
  # List the first level callModules here
}
