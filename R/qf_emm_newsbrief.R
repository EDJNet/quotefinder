#' Get current news in multiple languages from EMM newsbrief
#'
#' Imports and cleans RSS files from [http://emm.newsbrief.eu/]
#'
#' @param languages A character vector of one or more languages as two letter
#'   codes (e.g. "en"). Defaults to "en", as the previous default, "all", is not
#'   any more available. Current available languages are:
#'   `c("ar","bg","cs","da","de","el","en","es", "et","fi","fr","hr", "hu","it",
#'   "lt","lv","nl","pl","pt","ro","ru", "sk","sl","sv","sw","tr","zh")`
#' @param category_id A character vector of length 1, defaults to "rtn" (default
#'   category with all contents). The id for separate categories can be derived
#'   from the URL of the given section, e.g. for EC news the id is "ECnews".
#' @param shuffle Logical, defaults to TRUE. If TRUE, order in which languages
#'   are processed is randomised.
#' @param keep_xml Logical, defaults to FALSE. If TRUE, removes the xml file
#'   downloaded from EMM newsbrief.
#' @param store Logical, defaults to TRUE. If TRUE, it stores the data extracted
#'   from the xml as an rds file in a local subfolder, organised by language and
#'   date.
#' @param store_only_new Logical, defaults to TRUE. Before storing outputs, it
#'   checks the previous file and keeps only articles published after the most
#'   recent article incldued in the previous post.
#' @return Nothing, used for its side effects.
#' @examples
#'
#' @export
qf_get_emm_newsbrief <- function(languages = "en",
                                 category_id = "rtn",
                                 keep_xml = TRUE,
                                 shuffle = TRUE,
                                 store = TRUE, 
                                 store_only_new = TRUE) {
  
  if (shuffle==TRUE) {
    languages <- sample(languages)
  }
  
  for (i in languages) {
    
    base_path <- fs::path("emm_newsbrief", 
                          i,
                          as.character(Sys.Date()))
    
    fs::dir_create(path = base_path, recurse = TRUE)

    
    if (category_id == "rtn") {
      xml_location <- fs::path(base_path,
                               paste0(Sys.time(), "-emm_rtn_", i, ".xml"))
      
      download.file(url = paste0("https://emm.newsbrief.eu/rss/rss?type=rtn&language=", i, "&duplicates=false"),
                    destfile = xml_location)
      
    } else {
      xml_location <- fs::path(base_path,
                               paste0(Sys.time(), "-", category_id, "-", i, ".xml"))
      download.file(url = paste0("https://emm.newsbrief.eu/rss/rss?type=category&id=", category_id, "&language=", i, "&duplicates=false"),
                    destfile = xml_location)
    }

    
    xml2_list <- xml2::read_xml(xml_location) %>%
      xml2::xml_find_all(xpath = "//item") %>%
      xml2::as_list()
    
    nrows_rss <- length(xml2_list)
    
    if (nrows_rss>0) {
      rss_df <- tibble::tibble(title = character(nrows_rss),
                               language = character(nrows_rss),
                               link = character(nrows_rss),
                               keywords = character(nrows_rss),
                               pubDate = as.POSIXct(x = rep(x = NA, nrows_rss)),
                               source = character(nrows_rss), 
                               entity = list(nrows_rss))
      
      for (j in 1:nrows_rss) {
        temp <- xml2_list %>% magrittr::extract2(j)
        
        current_title <- temp$title %>% as.character()
        
        if (length(current_title)==0) {
          current_title <- ""
        }
        
        rss_df$title[j] <- current_title
        rss_df$language[j] <- temp$language %>% as.character()
        rss_df$link[j] <- temp$link %>% as.character()
        rss_df$keywords[j] <- temp$keywords %>% as.character()
       # rss_df$description[j] <- temp$description %>% as.character()
        rss_df$pubDate[j] <- temp$pubDate %>% as.character() %>% base::strptime("%a, %d %b %Y %H:%M:%S %z")
        rss_df$source[j] <- temp$source %>% as.character()
        entL <- temp[names(temp)=="entity"] 
        rss_df$entity[[j]] <- tibble::tibble(id = purrr::map(.x = entL, .f = attr, which = "id") %>% as.numeric, 
                                             name = purrr::map(.x = entL, .f = attr, which = "name") %>% as.character())
      }
      
      if (keep_xml == FALSE) {
        fs::file_delete(path = xml_location)
      }
      
      if (store == TRUE) {
        if (store_only_new == TRUE) {
          #check the most recent post of the previous stored file
          
          link_to_previous <- fs::dir_info(path = fs::path("emm_newsbrief", i),
                       recurse = TRUE,
                       glob = "*.rds") %>% 
            dplyr::arrange(dplyr::desc(modification_time)) %>% 
            dplyr::slice(1) %>% 
            dplyr::pull(path)
           if (length(link_to_previous)>0) {
             for (k in 1:102) {
               previous <- readr::read_rds(file = fs::dir_info(path = fs::path("emm_newsbrief", i),
                                                               recurse = TRUE,
                                                               glob = "*.rds") %>% 
                                             dplyr::arrange(dplyr::desc(modification_time)) %>% 
                                             dplyr::slice(k) %>% 
                                             dplyr::pull(path))
               if(nrow(previous)>0) {
                 break
               }
               if(k==102) {
                 warning("There are over empty 100 files stored from the given feed, you may want to check if there is any issue.")
               }
             }
             
             max_previous_time <- previous %>% 
               dplyr::pull(pubDate) %>% 
               max()
             
             saveRDS(object = dplyr::anti_join(x = rss_df %>% 
                                                 dplyr::filter(pubDate>=max_previous_time),
                                               y = previous %>% 
                                                 dplyr::filter(pubDate>=max_previous_time),
                                               by = "link") %>% 
                       dplyr::distinct(link, .keep_all = TRUE),
                     file = xml_location %>%
                       stringr::str_replace(pattern = stringr::fixed(".xml"),
                                            replacement = ".rds"))
           } else {
             saveRDS(object = rss_df,
                     file = xml_location %>%
                       stringr::str_replace(pattern = stringr::fixed(".xml"),
                                            replacement = ".rds"))
           }

        } else {
          saveRDS(object = rss_df,
                  file = xml_location %>%
                    stringr::str_replace(pattern = stringr::fixed(".xml"),
                                         replacement = ".rds"))
        }
      }
    }
  }
}

#' Extract most frequently used words from news titles
#'
#' @param language A character vector of language two letter codes. Defaults to
#'   NULL. If NULL, processes available languages.
#' @param n An integer. Number of words to keep. Defaults to `Inf` (i.e. keeps
#'   all words, ordered by frequency).
#' @param date Only news downloaded in the given date will be considered.
#'   Defaults to current day. To get data for the previous day, use
#'   `Sys.Date()-1`
#' @return A data.frame (a tibble) with `n` number of rows and two columns,
#'   `words` and `n` for number of occurrences.
#' @examples
#'
#' @export
qf_emm_extract_keywords <- function(language = NULL,
                                    date = NULL,
                                    n = Inf,
                                    store = TRUE) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("emm_newsbrief"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  
  for (i in language) {
    
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("emm_newsbrief", i),
                         recurse = FALSE,
                         type = "directory") %>% 
        fs::path_file()
    }
    
    for (j in date) {
      all_rds <- list.files(path = fs::path("emm_newsbrief",
                                            i,
                                            as.character(j)),
                            pattern = paste0(i, "\\.rds"),
                            full.names = TRUE)  
      if (length(all_rds)>0) {
        all_news <- suppressMessages(purrr::map_df(.x = all_rds,
                                                   .f = readr::read_rds)) %>% #unisce i file in un singolo data frame
          dplyr::distinct(link, .keep_all = TRUE)
        
        keywords <- all_news %>% 
          dplyr::transmute(title, Date = as.Date(pubDate)) %>%
          tidytext::unnest_tokens(input = title,
                                  output = "words") %>% 
          dplyr::anti_join(tibble::tibble(words = stopwords::stopwords(language = i,
                                                                       source = "stopwords-iso")),
                           by = "words") %>% # elimina stopwords
          dplyr::filter(!stringr::str_detect(string = words,
                                             pattern = "[[:digit:]]")) %>%  # togliere i numeri registrati come parole
          dplyr::group_by(words) %>% 
          dplyr::count(sort = TRUE) %>% 
          dplyr::ungroup() %>% 
          head(n) 
        
        if (store==TRUE) {
          keywords_base_path <- fs::path("keywords", 
                                         i)
          
          fs::dir_create(path = keywords_base_path)
          saveRDS(object = keywords,
                  file = fs::path(keywords_base_path,
                                  paste0(j,
                                         "-keywords_",
                                         n,
                                         "_",
                                         i,
                                         ".rds")))
        }
      }
    }
  }
  keywords
}



#' Extract most frequently used entities in emm newsbrief items
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param days An integer. How many days back in time should be considered, starting from today (0 means today only, 1 means yesterday, etc.)
#' @param date Only news downloaded in the given date will be considered. Defaults to all available dates.
#' @param n An integer. Number of entities to keep. Defaults to `Inf` (i.e. keeps all entities, ordered by frequency).
#' @return A data.frame (a tibble) with `n` number of rows and two columns, `words` and `n` for number of occurrences.
#' @examples
#' 
#' @export
qf_emm_extract_entities <- function(language = NULL,
                                    days = NULL,
                                    date = NULL,
                                    n = Inf,
                                    store = TRUE) {
  

  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("emm_newsbrief"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  lang_date_combo <-
    purrr::map_dfr(.x = language,
                   .f = function(x) {
                     tibble::tibble(language = x,
                                    date = 
                                      fs::dir_ls(path = fs::path("emm_newsbrief", x),
                                                 recurse = FALSE,
                                                 type = "directory") %>% 
                                      fs::path_file() %>% 
                                      base::as.Date())
                   })
  
  
  if (is.null(date)==FALSE) {
    lang_date_combo <- lang_date_combo %>% 
      dplyr::filter(date == as.Date(date))
  }
  
  if (is.null(days)==FALSE) {
    lang_date_combo <- lang_date_combo %>% 
      dplyr::filter(date >= Sys.Date()-days)
  }
  
  
  all_files <- fs::dir_ls(path = fs::path("emm_newsbrief",
                                          lang_date_combo$language,
                                          lang_date_combo$date),
                          recurse = FALSE,
                          type = "file",
                          glob = "*.rds")
  
  
  all_feeds <- purrr::map_dfr(.x = all_files,
                              .f = function(x) {
                                readr::read_rds(x) %>% 
                                  dplyr::select(pubDate, entity, link, language)
                                },
                              .id = "source") %>% 
    dplyr::distinct(link, .keep_all = TRUE) %>% 
    dplyr::select(-link)
  
  entities <- all_feeds %>%
    dplyr::transmute(date=as.Date(pubDate),
                     language,
                     entity) %>% 
    tidyr::unnest(cols = entity) %>% 
    dplyr::group_by(date, language, id, name) %>% 
    dplyr::count(sort = TRUE) %>% 
    dplyr::ungroup() %>% 
    head(n) 
  entities
}
