#' Create a single data frame from a vector of Twitter handles
#'
#' @param users A character vector of handles or id of Twitter users.
#' @param lists A character vector of twitter lists, either as IDs or in the form `user/slug``
#' @param since A date, expressed in the form Y-M-D (e.g. "2018-12-31")
#' @param save Logical, defaults to TRUE. If TRUE, the merged data frame is stored in the following location: `tweets_all/tweets.all.rds`
#' @examples
#' 
#' @export


qf_bind_rows_tweets <- function(users = NULL,
                                lists = NULL,
                                since = Sys.Date()-31, 
                                include_rts = FALSE,
                                add_date_column = TRUE,
                                save = TRUE,
                                twitter_token = NULL) {
  
  if (is.null(lists)==FALSE) {
    # make sure all lists are defined by id
    lists <- purrr::map_dbl(.x = lists, .f = function(x) {
      if (stringr::str_detect(string = x, pattern = "/")) {
        extracted <- stringr::str_split(string = x, pattern = "/", simplify = TRUE)
        as.numeric(quotefinder::qf_find_list_id(slug = extracted[[2]],
                                                owner_user = extracted[[1]],
                                                twitter_token = twitter_token))
      } else {
        as.numeric(x)
      }
    }
    )
    tweets_from_lists <- purrr::map_dfr(.x = lists, .f = function(x) {
      available_folders <- fs::dir_ls(path = fs::path("tweets_from_list", x))
      folders_filter_l <- as.Date(fs::path_file(path = available_folders))>since
      list_files_locations <- fs::dir_ls(path = available_folders[folders_filter_l],
                                         recurse = FALSE,
                                         type = "file",
                                         glob = "*.rds")
      purrr::map_dfr(.x = list_files_locations,
                     .f = readRDS)
    })
  } else {
    tweets_from_lists <- NULL
  }
  
  if (is.null(users)==FALSE) {
    available_users_location <- fs::dir_ls(path = fs::path("tweets_by_user"),
                                           recurse = FALSE, 
                                           type = "file", 
                                           glob = "*.rds")
    
    available_users <- fs::path_file(path = available_users_location) %>% 
      fs::path_ext_remove()
    
    
    missing_users <- users[!is.element(el = users, set = available_users)]
    
    if (length(missing_users)>0) {
      warning(paste0("There is no locally stored file with tweets by the following users:\n",
                     paste(missing_users, collapse = ", "), "\n"))  
    }
    
    existing_users_location <- fs::path("tweets_by_user",
                                        paste0(users[is.element(el = users, set = available_users)], ".rds"))
    
    
    if (is.null(date)) {
      tweets_all_users <- purrr::map_dfr(.x = existing_users_location,
                                         .f = function(x) {
                                           readRDS(x)
                                         })
    } else {
      tweets_all_users <- purrr::map_dfr(.x = existing_users_location,
                                         .f = function(x) {
                                           readRDS(x) %>% 
                                             dplyr::filter(created_at>as.POSIXct(as.Date(since)))
                                         })
    }
    
  } else {
    tweets_all_users <- NULL
  }
  
  tweets_all <- dplyr::bind_rows(tweets_from_lists,
                                 tweets_all_users) %>% 
    dplyr::distinct(status_id, .keep_all = TRUE) %>% 
    dplyr::filter(is_retweet == include_rts | is_retweet == FALSE)
  
  if (add_date_column == TRUE) {
    tweets_all <- tweets_all %>% 
      dplyr::mutate(date = as.Date(created_at))
  }
  
  if (save==TRUE) {
    fs::dir_create(path = "tweets_processed")
    readr::write_rds(x = tweets_all,
                     path = fs::path("tweets_processed", "tweets_all.rds"))
    message(paste("\nTweets have been saved in", sQuote(fs::path("tweets_processed", "tweets_all.rds"))))
  }
  return(tweets_all)
}

#' Create a list of available languages (tipically, to be used in a shiny app)
#' 
#' @export

qf_create_language_list <- function() {
  tweets_all <- readRDS(file = fs::path("tweets_processed", "tweets_all.rds"))
  lang <- tibble::tibble(lang = unlist(tweets_all$lang)) %>%
    tidyr::drop_na() %>%
    dplyr::count(lang, sort = TRUE) %>% 
    dplyr::select(lang)
  lang_list <- as.list(lang$lang)
  fs::dir_create(path = "tweets_processed")
  readr::write_rds(x = lang_list,
                   path = fs::path("tweets_processed", "tweets_lang_list.rds"))  
  message(paste("\nOrdered language list has been saved in ", sQuote(fs::path("tweets_processed", "tweets_lang_list.rds"))))
}


#' Create an ordered list of hashtags (tipically, to be used in a shiny app)
#' 
#' @export
#' 
qf_create_hashtag_list <- function() {
  tweets_all <- readRDS(file = fs::path("tweets_processed", "tweets_all.rds"))
  
  lang_list <-  readRDS(file = fs::path("tweets_processed", "tweets_lang_list.rds"))
  
  hashtags <- vector("list", length = length(lang_list))
  hashtags <- setNames(object = hashtags, nm = unlist(lang_list))
  for (i in seq_along(lang_list)) {
    tempL <- tibble::tibble(hashtags = tweets_all %>%
                              dplyr::filter(lang==lang_list[[i]]) %>%
                              dplyr::select(hashtags) %>% 
                              unlist()) %>% 
      tidyr::drop_na()  %>%
      dplyr::count(hashtags, sort = TRUE) %>% # make hashtags in order of most frequent, by language
      dplyr::mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
      dplyr::group_by(hashtagsLower) %>%
      dplyr::add_tally(wt = n) %>%
      dplyr::distinct(hashtagsLower, .keep_all = TRUE) %>%
      dplyr::arrange(desc(n)) %>% 
      dplyr::ungroup() %>%
      dplyr::select(hashtags) %>% 
      dplyr::pull(hashtags) %>%
      as.list()
    if (length(tempL) == 0) {
      names(tempL) <- NULL
    } else {
      names(tempL) <- paste0("#", unlist(tempL))
    }
    hashtags[[i]] <- tempL
  }
  # Hashtags any Language
  hashtagsAnyLanguage <- tibble::tibble(hashtags = tweets_all %>%
                                          dplyr::select(hashtags) %>% 
                                          unlist()) %>% 
    tidyr::drop_na()  %>%
    dplyr::count(hashtags, sort = TRUE) %>% # make hashtags in order of most frequent, by language
    dplyr::mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
    dplyr::group_by(hashtagsLower) %>%
    dplyr::add_tally(wt = n) %>%
    dplyr::distinct(hashtagsLower, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::select(hashtags) %>% 
    dplyr::pull(hashtags) %>%
    as.list()
  names(hashtagsAnyLanguage) <- paste0("#", unlist(hashtagsAnyLanguage))
  hashtags$AnyLanguage <- hashtagsAnyLanguage
  readr::write_rds(x = hashtags, path = fs::path("tweets_processed", "tweets_hashtags_list.rds"))
  message(paste("\nOrdered hashtag list has been saved in ", sQuote(fs::path("tweets_processed", "tweets_hashtags_list.rds"))))
}


#' Create an ordered list of hashtags (tipically, to be used in a shiny app)
#' 
#' @param recent_days Defaults to 7. Tweets posted within the given number of days will be considered "recent", and trending hashtags will be chosen based on relative popularity compared with older tweets.
#' 
#' @export
#' 
qf_create_trending_hashtag_list <- function(recent_days = 7) {
  tweets_all <- readRDS(file = fs::path("tweets_processed", "tweets_all.rds"))
  
  lang_list <-  readRDS(file = fs::path("tweets_processed", "tweets_lang_list.rds"))
  
  hashtags <- readRDS(file = fs::path("tweets_processed", "tweets_hashtags_list.rds"))
  
  trending_hashtags <- vector("list", length = length(lang_list))
  trending_hashtags <- setNames(object = trending_hashtags, nm = unlist(lang_list))
  for (i in seq_along(lang_list)) {
    currentDatasetPre <- tweets_all %>% 
      dplyr::filter(is.na(hashtags)==FALSE) %>%
      dplyr::filter(lang==lang_list[[i]])
    
    if(nrow(currentDatasetPre)>0) {
      tempL <- currentDatasetPre %>% 
        dplyr::select(date, hashtags) %>% 
        tidyr::unnest(cols = c(hashtags)) %>% 
        dplyr::mutate(hashtags = tolower(hashtags)) %>% 
        dplyr::mutate(NewOld = dplyr::if_else(condition = date>=as.Date(Sys.Date()-recent_days),
                                              true = "New",
                                              false = "Old")) %>% 
        dplyr::count(hashtags, NewOld) %>% 
        dplyr::ungroup() %>%
        tidyr::spread(NewOld, n, fill = 0) 
      
      
      currentHashtagsDF <- currentDatasetPre %>%
        dplyr::select(screen_name, hashtags) %>%
        tidyr::unnest(cols = c(hashtags)) %>%
        na.omit() %>% 
        dplyr::group_by(hashtags) %>%
        dplyr::add_count(sort = TRUE) %>% 
        dplyr::rename(nTotalOrig = n) %>% 
        dplyr::mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
        dplyr::group_by(hashtagsLower) %>%
        dplyr::add_tally() %>%
        dplyr::ungroup() %>% 
        dplyr::rename(nTotal = n) %>% 
        dplyr::group_by(hashtags, nTotal) %>% 
        dplyr::distinct(screen_name, .keep_all = TRUE) %>% 
        dplyr::add_count() %>% 
        dplyr::rename(nMepPerHashtag = n) %>% 
        dplyr::select(-screen_name) %>% 
        dplyr::arrange(dplyr::desc(nMepPerHashtag), dplyr::desc(nTotal)) %>% 
        dplyr::ungroup() %>% 
        dplyr::distinct(hashtagsLower, .keep_all = TRUE) %>% 
        dplyr::mutate(hashtagString = paste0("#", hashtags, " (", nMepPerHashtag, " MEPs, ", nTotal, " tweets)"))
      
      ##  consider also how many MEPs
      
      
      if (ncol(tempL)==3) {
        tempL <- tempL %>% 
          dplyr::mutate(New = ((New + 1) / sum(New + 1)),
                        Old = ((Old + 1) / sum(Old + 1))) %>%
          dplyr::mutate(logratio = log(New / Old)) %>%
          dplyr::arrange(dplyr::desc(logratio)) %>% 
          dplyr::transmute(hashtags, NewLog = logratio) %>% 
          head(200) 
        
        tempL <- dplyr::left_join(tempL, 
                                  currentHashtagsDF %>%
                                    dplyr::transmute(hashtags = hashtagsLower, nMepPerHashtag),
                                  by = "hashtags") %>% 
          dplyr::arrange(dplyr::desc(NewLog*nMepPerHashtag)) %>% 
          head(10) %>% 
          dplyr::pull(hashtags)
        
        trending_hashtags[[i]] <- paste0("#", as.character(hashtags[[i]])[is.element(el = tolower(as.character(hashtags[[i]])), set = tempL)])
      }
    }
  }
  currentHashtagsDF <-  tweets_all %>% 
    dplyr::filter(is.na(hashtags)==FALSE) %>%
    dplyr::select(screen_name, hashtags) %>%
    tidyr::unnest(cols = c(hashtags)) %>%
    na.omit() %>% 
    dplyr::group_by(hashtags) %>%
    dplyr::add_count(sort = TRUE) %>% 
    dplyr::rename(nTotalOrig = n) %>% 
    dplyr::mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
    dplyr::group_by(hashtagsLower) %>%
    dplyr::add_tally() %>%
    dplyr::ungroup() %>% 
    dplyr::rename(nTotal = n) %>% 
    dplyr::group_by(hashtags, nTotal) %>% 
    dplyr::distinct(screen_name, .keep_all = TRUE) %>% 
    dplyr::add_count() %>% 
    dplyr::rename(nMepPerHashtag = n) %>% 
    dplyr::select(-screen_name) %>% 
    dplyr::arrange(dplyr::desc(nMepPerHashtag), dplyr::desc(nTotal)) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(hashtagsLower, .keep_all = TRUE) %>% 
    dplyr::mutate(hashtagString = paste0("#", hashtags, " (", nMepPerHashtag, " MEPs, ", nTotal, " tweets)"))
  
  temptrending_hashtags <- 
    tweets_all %>% 
    dplyr::filter(is.na(hashtags)==FALSE) %>% 
    dplyr::select(date, hashtags) %>% 
    tidyr::unnest(cols = c(hashtags)) %>% 
    dplyr::mutate(hashtags = tolower(hashtags)) %>% 
    dplyr::mutate(NewOld = dplyr::if_else(condition = date>=as.Date(Sys.Date()-recent_days),
                                          true = "New", false = "Old")) %>% 
    dplyr::count(hashtags, NewOld) %>% 
    dplyr::ungroup() %>%
    tidyr::spread(NewOld, n, fill = 0) %>%
    dplyr::mutate(New = ((New + 1) / sum(New + 1)),
                  Old = ((Old + 1) / sum(Old + 1)))  %>%
    dplyr::mutate(logratio = log(New / Old)) %>%
    dplyr::arrange(dplyr::desc(logratio)) %>% 
    dplyr::transmute(hashtags, NewLog = logratio) 
  
  temptrending_hashtags <- dplyr::left_join(temptrending_hashtags, 
                                            currentHashtagsDF %>%
                                              dplyr::transmute(hashtags = hashtagsLower, nMepPerHashtag),
                                            by = "hashtags") %>% 
    dplyr::arrange(dplyr::desc(NewLog*nMepPerHashtag)) %>% 
    head(10) %>% 
    dplyr::pull(hashtags)
  
  trending_hashtags$AnyLanguage <- paste0("#", as.character(hashtags$AnyLanguage)[is.element(el = tolower(as.character(hashtags$AnyLanguage)), set = temptrending_hashtags)])
  
  readr::write_rds(x = trending_hashtags,
                   path = fs::path("tweets_processed", "tweets_trending_hashtags_list.rds"))
}