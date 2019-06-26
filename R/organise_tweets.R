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
                                save = TRUE,
                                twitter_token = NULL) {
  
  if (is.null(lists)==FALSE) {
    # make sure all lists are defined by id
    lists <- purrr::map_dbl(.x = lists, .f = function(x) {
      if (stringr::str_detect(string = x, pattern = "/")) {
        extracted <- stringr::str_split(string = x, pattern = "/", simplify = TRUE)
        as.numeric(edjnetquotefinder::qf_find_list_id(slug = extracted[[2]],
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
    dplyr::distinct(status_id)
  
  if (save==TRUE) {
    fs::dir_create(path = "tweets_all")
    saveRDS(object = tweets_all,
            file = fs::path("tweets_all", "tweets_all.rds"))
    message(paste("\nTweets have been saved in", sQuote(fs::path("tweets_all", paste0(Sys.time(), "-tweets_all.rds")))))
  }
  return(tweets_all)
}