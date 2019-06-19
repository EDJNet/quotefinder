#' Create a single data frame from a vector of Twitter handles
#'
#' @param users A character vector of handles or id of Twitter users.
#' @param since A date, expressed in the form Y-M-D (e.g. "2018-12-31")
#' @param save Logical, defaults to TRUE. If TRUE, the merged data frame is stored in the following location: `tweets_all/tweets.all.rds`
#' @examples
#' 
#' @export


qf_bind_rows_tweets <- function(users,
                                since = NULL, 
                                save = TRUE) {
  
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
  
  pb <- dplyr::progress_estimated(length(existing_users_location))
  
  if (is.null(date)) {
    tweets_all <- purrr::map_dfr(.x = existing_users_location,
                                .f = function(x) {
                                  pb$tick()$print()
                                  readRDS(x)
                                })
  } else {
    tweets_all <- purrr::map_dfr(.x = existing_users_location,
                                .f = function(x) {
                                  pb$tick()$print()
                                  readRDS(x) %>% 
                                    filter(created_at>as.POSIXct(as.Date(since)))
                                })
  }
  if (save==TRUE) {
    fs::dir_create(path = "tweets_all")
    saveRDS(object = tweets_all,
            file = fs::path("tweets_all", "tweets_all.rds"))
    message(paste("\nTweets have been saved in", sQuote(fs::path("tweets_all", "tweets_all.rds"))))
  }
  return(tweets_all)
}