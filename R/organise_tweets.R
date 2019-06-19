#' Create a single data frame from a vector of Twitter handles
#'
#' @param users A character vector of handles or id of Twitter users.
#' @param since A date, expressed in the form Y-M-D (e.g. "2018-12-31")
#' @examples
#' 
#' @export


qf_bind_rows_tweets <- function(users,
                                since = NULL) {
  
  available_users_location <- fs::dir_ls(path = fs::path("tweets_by_user"),
                                         recurse = FALSE, 
                                         type = "file", 
                                         glob = "*.rds")
  
  available_users <- fs::path_file(path = available_users_location) %>% 
    fs::path_ext_remove()
  
 
  missing_users <- users[!is.element(el = users, set = available_users)]
  
  if (length(missing_users)>0) {
    warning(paste0("There is no locally stored file with tweets from the following users:\n",
                   paste(missing_users, collapse = ", ")))  
  }
  
  existing_users_location <- fs::path("tweets_by_user",
                             paste0(users[is.element(el = users, set = available_users)], ".rds"))
  
  pb <- dplyr::progress_estimated(length(existing_users_location))
  
  if (is.null(date)) {
    purrr::map_dfr(.x = existing_users_location,
                                .f = function(x) {
                                  pb$tick()
                                  readRDS(x)
                                })
  } else {
    purrr::map_dfr(.x = existing_users_location,
                                .f = function(x) {
                                  pb$tick()
                                  readRDS(x) %>% 
                                    filter(created_at>as.POSIXct(as.Date(since)))
                                })
  }
}