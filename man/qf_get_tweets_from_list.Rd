% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tweets.R
\name{qf_get_tweets_from_list}
\alias{qf_get_tweets_from_list}
\title{Extracts and stores tweets locally
Extracts and stores tweets locally based on Twitter lists}
\usage{
qf_get_tweets_from_list(
  list_id = NULL,
  slug = NULL,
  owner_user = NULL,
  since_id = NULL,
  max_id = NULL,
  n = 200,
  include_rts = TRUE,
  parse = TRUE,
  twitter_token = NULL,
  cache_lists = TRUE,
  overwrite_lists = FALSE
)
}
\arguments{
\item{n}{An integer, number of new tweets to request per users, defaults to 200.}

\item{twitter_token}{A twitter token generated by rtweet.}

\item{cache_lists}{Logical, defaults to TRUE. If TRUE, stores locally the lists owned by a given user, in a subfolder `lists_by_user`. If list has already been downloaded, it just loads it. To overwrite, set `overwrite` to TRUE.}

\item{overwrite_lists}{Logical, defaults to FALSE. If TRUE, it overwrites previously downloaded lists.}
}
\description{
Extracts and stores tweets locally
Extracts and stores tweets locally based on Twitter lists
}
\examples{

}
