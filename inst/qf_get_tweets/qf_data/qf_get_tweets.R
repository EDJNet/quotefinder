# get tweets
rmarkdown::render(input = "qf_data/qf_get_tweets.Rmd")
# pre-process tweets
rmarkdown::render(input = "qf_data/qf_pre_process_tweets.Rmd")
  