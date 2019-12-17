emm_df <- readRDS(file = file.path(.quotefinder.path,"qf_data", "emm_newsbrief_processed", "emm_newsbrief_processed.rds"))
emm_languages <- readRDS(file = file.path(.quotefinder.path,"qf_data", "emm_newsbrief_processed", "emm_newsbrief_languages.rds"))

quotefinder:::app_server
