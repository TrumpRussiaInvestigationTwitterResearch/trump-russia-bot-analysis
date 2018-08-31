library(rtweet)
library(readr)
final_ids<-read_csv("final_ids.csv")
final_dat<-lookup_tweets(final_ids$status_id)

write_rds(final_dat,"rtweet_finalish_051618.rds")