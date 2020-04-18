library(rtweet)

the_bern <- get_timeline("BernieSanders", n = 500)

the_pete <- get_timeline("PeteButtigieg", n = 500)

head(the_bern)


#Extracting the data as csv's 

save_as_csv(the_bern,"bernie.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

save_as_csv(the_pete, "pete.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#We now have the tweets for two political candidates
