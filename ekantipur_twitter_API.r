###########################################################################################################
#                                       E-kantipue twitter Feed                                           #
###########################################################################################################

##lets load packages
pacman::p_load( twitteR, tm, lubridate, dplyr, ggplot2, plotly)

##Set the file dir

setwd("M:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))

#facebook key
app_id = "851737074936993"
app_secret = "eae791c875ddaf7644e1a846028991ca"

#facebook key
consumer_key <- "BpZVspkjJoLxlsrNWYcvch9Kz"
consumer_secret <- "5eOVJPT68tbOHz8PJPfYEOV9vH6DHut0gFixO5Fju8UeQfXfMy"
access_token <- "724837043507990529-tWbrWl5DIrtoByyHdQPhqlqj82aiGLU"
access_secret <- "BlHdecII8zhUGZXixchGS3Owo3BJn4W3WsAAHGE3mlxeR"


token<- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#lets get twitter from user timeline
term <- userTimeline('ekantipur_com', n=3200)

#lets convert to dataframe
term <- twListToDF(term)
term2 = term
term2$created = substr(term2$created, 1,10)

##remmove these date as we had nepal vs nam so no of tweet is outliner
term2 = term2[which(term2$created!='2016-04-18' & term2$created!='2016-04-16' ),c("id","replyToUID")]
term = left_join(term2, term)

##add some coloum and make time NST
term$created = ymd_hms(term$created)
term$additon_hour = "05:45:00"
term$additon_hour = hms(term$additon_hour)
term$created = term$created + term$additon_hour
term$hour = hour(term$created)
term$wekday = wday(term$created, label = T)

##lets make a summary

term = term[order(term$favoriteCount, decreasing = T), ]
temp <- term %>%
            group_by(hour)%>%
              summarise(No_tweet = length(created), No_of_fav = sum(favoriteCount), No_of_retwt = sum(retweetCount))
temp$fav_per_twt = temp$No_of_fav/temp$No_tweet


# lets do ploting

g <- ggplot(temp, aes(hour, No_tweet, color = fav_per_twt,size = No_of_fav)) +
  geom_point() +scale_colour_gradient2(low = "green",mid='blue', high = "red", midpoint =18 )+
  ggtitle("No of twitter per  hours, Fav count and Fav per tweet")
ggplotly(g)
plot(g)


###facebook

fb_oauth <- fbOAuth(app_id, app_secret, extended_permissions = T)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")
me <- getPage("eKantipur", token=fb_oauth, n=80)
me = me[order(me$likes_count,decreasing = T), ]


