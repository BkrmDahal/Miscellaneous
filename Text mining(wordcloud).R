##########################################text mining##########################################
## description 
# file require


#################load required package

suppressPackageStartupMessages(require("dplyr"))
suppressPackageStartupMessages(require("tm"))
#suppressPackageStartupMessages(require("ggplot2"))
#suppressPackageStartupMessages(require("reshape2"))
#suppressPackageStartupMessages(require("magrittr"))
currentDate = Sys.Date()


#########Set the file dir

setwd("G:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))

cname=file.path("G:/R_Script/text anlsyis/")
dir(cname)


# load library and read file

bc = Corpus(DirSource(cname))


# clean text for processing


bc <- tm_map(bc, gsub, pattern="delivery", replacement="deliver")
#convert to lower case
bc <- tm_map(bc, tolower)
# remove punctuation
bc <- tm_map(bc, removePunctuation)
# remove numbers
bc <- tm_map(bc, removeNumbers)
# stopwords("english") 
bc <- tm_map(bc, removeWords, stopwords("english")) 
#remove words
#bc <- tm_map(bc, removeWords, c("hospit","college","floor","old","plaza","colleg","hospital","ward","hotel",
  #"marga","ltd","near", "marg", "new","nepal", "nep", "chowk", 'bank',"next", "house", "school",  "opposite")) 
bc <- tm_map(bc, removeWords, c("hospit","college","floor","old","plaza","colleg","hospital","ward","hotel",
                                "opp","galli","way", "ahead","shop","kathmandu","area","restaur",
                                "please","can","will","thank","plz",
           "marga","ltd","near", "marg", "new","nepal", "nep", "chowk", 'bank',"next", "house", "school",  "opposite")) 

# Removing common word endings (e.g., “ing”, “es”, “s”)

library(SnowballC)   

bccopy <- bc
bc <- tm_map(bc, stemDocument) 
bc <- tm_map(bc, stemCompletion, dictionary=bccopy)




#stemCompletion_mod <- function(x,dict=dictCorpus) {
#PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))


# Stripping unnecesary whitespace from your documents

bc <- tm_map(bc, stripWhitespace) 

bc <- tm_map(bc, PlainTextDocument) 



#Convert to Matris 

bc <- DocumentTermMatrix(bc) 
freq <- colSums(as.matrix(bc)) 
freq <- freq[order(freq, decreasing = T)]


# convert to data frane

bc_data <- data.frame(word=names(freq), freq=freq)


# make wordcloud

library(wordcloud)


# save wordcloud

pal2 <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1280,height=800)
m = wordcloud(names(freq), freq, scale=c(10,1),min.freq=150,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()


setwd(filepath)
dir.create("Output")
setwd(paste(filepath, "Output", sep="/"))
csvFileName = paste("words",currentDate,".csv",sep=" ") 
write.csv(bc_data, file=csvFileName)
setwd(paste(filepath, "Input", sep="/"))

##########################################Author Bikram Dahal#########################################
