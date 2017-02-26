### Social Network analysis of buyer buying habit to find buyer repurchase habit 


##load required package
suppressPackageStartupMessages(require("dplyr"))
suppressPackageStartupMessages(require("tidyr"))
suppressPackageStartupMessages(require("igraph"))
suppressPackageStartupMessages(require("stringi"))
suppressPackageStartupMessages(require("magrittr"))
currentDate = Sys.Date()


#set input to require directory
setwd("G:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))



##import file order_mangement file
bob = read.csv("Order.csv", stringsAsFactors=F)
#bob$Order.date=as.Date(bob$Order.date, "%m/%d/%Y %H:%M")
bob$gross_date=substr(bob$gross_date,1,10) 

## subset of data
bob = subset(bob, bob$gross_date  > as.Date("2015-01-01", "%Y-%m-%d") )
bob$gross_date=as.Date(bob$gross_date, "%Y-%m-%d")
bob$dummay=1


##rename to BOB(order_processing as code are previous written for this file)
bob = rename(bob, Order.number=order_nr,
             Order.date=gross_date,
             latest_status=current_status,
             Billing.name=buyer_name,
             Buyer.email=buyer_email,
             Buyer.phone=buyer_phone)

##remove invalid and duplicate order
bob =subset(bob, bob$latest_status!="invalid")
bob =subset(bob, bob$cancellation_reason!="Duplicate Order" &bob$cancellation_reason!="DuplicateÂ order" )

# sku to 17 char

bob$sku = substr(bob$sku, 1,17)


##filter for those buyer made atleast 3 valid orderr

bob_all = bob %>%
            group_by(Buyer.phone)%>%
                  summarise(valid=sum(dummay))
bob_all =  subset(bob_all, bob_all$valid>2)



##read listing data for level

sku = read.csv("sku.csv", stringsAsFactors=F)[c("sku", "lvl1", "lvl2", "lvl3")]

bob = left_join(bob, sku, by="sku")


##filter only seller who have more than 2valid order

bob_all = left_join(bob_all, bob, by = "Buyer.phone")

bob_all = bob_all[order(bob_all$Order.date), ]


##summarize by lvl

bob_buyer = bob_all %>%
                group_by(Buyer.phone,Order.date, Order.number, sku, lvl1, lvl2, lvl3)%>%
                summarise(random = sum(valid))
bob_buyer1 = bob_buyer %>%
                    group_by(Buyer.phone)%>%
                              summarise(lv3 = list(lvl2))####change lvl3 to level you choice

##remove "c()"

bob = data.frame(substr(bob_buyer1$lv3, 3,nchar(bob_buyer1$lv3)-1))

colnames(bob)[1]="list"


##remove space and comma
bob$list= stri_replace_all_charclass(as.character(bob$list),  "\\p{WHITE_SPACE}", "")
bob=separate(bob, col = list, into=c(1:10), sep = "\\,", extra= "drop")
bobnull = data.frame(first = as.character(),
                     second = as.character())


##change data to two row file source and target

for (i in 1:(ncol(bob)-1)){
  a =as.numeric(i)
  b=as.numeric(i+1)
  bob1 =bob[ , a:b]
  colnames(bob1)[1]="first"
  colnames(bob1)[2]="second"
  bob1 = bob1[complete.cases(bob1), ]
  bobnull = rbind(bob1, bobnull)
}

##remove " from word
bobnull$first = substr(bobnull$first, 2,nchar(bobnull$first)-1)
bobnull$second = substr(bobnull$second, 2,nchar(bobnull$second)-1)


##summize lvl

bobnull$dummy = 1

bobnull = bobnull %>%
                group_by(first, second) %>%
                summarise(weight = sum(dummy))

##cretria to remove weak link and lvl

bobnull = subset(bobnull, bobnull$weight>30)
bobnull = subset(bobnull, bobnull$first=="SportsWatches")###change as per requirement


##igraph 

g = graph.data.frame(bobnull,directed = T)
tkplot(g, vertex.color="green" , edge.width=E(g)$weight/10)



###Save file of list



setwd(paste(filepath, "Output", sep="/"))
csvFileName2 = paste("list",currentDate,".csv",sep=" ") 
write.csv(bob, file=csvFileName2)
setwd(paste(filepath, "Input", sep="/"))

rm(list=ls())


##########################################Author Bikram Dahal#########################################








