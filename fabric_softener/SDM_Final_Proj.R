#########################
## Loading all data files

d1pur <- read.table("D1PUR.dat")
colnames(d1pur) <- c("HH_id","trip_info")

merch <- read.table("MERCH.dat")
colnames(merch) <- c("SKU#",  "store#",  "IRIweek",  "price_paid",  "merchandising")

arsp <- read.table("ARSP.dat")
colnames(arsp) <- c("SKU#",  "store#", "ARSP")

brsinfo <- read.table("BRSINFO.dat")
colnames(brsinfo) <- c("SKU#",  "description_of_SKU",  "brand",  "form",  "formula1", "formula2",
                    "size",  "brand#",  "form#",  "formula2#",  "size#")

iridates <- read.table("IRIdates.csv", skip = 540, sep=",")
iridates <- iridates[,1:2]
colnames(iridates) <- c("week#", "ending_date")

################################
## Cleaning Individual Dataframes

View(d1pur)
# variable Trip_Info is in  format AAABBBCCC 
# AAA = IRI week
# BBB = store#
# CCC = SKU# purchased
d1pur[,3]<- substr(d1pur[,2],0,3)
d1pur[,4]<- substr(d1pur[,2],4,6)
d1pur[,5]<- substr(d1pur[,2],7,9)
colnames(d1pur) <- c("HH_id", "trip_info", "IRIweek", "store#", "SKU#")

View(merch)
# Converting SKU# to 3 digit format
merch[,1] <- sprintf("%03d",merch[,1])
# Converting store# to 3 digit format
merch[,2] <- sprintf("%03d",merch[,2])
# Converting all merchandising obs to 6 digit format
merch[,5] <- sprintf("%06d",merch[,5])
# The format of the merchandising variable is AAABCD where
# AAA = depromoted price
# B   = ignore
# C   = display
# D   = feature
merch[,6] <- substr(merch[,5],0,3)
merch[,7] <- substr(merch[,5],4,4)
merch[,8] <- substr(merch[,5],5,5)
merch[,9] <- substr(merch[,5],6,6)
colnames(merch) <- c("SKU#",  "store#",  "IRIweek",  "price_paid",  "merchandising", 
                     "regular_price", "ignore", "display", "feature")
# Creating dummies for feature and display
merch[,10] <- ifelse(as.numeric(merch[,8])>=1,1,0)
merch[,11] <- ifelse(as.numeric(merch[,9])>=1,1,0)
colnames(merch) <- c("SKU#",  "store#",  "IRIweek",  "price_paid",  "merchandising" 
                     ,"regular_price", "ignore", "display", "feature", "d_disp", "d_feat")
# Converting regular_price into "0.00" format
merch[,6] <- paste(substr(merch[,6],0,1),substr(merch[,6],2,3),sep = ".")
merch[,6] <- as.numeric(merch[,6])
# Creating new variable price_cut
# price_cut = regular_price - price_paid (if the result is < 0, price_cut = 0)
merch[,12] <- merch[,6] - merch[,4] 
merch[,12] <- ifelse(merch[,12]<0,0,merch[,12])
colnames(merch) <- c("SKU#",  "store#",  "IRIweek",  "price_paid",  "merchandising" 
                     ,"regular_price", "ignore", "display", "feature", "d_disp", "d_feat", "price_cut")

View(arsp)
# Converting SKU# to 3 digit format
arsp[,1] <- sprintf("%03d",arsp[,1])
# Converting store# to 3 digit format
arsp[,2] <- sprintf("%03d",arsp[,2])

View(brsinfo)
# Getting rid of Formula1 col
brsinfo <- brsinfo[,c(1,2,3,4,6,7,8,9,10,11)]
brsinfo <- brsinfo[,c(1,3,4,5,6)]
# Creating Dummy Variables
brsinfo <- cbind2(brsinfo, model.matrix(~brand -1,brsinfo))
brsinfo <- cbind2(brsinfo, model.matrix(~form -1,brsinfo))
brsinfo <- cbind2(brsinfo, model.matrix(~formula2 -1,brsinfo))
brsinfo <- cbind2(brsinfo, model.matrix(~size -1,brsinfo))


######################
# Making Final Dataset

# Combining brsinfo and arsp
arsp_brs <- cbind2(arsp,brsinfo)
arsp_brs_1 <- arsp_brs[,-1]
## *** USED Dr. Zantedeschi's function in this part. 
## It's a much better and easier way of merging than what I had in mind**** 
d1pur[,3] <- as.numeric(d1pur[,3])
d1pur[,4] <- as.numeric(d1pur[,4])
d1pur[,5] <- as.numeric(d1pur[,5])
d1pur1 <- d1pur[,c(5,4,3,1,2)]
merch[,1] <- as.numeric(merch[,1])
merch[,2] <- 70
merch[,3] <- as.numeric(merch[,3])
merch_d1 <- merge(d1pur1, merch, by=c("IRIweek", "store#","SKU#"))
# Combining all data into final dataset 
finaldata <- merge(arsp_brs_1,merch_d1,by=c("SKU#"))
# Removing Redundent variables
finaldatav1 <- finaldata[,c(-2,-7,-31,-33,-35,-37)]
write.csv(finaldatav1,"finaldatav1.csv",row.names = F)
#Importing date/season/month file created in excel from IRIdates.csv
IRIdates_months_seasons <- read.csv("IRIdates_months_seasons.csv")
finaldatav2 <- merge(finaldatav1,IRIdates_months_seasons,by=("IRIweek"))
finaldatav3 <- finaldatav2
write.csv(finaldatav3,"finaldatav3.csv",row.names = F)


###########
# Analysis

with(finaldatav1,plot(HH_id,ARSP))
hist(finaldatav1$ARSP)
hist(finaldatav1$price_paid)
hist(finaldatav1$price_cut)
hist(finaldatav1$`SKU#`)
hist(finaldatav1$HH_id,breaks = 500)

barplot(prop.table(table(finaldata$`SKU#`)))
barplot(prop.table(table(finaldata$brand)))
barplot(prop.table(table(finaldata$form)))
barplot(prop.table(table(finaldata$formula2)))
barplot(prop.table(table(finaldata$size)))

barplot(prop.table(table(finaldata$HH_id)))
par(mfrow=c(2,2))
barplot(prop.table(table(finaldata$IRIweek[finaldata$formS==1])),ylim=c(0,0.1))
barplot(prop.table(table(finaldata$IRIweek[finaldata$formB==1])),ylim=c(0,0.1))
barplot(prop.table(table(finaldata$IRIweek[finaldata$formF==1])),ylim=c(0,0.1))
barplot(prop.table(table(finaldata$IRIweek[finaldata$formL==1])),ylim=c(0,0.1))

par(mfrow=c(2,2))
barplot(prop.table(table(finaldata$IRIweek[finaldata$sizeSM==1])),ylim=c(0,0.1), main = "Small")
barplot(prop.table(table(finaldata$IRIweek[finaldata$sizeMD==1])),ylim=c(0,0.1), main = "Medium")
barplot(prop.table(table(finaldata$IRIweek[finaldata$sizeLR==1])),ylim=c(0,0.1), main = "Large")
barplot(prop.table(table(finaldata$IRIweek[finaldata$sizeXL==1])),ylim=c(0,0.1), main = "X-Large")




par(mfrow=c(1,1))
barplot(prop.table(table(finaldata$display)))
barplot(prop.table(table(finaldata$feature)))
barplot(prop.table(table(finaldata$d_disp)))
barplot(prop.table(table(finaldata$d_feat)))

par(mfrow=c(2,2))
barplot(prop.table(table(finaldata$ARSP)),ylim=c(0,0.2))
barplot(prop.table(table(finaldata$price_paid)),ylim=c(0,0.2))
barplot(prop.table(table(finaldata$regular_price)),ylim=c(0,0.2))
barplot(prop.table(table(finaldata$price_cut)),ylim=c(0,0.2))

plot(finaldatav1$HH_id,finaldatav1$price_cut)
plot(finaldatav1$HH_id,finaldatav1$price_paid)


#####################################
## Temporal Variations in SKU choices


# Size of SKU's bought in IRI Weeks  
par(mfrow=c(2,2))
barplot(prop.table(table(finaldatav2$IRIweek[finaldatav2$sizeSM==1 & finaldatav2$Season=="summer"])),ylim=c(0,0.1), main = "Small")
barplot(prop.table(table(finaldatav2$IRIweek[finaldatav2$sizeMD==1 & finaldatav2$Season=="summer"])),ylim=c(0,0.1), main = "Medium")
barplot(prop.table(table(finaldatav2$IRIweek[finaldatav2$sizeLR==1 & finaldatav2$Season=="summer"])),ylim=c(0,0.1), main = "Large")
barplot(prop.table(table(finaldatav2$IRIweek[finaldatav2$sizeXL==1 & finaldatav2$Season=="summer"])),ylim=c(0,0.1), main = "X-Large")
par(mfrow=c(1,1))
    # No clear pattern

table(finaldatav2$Season,finaldata$form)
table(finaldatav2$Season,as.factor(finaldata$`SKU#`))
table(finaldatav2$Season,finaldatav2$d_feat)
table(finaldatav2$Season,finaldatav2$d_disp)

# Sale of Brands by Season
table(finaldatav2$Season,finaldata$brand)
par(mfrow=c(2,2))
barplot(prop.table(table(finaldata$brand[finaldatav2$Season=="winter"])), main="Winter", xlab = "Brand", col = "Light Blue")
barplot(prop.table(table(finaldata$brand[finaldatav2$Season=="spring"])), main="Spring", xlab = "Brand", col = "light green")
barplot(prop.table(table(finaldata$brand[finaldatav2$Season=="summer"])), main="Summer", xlab = "Brand", col = "orange")
barplot(prop.table(table(finaldata$brand[finaldatav2$Season=="autumn"])), main="Autumn", xlab = "Brand", col = "brown")
par(mfrow=c(1,1)) 
    #Clear distinction of choice of brand in different seasons
    # There could be someting in a particular brand that causes it to be prefered in a season

# Sale of Brands by Month
table(finaldatav2$Season,finaldata$brand)
par(mfrow=c(3,4))
for (i in 1:12) {
  barplot(prop.table(table(finaldata$brand[finaldatav2$Month==i])), main=i, xlab = "Brand")
}
par(mfrow=c(1,1))
    # No clear pattern 

# Sale of Size by Season
table(finaldatav3$Season,finaldatav3$size)
par(mfrow=c(2,2))
barplot(prop.table(table(finaldata$size[finaldatav2$Season=="winter"])), main="Winter", xlab = "Brand", col = "Light Blue", ylim = c(0,0.8))
barplot(prop.table(table(finaldata$size[finaldatav2$Season=="spring"])), main="Spring", xlab = "Brand", col = "light green", ylim = c(0,0.8))
barplot(prop.table(table(finaldata$size[finaldatav2$Season=="summer"])), main="Summer", xlab = "Brand", col = "orange", ylim = c(0,0.8))
barplot(prop.table(table(finaldata$size[finaldatav2$Season=="autumn"])), main="Autumn", xlab = "Brand", col = "brown", ylim = c(0,0.8))
par(mfrow=c(1,1)) 
    # Medium is the most widely sold size across seasons
    # XL is mostly sold in Autumn
    # Summer and Spring see high sales of small


# Price cut by Season
par(mfrow=c(2,2))
hist(finaldatav3$price_cut[finaldatav3$Season=="winter"], ylim = c(0,1500), main="Winter", xlab = "Price Cut", col = "Light Blue")
hist(finaldatav3$price_cut[finaldatav3$Season=="spring"], ylim = c(0,1500), main="Spring", xlab = "Price Cut", col = "light green")
hist(finaldatav3$price_cut[finaldatav3$Season=="summer"], ylim = c(0,1500),main="Summer", xlab = "Price Cut", col = "orange")
hist(finaldatav3$price_cut[finaldatav3$Season=="autumn"], ylim = c(0,1500), main="Autumn", xlab = "Price Cut", col = "brown")
par(mfrow=c(1,1))
    # The higher price cuts took place in the Autumn and the most lower price cuts took place in Spring

# Sale of SKU by Season
table(finaldatav3$Season,finaldatav3$`SKU#`)
par(mfrow=c(2,2))
barplot(prop.table(table(finaldatav3$`SKU#`[finaldatav3$Season=="winter"])), main="Winter", xlab = "Brand", col = "Light Blue", ylim = c(0,0.1))
barplot(prop.table(table(finaldatav3$`SKU#`[finaldatav3$Season=="spring"])), main="Spring", xlab = "Brand", col = "light green", ylim = c(0,0.1))
barplot(prop.table(table(finaldatav3$`SKU#`[finaldatav3$Season=="summer"])), main="Summer", xlab = "Brand", col = "orange", ylim = c(0,0.1))
barplot(prop.table(table(finaldatav3$`SKU#`[finaldatav3$Season=="autumn"])), main="Autumn", xlab = "Brand", col = "brown", ylim = c(0,0.1))
par(mfrow=c(1,1))
    # Some SKU choices are more popular in some seasons than others

# ARSP per SKU by SIZE
plot(finaldatav3$`SKU#`,finaldatav3$ARSP, col = finaldatav3$size, pch =19)
    #no clear pattern

# Monthly/Seasonal Sales of SKU's
barplot(prop.table(table(finaldatav3$Month)))
    # First Half of the year the sales are high as compared to rest of the year
barplot(prop.table(table(finaldatav3$Season)))
    # Spring sees the highest sales followed by winter
    # Highest price cuts in autum maybe because it sees the lowest sales


######################################
## Household Variations in SKU choices

par(mfrow=c(2,2))
hist(finaldatav2$HH_id[finaldatav2$Season=="winter"], ylim = c(0,300), main="Winter", xlab = "House ID", col = "Light Blue")
hist(finaldatav2$HH_id[finaldatav2$Season=="spring"], ylim = c(0,300), main="Spring", xlab = "House ID", col = "light green")
hist(finaldatav2$HH_id[finaldatav2$Season=="summer"], ylim = c(0,300), main="Summer", xlab = "House ID", col = "orange")
hist(finaldatav2$HH_id[finaldatav2$Season=="autumn"], ylim = c(0,300), main="Autumn", xlab = "House ID", col = "brown")
par(mfrow=c(1,1)) 
    #Sales of fabric softner go up in winter and spring months. Highest in Spring

table(finaldatav2$Season,finaldata$form)

# Relationship between Price_cut and Price_paid
cor(finaldatav2$price_cut,finaldatav2$regular_price) # +0.5 correlation
plot(finaldatav2$price_cut,finaldatav2$regular_price, col=finaldatav3$size) 
    #other than no price cut, There is a positive relationship between the two
plot(log(finaldatav2$price_cut),finaldatav2$regular_price)
plot(finaldatav2$price_cut,log(finaldatav2$regular_price))
plot(log(finaldatav2$price_cut),log(finaldatav2$regular_price)) # log-log looks the best fit
    #the more the price is cut, the more the consumer is willing to pay for the fabric softner



##############################################
## Making predictive Models at Household Level

# Dividing dataset into Training and Holdout datasets
train <- finaldatav3[1:4417,]
test <- finaldatav3[4418:6554,]
as.factor(finaldatav3$SKU.)

library("party")
ctree1 <- ctree(as.factor(brand) ~ as.factor(HH_id) + as.factor(Month) + as.factor(Season) +
                  as.factor(brand) + as.factor(form) + as.factor(formula) + as.factor(size) +
                  as.factor(display)  + as.factor(feature) + as.numeric(regular_price) + as.numeric(price_paid) +as.numeric(price_cut), data = train)

glm1 <- glm(as.factor(SKU.) ~ as.factor(HH_id) + as.factor(Month) + as.factor(Season) +
              as.factor(brand) + as.factor(form) + as.factor(formula) + as.factor(size) +
              as.factor(display)  + as.factor(feature) + as.numeric(regular_price) + as.numeric(price_paid) +as.numeric(price_cut), data = train)
summary(glm1)
    # Dimension of SKU variable has to be reduced as it has 59 levels. 


)



# Create Lag variable for IRIweek