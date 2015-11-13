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
################################

View(d1pur)
# variable Trip_Info is in  format AAABBBCCC 
# AAA = IRI week
# BBB = store#
# CCC = SKU# purchased
d1pur[,3]<- substr(d1pur[,2],0,3)
d1pur[,4]<- substr(d1pur[,2],4,6)
d1pur[,5]<- substr(d1pur[,2],7,9)
colnames(d1pur) <- c("HH_id", "trip_info", "iri_week", "store#", "sku#_pur")

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


####################
# Making one dataset
####################
plot(as.numeric(d1pur$iri_week),as.numeric(d1pur$HH_id))

hist(as.numeric(d1pur$iri_week))













