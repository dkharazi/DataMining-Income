## Import Statements

## Import Libraries 
library(readr)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

## Import Dataset
income <- read_csv("./income_tr.csv") ## Input Location of income_tr.csv file
income.df <- data.frame(income)

## Create new data frame
newincome.df <- data.frame(income.df$ID)
newincome.df[,2:42] <- 0

## Rename columns
colnames(newincome.df) <- c("ID", "isAge10-19", "isAge20-29", "isAge30-39", "isAge40-49", "isAge50-59", "isAge60-69", "isAge70-79", "isAge80-89", "isWorkPriv", "isWorkOther", "isWgt20-99k", "isWgt100-199k", "isWgt200-299k", "isWgt300-399k", "isWgt400-499k", "isWgt500k+", "isEduHighGradOrLess", "isEduSCol-Bach", "isEduMastOrMore", "isMarried", "isNeverMarried", "isDivOrSep", "isWidowed", "isOccBus", "isOccTech", "isOccOther", "isRelHusbOrWife", "isRelUnmarried", "isRelOther", "isRaceWhite", "isRaceOther", "isMale", "isFemale", "isCapGain0", "isCapGainOther", "isCapLoss0", "isCapLossOther", "isHPW40OrLess", "isHPW41OrMore", "isCountryUS", "isCountryOther")


## Data Pre-Processing

## Get indices of corresponding entries for "age"
tenInd <- which(income.df$age >= 10 & income.df$age <= 19)
twentyInd <- which(income.df$age >= 20 & income.df$age <= 29)
thirtyInd <- which(income.df$age >= 30 & income.df$age <= 39)
fortyInd <- which(income.df$age >= 40 & income.df$age <= 49)
fiftyInd <- which(income.df$age >= 50 & income.df$age <= 59)
sixtyInd <- which(income.df$age >= 60 & income.df$age <= 69)
seventyInd <- which(income.df$age >= 70 & income.df$age <= 79)
eightyInd <- which(income.df$age >= 80 & income.df$age <= 89)

## Assign correct values to newincome.df according to "age"
newincome.df$`isAge10-19`[tenInd] = 1
newincome.df$`isAge20-29`[twentyInd] = 1
newincome.df$`isAge30-39`[thirtyInd] = 1
newincome.df$`isAge40-49`[fortyInd] = 1
newincome.df$`isAge50-59`[fiftyInd] = 1
newincome.df$`isAge60-69`[sixtyInd] = 1
newincome.df$`isAge70-79`[seventyInd] = 1
newincome.df$`isAge80-89`[eightyInd] = 1

## Get indices of corresponding entries for "workclass"
privInd <- which(income.df$workclass %in% c("Private"))
othInd <- which(!income.df$workclass %in% c("Private"))

## Assign correct values to newincome.df according to "workclass"
newincome.df$isWorkPriv[privInd] = 1
newincome.df$isWorkOther[othInd] = 1

## Get indices of corresponding entries for "fnlwgt"
twentyInd <- which(income.df$fnlwgt >= 20000 & income.df$fnlwgt < 100000)
onehundInd <- which(income.df$fnlwgt >= 100000 & income.df$fnlwgt < 200000)
twohundInd <- which(income.df$fnlwgt >= 200000 & income.df$fnlwgt < 300000)
threehundInd <- which(income.df$fnlwgt >= 300000 & income.df$fnlwgt < 400000)
fourhundInd <- which(income.df$fnlwgt >= 400000 & income.df$fnlwgt < 500000)
fivehundInd <- which(income.df$fnlwgt >= 500000 & income.df$fnlwgt < 1000000)

## Assign correct values to newincome.df according to "fnlwgt"
newincome.df$`isWgt20-99k`[twentyInd] = 1
newincome.df$`isWgt100-199k`[onehundInd] = 1
newincome.df$`isWgt200-299k`[twohundInd] = 1
newincome.df$`isWgt300-399k`[threehundInd] = 1
newincome.df$`isWgt400-499k`[fourhundInd] = 1
newincome.df$`isWgt500k+`[fivehundInd] = 1

## Get indices of corresponding entries for "education"
highInd <- which(income.df$education %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th", 
                                            "9th", "10th", "11th", "12th", "HS-grad"))
bachInd <- which(income.df$education %in% c("Some-college", "Assoc-acdm", "Assoc-voc",
                                            "Bachelors"))
mastInd <- which(income.df$education %in% c("Masters", "Doctorate", "Prof-school"))

## Assign correct values to newincome.df according to "education"
newincome.df$isEduHighGradOrLess[highInd] = 1
newincome.df$`isEduSCol-Bach`[bachInd] = 1
newincome.df$isEduMastOrMore[mastInd] = 1

## Get indices of corresponding entries for "marital_status"
marriedInd <- which(income.df$marital_status %in% c("Married-AF-spouse", 
                                                    "Married-civ-spouse", 
                                                    "Married-spouse-absent"))
nMarriedInd <- which(income.df$marital_status %in% c("Never-married"))
divInd <- which(income.df$marital_status %in% c("Divorced", "Separated"))
widInd <- which(income.df$marital_status %in% c("Widowed"))

## Assign correct values to newincome.df according to "marital_status"
newincome.df$isMarried[marriedInd] = 1
newincome.df$isNeverMarried[nMarriedInd] = 1
newincome.df$isDivOrSep[divInd] = 1
newincome.df$isWidowed[widInd] = 1

## Get indices of corresponding entries for "occupation"
occBusInd <- which(income.df$occupation %in% c("Adm-clerical", 
                                               "Exec-managerial", 
                                               "Sales"))
occTechInd <- which(income.df$occupation %in% c("Machine-op-inspct", "Tech-support"))
occOthInd <- which(income.df$occupation %in% c("?", "Craft-repair", "Farming-fishing",
                                               "Handlers-cleaners", "Other-service", 
                                               "Priv-house-serv", "Prof-specialty", 
                                               "Protective-serv", "Transport-moving"))

## Assign correct values to newincome.df according to "occupation"
newincome.df$isOccBus[occBusInd] = 1
newincome.df$isOccTech[occTechInd] = 1
newincome.df$isOccOther[occOthInd] = 1

## Get indices of corresponding entries for "relationship"
relHW <- which(income.df$relationship %in% c("Husband", 
                                             "Wife"))
relUnmarr <- which(income.df$relationship %in% c("Unmarried"))
relOther <- which(income.df$relationship %in% c("Not-in-family", "Other-relative",
                                                "Own-child"))

## Assign correct values to newincome.df according to "relationship"
newincome.df$isRelHusbOrWife[relHW] = 1
newincome.df$isRelUnmarried[relUnmarr] = 1
newincome.df$isRelOther[relOther] = 1

## Get indices of corresponding entries for "race"
raceWhite <- which(income.df$race %in% c("White"))
raceOther <- which(!income.df$race %in% c("White"))

## Assign correct values to newincome.df according to "race"
newincome.df$isRaceWhite[raceWhite] = 1
newincome.df$isRaceOther[raceOther] = 1

## Get indices of corresponding entries for "gender"
genMale <- which(income.df$gender %in% c("Male"))
genFemale <- which(income.df$gender %in% c("Female"))

## Assign correct values to newincome.df according to "gender"
newincome.df$isMale[genMale] = 1
newincome.df$isFemale[genFemale] = 1

## Get indices of corresponding entries for "capital_gain"
gainZero <- which(income.df$capital_gain == 0)
gainOther <- which(income.df$capital_gain != 0)

## Assign correct values to newincome.df according to "capital_gain"
newincome.df$isCapGain0[gainZero] = 1
newincome.df$isCapGainOther[gainOther] = 1

## Get indices of corresponding entries for "capital_loss"
lossZero <- which(income.df$capital_loss == 0)
lossOther <- which(income.df$capital_loss != 0)

## Assign correct values to newincome.df according to "capital_loss"
newincome.df$isCapLoss0[lossZero] = 1
newincome.df$isCapLossOther[lossOther] = 1

## Get indices of corresponding entries for "hours_per_week"
hrsForty <- which(income.df$hour_per_week > 0 & income.df$hour_per_week <= 40)
hrsGreater <- which(income.df$hour_per_week > 40 & income.df$hour_per_week <= 100)

## Assign correct values to newincome.df according to "hours_per_week"
newincome.df$isHPW40OrLess[hrsForty] = 1
newincome.df$isHPW41OrMore[hrsGreater] = 1

## Get indices of corresponding entries for "native_country"
countryUS <- which(income.df$native_country %in% c("United-States"))
countryOther <- which(!income.df$native_country %in% c("United-States"))

## Assign correct values to newincome.df according to "native_country"
newincome.df$isCountryUS[countryUS] = 1
newincome.df$isCountryOther[countryOther] = 1


## Proximity Functions

### Cosine Similarity

## Convert data frame to matrix
newincome.m <- data.matrix(newincome.df)

## Create adjustable k
k <- 5 # Adjust k here

## Create a new data frame containing cosine similarities and output values
cossim.m <- matrix(nrow=(nrow(newincome.df)+1), ncol=(nrow(newincome.df)+1))
cossim.m[2:nrow(cossim.m),1] <- newincome.df$ID
cossim.m[1,2:nrow(cossim.m)] <- newincome.df$ID
cosine.df <- data.frame(income.df$ID)
cosine.df[,2:((k*2)+1)] <- 0

## Rename columns
names(cosine.df)[seq(2,ncol(cosine.df),2)] <- "Index"
names(cosine.df)[seq(1,ncol(cosine.df),2)] <- "Proximity"
names(cosine.df)[1] <- "ID"

## Create a function for calculating cosine similarity of two distinct data entries
cosine <- function(indi, indj) {
  xVec <- as.numeric(newincome.df[indi,2:42])
  yVec <- as.numeric(newincome.df[indj,2:42])
  xlen <- sqrt(sum(xVec^2))
  ylen <- sqrt(sum(yVec^2))
  simC <- sum(t(xVec)*yVec)/(xlen*ylen)
  return(simC)
}

## Perform function for each entry
for (rowi in 1:nrow(newincome.df)) {
  for (rowj in 1:nrow(newincome.df)) {
    cossim.m[rowi+1, rowj+1] <- cosine(rowi, rowj)
  }
}

## Convert matrix to dataframe
cossim.df <- data.frame(cossim.m)
cossim.df[1,1] = 0
colnames(cossim.df) <- cossim.df[1,]
cossim.df <- cossim.df[-1,]
colnames(cossim.df)[1] <- "ID"

## Remove high similarities of entries similar to itself
for(i in 1:nrow(cossim.df)) {
  cossim.df[i, i+1] = 0
}

## Find k most similar entries and add values from matrix to output df
for(col in 2:ncol(cossim.df)) {
  i = 0
  kmost <- sort(cossim.df[,col], index.return=TRUE, decreasing = TRUE)
  kmost <- lapply(kmost, `[`, kmost$x %in% head(kmost$x,k))
  for(coscol in 1:k) {
    cosine.df[col-1,coscol+i+1] <- kmost$ix[coscol]
    cosine.df[col-1,coscol+i+2] <- kmost$x[coscol]
    i = i + 1
  }
}

### Jaccard Similarity

## Convert data frame to matrix
newincome.m <- data.matrix(newincome.df)

## Create adjustable k
k <- 5 # Adjust k here

## Create a new data frame containing jaccard similarities and output values
jaccard.m <- matrix(nrow=(nrow(newincome.df)+1), ncol=(nrow(newincome.df)+1))
jaccard.m[2:nrow(jaccard.m),1] <- newincome.df$ID
jaccard.m[1,2:nrow(jaccard.m)] <- newincome.df$ID
jaccard.df <- data.frame(income.df$ID)
jaccard.df[,2:((k*2)+1)] <- 0

## Rename columns
names(jaccard.df)[seq(2,ncol(jaccard.df),2)] <- "Index"
names(jaccard.df)[seq(1,ncol(jaccard.df),2)] <- "Proximity"
names(jaccard.df)[1] <- "ID"

## Create a function for calculating euclidean similarity of two distinct data entries
jaccard <- function(indi, indj) {
  bmask <- rep(1, 41)
  xVec <- as.numeric(newincome.df[indi,2:42])
  yVec <- as.numeric(newincome.df[indj,2:42])
  M01 <- sum((!xVec&bmask)&(yVec&bmask))
  M10 <- sum((xVec&bmask)&(!yVec&bmask))
  M00 <- sum(!xVec & !yVec)
  M11 <- sum(xVec & yVec)
  simJ <- (M11)/(M01+M10+M11)
  return(simJ)
}

## Perform function for each entry
for (rowi in 1:nrow(newincome.df)) {
  for (rowj in 1:nrow(newincome.df)) {
    jaccard.m[rowi+1, rowj+1] <- jaccard(rowi, rowj)
  }
}

## Convert matrix to dataframe
jacsim.df <- data.frame(jaccard.m)
jacsim.df[1,1] = 0
colnames(jacsim.df) <- jacsim.df[1,]
jacsim.df <- jacsim.df[-1,]
colnames(jacsim.df)[1] <- "ID"

## Remove high similarities of entries similar to itself
for(i in 1:nrow(jacsim.df)) {
  jacsim.df[i, i+1] = 0
}

## Find k most similar entries and add values from matrix to output df
for(col in 2:ncol(jacsim.df)) {
  i = 0
  kmost <- sort(jacsim.df[,col], index.return=TRUE, decreasing = TRUE)
  kmost <- lapply(kmost, `[`, kmost$x %in% head(kmost$x,k))
  for(coscol in 1:k) {
    jaccard.df[col-1,coscol+i+1] <- kmost$ix[coscol]
    jaccard.df[col-1,coscol+i+2] <- kmost$x[coscol]
    i = i + 1
  }
}

## Export cosine similarity output
write.csv(cosine.df, file = "CosineSimilarityOutput.csv")

## Export jaccard similarity output
write.csv(jaccard.df, file = "JaccardSimilarityOutput.csv")


## Exploratory Analysis

### Plots and Other Analysis

## Analyze Class
nrow(subset(income.df, income.df$class==">50K"))
nrow(subset(income.df, income.df$class=="<=50K"))

## Analyze Gender
barplot(table(income.df$gender))

## Analyze Education and Marriage
somecollege <- subset(newincome.df, newincome.df$isEduHighGradOrLess == 1)
bachelors <- subset(newincome.df, newincome.df$`isEduSCol-Bach` == 1)
masters <- subset(newincome.df, newincome.df$isEduMastOrMore == 1)

sum(somecollege$isMarried)/nrow(somecollege)
sum(bachelors$isMarried)/nrow(bachelors)
sum(masters$isMarried)/nrow(masters)

nrow(somecollege)
nrow(bachelors)
nrow(masters)

boxplot(income.df$education_cat~income.df$class)

## Analyze Race and Marriage
whiterace <- subset(newincome.df, newincome.df$isRaceWhite == 1)
otherrace <- subset(newincome.df, newincome.df$isRaceOther == 1)

sum(whiterace$isMarried)/nrow(whiterace)
sum(otherrace$isMarried)/nrow(otherrace)

nrow(whiterace)
nrow(otherrace)

## Analyze Race and Occupation
whiterace <- subset(newincome.df, newincome.df$isRaceWhite == 1)
otherrace <- subset(newincome.df, newincome.df$isRaceOther == 1)

sum(whiterace$isOccBus)/nrow(whiterace)
sum(otherrace$isOccBus)/nrow(otherrace)

nrow(whiterace)
nrow(otherrace)

## Analyze Native Country and Education
UScountry <- subset(newincome.df, newincome.df$isCountryUS == 1)
othercountry <- subset(newincome.df, newincome.df$isCountryOther == 1)

sum(UScountry$isEduHighGradOrLess)/nrow(UScountry)
sum(othercountry$isEduHighGradOrLess)/nrow(othercountry)
sum(UScountry$`isEduSCol-Bach`)/nrow(UScountry)
sum(othercountry$`isEduSCol-Bach`)/nrow(othercountry)
sum(UScountry$isEduMastOrMore)/nrow(UScountry)
sum(othercountry$isEduMastOrMore)/nrow(othercountry)

nrow(UScountry)
nrow(othercountry)

## Analyze Age
age10 <- subset(newincome.df, newincome.df$`isAge10-19` == 1)
age20 <- subset(newincome.df, newincome.df$`isAge20-29` == 1)
age30 <- subset(newincome.df, newincome.df$`isAge30-39` == 1)
age40 <- subset(newincome.df, newincome.df$`isAge40-49` == 1)
age50 <- subset(newincome.df, newincome.df$`isAge50-59` == 1)
age60 <- subset(newincome.df, newincome.df$`isAge60-69` == 1)
age70 <- subset(newincome.df, newincome.df$`isAge70-79` == 1)
age80 <- subset(newincome.df, newincome.df$`isAge80-89` == 1)

nrow(age10)
nrow(age20)
nrow(age30)
nrow(age40)
nrow(age50)
nrow(age60)
nrow(age70)
nrow(age80)

boxplot(income.df$age~income.df$class)

## Analyze Hours per Week
boxplot(income.df$hour_per_week~income.df$class)
hist(income.df$hour_per_week)

## Analyze Class and Marital Status
ggplot(income.df, aes(income.df$class, ..count..)) + geom_bar(aes(fill = income.df$marital_status), position = "dodge")

## Analyze Class and Workclass
ggplot(income.df, aes(income.df$class, ..count..)) + geom_bar(aes(fill = income.df$workclass), position = "dodge")
table(income.df$workclass)/nrow(income.df)

## Analyze Class and Occupation
ggplot(income.df, aes(income.df$class, ..count..)) + geom_bar(aes(fill = income.df$occupation), position = "dodge")
table(income.df$workclass)/nrow(income.df)

## Analyze Class and Relationship
ggplot(income.df, aes(income.df$class, ..count..)) + geom_bar(aes(fill = income.df$relationship), position = "dodge")

## Analyze Class and Race
ggplot(income.df, aes(income.df$class, ..count..)) + geom_bar(aes(fill = income.df$race), position = "dodge")
table(income.df$race)/nrow(income.df)