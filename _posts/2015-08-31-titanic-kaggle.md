---
title: "titanic kaggle"
# fn <- "2015-08-31-boosting-algo.Rmd"
# library(knitr) ; knit(fn)  # produces the md file
# pandoc(fn, format = "docx") # prodces the .docx file
# permalink: If you need your processed blog post URLs to be something other than the default /year/month/day/title.html then you can set this variable and it will be used as the final URL.
date: "2015 July"
tags:
- R
- model 
categories:
- rstat, titanic, kaggle
# published = false =Jekyll will not process the file. Else Rmd file appears as a blog with html file i.e. 2 posts.
published: false 
# output: ioslides_presentation  
output:
  #  html_document:
  # http://rmarkdown.rstudio.com/html_fragment_format.html
  # html_fragment - no title or author -  excl std hdr content . HTML within larger web sites (e.g. blogs).
  html_fragment: 
    toc: true
    # theme: united
    number_sections: true
    keep_md: false
  # rmarkdown::render(fn) # produces table of contents and united theme
  # render(fn, pdf_document()) # library(rmarkdown) ; # knit2html() 
fontsize: 12pt
layout: post
author: "ttmmghmm"
---


## titantic - kaggle
[Source blog]<https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md>


#### Titanic Survival Prediction

[Titanic survival prediction challenge](http://www.kaggle.com/c/titanic-gettingStarted) 
presented by [Kaggle](http://www.kaggle.com).  


### Background
 hypothesize - a passenger's survival was heavily dependent upon two factors:
1. Recognition of the possibility that the ship could sink
2. Access to a lifeboat 

### Getting the Data Into R
Kaggle packaged the data for the Titanic challenge into two csv-format files:
- **train.csv** (data containing attributes and known outcomes [survived or perished] for a subset of the passengers)
- **test.csv** (data containing attributes *without* outcomes for a subset of passengers)

```{r}
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}
```
I've pushed the [Titanic csv files](https://github.com/wehrley/Kaggle_Titanic) to my GitHub account so that I can access the data from anywhere and, more importantly, demonstrate here the reading of data from a web source.  Here are the arguments I will pass into this custom file reading function for the train.csv file:
```{r}
Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv
```
Specifying missing types up front should make the data munging process a bit easier, and while I may have to change the class type for a data frame column or two along the way, I've specified class definitions in a manner which should be most conducive to modeling later.  This leaves me with much cleaner code for reading the csv files.
```{r}
train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw   
```
### Data Munging
data preparation accounts for the bulk of the effort.  
Sometimes my greatest insights have come from this phase, often referred to as data pre-processing. 

missing data in the training set. - use the ```missmap ``` function from the [Amelia package](http://cran.r-project.org/web/packages/Amelia/) to display those. 
```{r}
## map missing data by provided feature
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
```
20 percent of the Age data is missing, 
70 percent of the passengers cannot be linked to a specific cabin number.  

### simple data visualization tools to work 
predict whether or not a passenger survived.  
```{r}
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")
```
the dominant categories in the first three graphs:
* more passengers perished than survived
* about twice as many passengers in 3rd class than in either 1st or 2nd
* male passengers far outnumbered females

Perhaps these are the first clues that the two themes discussed earlier -- women and children first policy, and location on the ship -- could dictate the feature set.
Although the fact that Southampton was the port of embarkation for most passengers doesn't make for a very balanced *Embarked* factor, it might mean something in the final analysis.

Mosaic plots offer an interesting -- and arguably under-utilized -- way to summarize data.  The [vcd package](http://cran.r-project.org/web/packages/vcd/index.html) includes the ``` mosaicplot``` function for creating those.  The following mosaic suggests that traveling class did influence the odds of a passenger's survival.
```{r}
mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6ucVdZYmJMNnotaWs)

Do you recall the earlier bar graph showing that some 2/3 of passengers were males?  That is taken into account by the width of the two rectangles labeled "male" in the mosaic below.  Now look at the *height* of the leftmost light gray rectangle [representing the proportion of females who survived] and compare it to the much shorter light gray rectange [representing proportion of males who survived].  Gender should certainly prove to be a prominent feature in the final model.   
```{r}
mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uWkpVaTRMRGlNSms)

Is it possible that "survival of the fittest" dictated the fate of passengers in certain parts of the ship?  Perhaps, though it isn't apparent at first glance from the boxplot of Age by Survival.
```{r}
boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uQVZ1dThUdkFJV1U)

While passenger survival didn't vary as much across the three ports of embarkation as it did between genders and traveling classes, perhaps the *Embarked* feature will prove useful at some point.  
```{r}
mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")
```
![alt text](http://drive.google.com/uc?export=view&id=0B-yx9UUIpB6uaVdXbmdYeXNRVzA)

Just one more graph, then we'll get to those missing ages.  The [corrgram package](http://cran.r-project.org/web/packages/corrgram/) is the source of a function for creating what is sometimes referred to as a correlogram.  The one shown below confirms a couple of observations already made -- namely, that survival odds drop with class, and age may not prove to be a significant predictor.  Given that the upper class ranks tend to be represented by an older demographic, an inverse correlation between age and traveling class is to be expected.  Although fare and class are closely related, it might be worth throwing the **Fare** feature into the mix as another way to define a 
```{r}
library(plyr)
require(corrgram)
corrgram.data <- df.train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

```
Time to tackle those missing ages.  A common approach to this type of situation is to replacing the missings with the average of the available values.  In this case, that would mean replacing 177 missing **Age** values with 29.7.
```{r}
summary(df.train$Age)
```
Taking that approach would be fine if only a small fraction of the ages were missing.  However, with missings accounting for 20 percent of all **Age** data in a relatively small data set (<900 records), one could justify a search for a more refined method of imputation.  Let's peek again at the list of currently available features:    
```{r}
names(df.train)
```
**PassengerId** is merely a record number, and we already know that splitting the ages solely by **Survived** doesn't reveal much.  A boxplot of ages by passenger traveling class looks interesting

This makes intuitive sense: Passengers in the upper classes (first and second) would tend to be wealthier, and in that period of U.S. history, acquiring wealth usually required a good deal of time (no dot-com kings in their 20s were aboard the Titanic on her maiden voyage).  There are no missing values in **Pclass**, so we could replace the missing age for, say, a third class passenger with the average or median of the available ages for those in ``` Pclass="3"```.  Doing so would be an improvement over assigning 29.7 to all **Age** missings.

Inspection of the next feature -- **Name** -- reveals what could be an even better approach...
```{r}
head(df.train$Name, n=10L)
```
Notice the titles -- Mr., Mrs., Miss., Master. -- following each of the surnames.  The [Wikipedia entry](http://en.wikipedia.org/wiki/Master_%28form_of_address%29) for the [English honorific](http://en.wikipedia.org/wiki/English_honorific) "Master" explains that,
By the late 19th century, etiquette dictated that men be addressed as Mister, and boys as Master."  

The title "Miss" should help with differentiation betweeen younger and older females.  Also, note the way the title appears in the name: The format "Surname, Title. Firstname..." is consistent in **Name** across all records.  I used that pattern to create a custom function which employs a regular expression and the ``` regexpr``` function to extract the title from each name:
```{r}
## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
#  data <- data.frame(Name = tail(df.train$Name, 5))
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  substr(data$Name, title.dot.start+2, title.comma.end-1)
}   
```
Let's fetch the titles, given them their own column in the **df.train** data frame, and look at the uniques.
```{r}
str(df.train$Name)
df.train$Title <- getTitle(df.train)
unique(df.train$Title)

```
To identify the titles which have at least one record with an age missing, I'll use the ``` bystats``` function from the [Hmisc package](http://cran.r-project.org/web/packages/Hmisc/index.html).
```{r}
options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))
#                N Missing Mean Median
# Capt           1       0 70.0   70.0
# Col            2       0 58.0   58.0
# Don            1       0 40.0   40.0
# Dr             6       1 42.0   46.5
# Jonkheer       1       0 38.0   38.0
# Lady           1       0 48.0   48.0
# Major          2       0 48.5   48.5
# Master        36       4  4.6    3.5
# Miss         146      36 21.8   21.0
# Mlle           2       0 24.0   24.0
# Mme            1       0 24.0   24.0
# Mr           398     119 32.4   30.0
# Mrs          108      17 35.9   35.0
# Ms             1       0 28.0   28.0
# Rev            6       0 43.2   46.5
# Sir            1       0 49.0   49.0
# the Countess   1       0 33.0   33.0
# ALL          714     177 29.7   28.0
```
Now I can assign the titles with at least one missing **Age** value to a list...
```{r}
## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
```
...then pass that list to the following custom function I created for imputing the missing ages:  
```{r}
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}
```
I apply the ``` impute``` function from the Hmisc package on a per-title basis to assign the median of the available ages to the missing age(s).  For example, the single record with a missing **Age** value and ``` Title="Dr"``` will be assigned the median of the ages from the 6 records with ``` Title="Dr"``` which *do* have age data.
```{r}
df.train$Age[which(df.train$Title=="Dr")]
```
After doing the age imputations, I check the **Age** data and find that the function seems to have done its job.
```{r}
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
                             titles.na.train)
df.train$Age[which(df.train$Title=="Dr")]
summary(df.train$Age)
```
You may recall that the ``` Embarked``` feature also had at least one missing value.  A summary of that data...
```{r}
summary(df.train$Embarked)
```
...reveals just two missings.  It should be fine to replace those missings with "S", the most common value.
```{r}
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'
```
While there are no missing Fare values, a summary does show at least one ``` Fare=0```...
```{r}
summary(df.train$Fare)
```
(That exceptionally high fare of $512.30 suggests that some tickets were purchased in groups.  We'll address that later.)
A zero fare might have been assigned to a baby.  However, a closer look at records where ``` Fare = 0``` suggests otherwise... 
```{r}
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                          subset(df.train, Fare < 7)$Pclass), 
                          c("Age", "Title", "Pclass", "Fare")]
#     Age Title Pclass Fare
# 264  40    Mr      1  0.0
# 634  30    Mr      1  0.0
# 807  39    Mr      1  0.0
# 816  30    Mr      1  0.0
# 823  38 Noble      1  0.0
# 278  30    Mr      2  0.0
# 414  30    Mr      2  0.0
# 467  30    Mr      2  0.0
# 482  30    Mr      2  0.0
# 675  30    Mr      2  0.0
# 733  30    Mr      2  0.0
# 180  36    Mr      3  0.0
# 272  25    Mr      3  0.0
# 303  19    Mr      3  0.0
# 598  49    Mr      3  0.0
# 379  20    Mr      3  4.0
# 873  33    Mr      1  5.0
# 327  61    Mr      3  6.2
# 844  34    Mr      3  6.4
# 819  43    Mr      3  6.4
# 203  34    Mr      3  6.5
# 372  18    Mr      3  6.5
# 144  19    Mr      3  6.8
# 655  18  Miss      3  6.8
# 412  30    Mr      3  6.9
# 826  30    Mr      3  7.0
# 130  45    Mr      3  7.0
# 805  27    Mr      3  7.0
```
The jump in fares from 0 to the 4-7 range suggests errors.  I replaced the zero **Fare** values with the median fare from the respective passenger class using the imputMedian function introduced earlier.
```{r}
## impute missings on Fare feature with median fare by Pclass
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))
```
I see the titles as more than merely a guide for imputation of missing ages.  A passenger's title can reflect gender, his/her position on the ship (officers & royalty), and access to a lifeboat (where "Master" superceded "Mr").  Making the effort to get the **Title** feature model-ready seems worthwhile.

Recall from the ``` bystats``` results above that the training data contains 17 different titles.  We already know that "Master" and "Mr" should separate the males into roughly two groups by age.  The following script...
```{r}
df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                         "Dr","Don","Jonkheer","the Countess","Mrs",
                         "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(df.train$Age ~ df.train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")
```
...produces [this boxplot](https://drive.google.com/file/d/0B-yx9UUIpB6ubEZ5NU5WSFo1U0E/edit?usp=sharing) (too wide for display here) showing passenger age by title, including shading which illustrates the manner in which I consolidated the titles.  I created and applied a custom function for revaluing the titles, then reclassified **Title** to a factor type, as follows:
```{r}
## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                               "Jonkheer", "Lady", "Major", 
                               "Rev", "Sir"),
                               "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)
```
I assigned the Countess of Rothes, a woman in first class and the sole passenger with a "Countess" title, to the "Mrs" group.  In retrospect, I could have placed her under the "Noble" umbrella.  Given that 91 of the 94 female first-class passengers in the training set survived, I was willing to live with that choice.

All of the work done designing the new **Title** column can be considered a part of **feature engineering**.  The other features I chose to add are generated using custom function ``` featureEngrg```, which can be applied to both the training data in **df.train** and the Kaggle-provided test data in **df.infer**.
```{r}
require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## add remaining features to training data frame
df.train <- featureEngrg(df.train)
```
Some color on the features I've added:
* **Boat.dibs** - assumes all females plus males under 15 get "dibs' on access to a lifeboat.  Filtering by **Title="Master"** was considered, but the highest age in the training data for males addressed as "Master" was just 12, and I wanted to account for male teens with **Title="Mr"** who could pass for a child.
* **Deck** - levels are as shown in the Titanic cross-section displayed previously.  Cabin data provided for just 23 percent of training data records, so it's tough to give this one much emphasis.
* **Side** - subject to the same concern (dearth of data) expressed for Deck

I finish the data munging process by paring down the data frame to the columns I will use in model building.
```{r}
train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family", "Survived")
df.train.munged <- df.train[train.keeps]
```

## Fitting a Model

Later, I will be conducting the predictive modeling effort using the ``` caret``` package.  Created by Max Kuhn of Pfizer Global R&D, ```caret```provides a unified interface for modeling & prediction, and streamlines the model tuning process using resampling.  [The package](http://cran.r-project.org/web/packages/caret/index.html) includes a ``` createDataPartition``` function for splitting data into a training set and a test set (sometimes referred to as a *validation* set) via [stratified random sampling](http://www.investopedia.com/terms/stratified_random_sampling.asp).  In [this presentation](https://drive.google.com/file/d/0B-yx9UUIpB6uaGVQVmhveGNjNDQ/edit?usp=sharing), Kuhn delivered the best explanation I've seen of the decision on how to "spend" the available training data.  His conclusion: 
Statistically, the best course of action would be to use all the data for model building and use statistical methods to get good estimates of error.  From a non-statistical perspective, many consumers of these models emphasize the need for an untouched set of samples to evaluate performance.

I selected an 80/20 split for training data and testing data.  The code:
```{r}
nn <- nrow(df.train.munged)
training.rows <- ind <- sample(x = nn, size = nn*.8)
train.batch <- df.train.munged[+ind, ]
test.batch <- df.train.munged[-ind, ]
# ## split training data into train batch and test batch
# set.seed(23)
# library(caret)
# training.rows <- createDataPartition(df.train.munged$Survived, 
#                                      p = 0.8, list = FALSE)
# train.batch <- df.train.munged[training.rows, ]
# test.batch <- df.train.munged[-training.rows, ]
```
Before I go pouring features into the popular Random Forest method, I'm going to give one of the simplest classification methods a crack at the Titanic prediction challenge.  Logistic regression, which surfaced about 70 years ago, has been used extensively in multiple fields.  I'll start simple by passing essentially the features provided in the raw training data (remember that we combined ``` SibSp``` and ``` Parch``` to form ``` Family```) through the R function for fitting general linearized models.  When entering the model formula, I typically have a habit of listing the features in an order roughly corresponding to what I initially believe their importance will be.  In this case, I've ordered them roughly by the two main themes I discussed earlier (women & children first policy and location on the ship).  By setting the argument ``` family``` to ``` binomial``` with a ``` logit``` link, I'm asking ``` glm( )``` to produce a logistic regression.

```{r}
Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit")
)
Titanic.logit.1
```
