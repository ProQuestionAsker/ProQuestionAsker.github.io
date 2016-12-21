+++
showonlyimage = false
draft = false
image = "projects/Titanic_md_files/Titanic.jpg"
date = "2016-11-05T18:25:22+05:30"
title = "Machine Learning from Disaster"
weight = 0
type = "post"
author = "Amber Thomas"
tags = [
"machine learning",
"data viz"]
+++
Kaggle Playground Competition 

Data exploration and machine learning in RMarkdown.
<!--more-->
Introduction
------------

-   [Introduction](#introduction)
    -   [Loading Necessary Packages](#loading-necessary-packages)
    -   [Importing Data](#importing-data)
-   [Feature Engineering](#feature-engineering)
    -   [Names and Titles](#names-and-titles)
    -   [SibSp and Parch for Family Size](#sibsp-and-parch-for-family-size)
    -   [Ticket Numbers and Travel Groups](#ticket-numbers-and-travel-groups)
-   [Missing Data](#missing-data)
    -   [Missing Fare](#missing-fare)
    -   [Missing Embarkment](#missing-embarkment)
    -   [Missing Age](#missing-age)
-   [Modeling for Survival](#modeling-for-survival)
    -   [Creating trainControl](#creating-traincontrol)
    -   [Fitting a random forest model](#fitting-a-random-forest-model)
    -   [Fitting a glmnet model](#fitting-a-glmnet-model)
    -   [Comparing model fit](#comparing-model-fit)
-   [Predicting Survival](#predicting-survival)
    -   [Preparing the prediction for Kaggle](#preparing-the-prediction-for-kaggle)
    -   [Testing with Kaggle](#testing-with-kaggle)

Introduction
------------

This is my first project on Kaggle and my first attempt at machine learning. I'll do my best to illustrate what I've down and the logic behind my actions, but feedback is very much welcome and appreciated!

### Loading Necessary Packages

``` r
# For data manipulation and tidying
library(dplyr)

# For data visualizations
library(ggplot2)

# For modeling and predictions
library(caret)
library(glmnet)
library(ranger)
library(e1071)
```

### Importing Data

The data were downloaded directly from the [Kaggle Website](https://www.kaggle.com/c/titanic/data). Before binding the training and test sets into a single data file, I added a column called "Dataset" and labelled rows from the training file "train" and rows from the testing file "test".

``` r
train <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = FALSE)
train$Dataset <- "train"

test <- read.csv(file = "test.csv", header = TRUE, stringsAsFactors = FALSE)
test$Dataset <- "test"

full <- bind_rows(train, test)
```

The full dataset can then be inspected:

``` r
str(full)
```

    ## 'data.frame':    1309 obs. of  13 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : chr  "male" "female" "female" "female" ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  "" "C85" "" "C123" ...
    ##  $ Embarked   : chr  "S" "C" "S" "S" ...
    ##  $ Dataset    : chr  "train" "train" "train" "train" ...

It appears that several of these variables should be represented as factors and thus should be reclassified.

``` r
factor_variables <- c("PassengerId", "Survived", "Pclass", "Sex", 
    "Embarked", "Dataset")
full[factor_variables] <- lapply(full[factor_variables], function(x) as.factor(x))
```

We are now left with the following variables:

-   **Passenger ID** : A seemingly unique number assigned to each passenger

-   **Survived** : A binary indicator of survival (0 = died, 1 = survived)

-   **Pclass** : A proxy for socio-economic status (1 = upper, 3 = lower)

-   **Name** : Passenger's Name. For wedded women, her husband's name appears first and her maiden name appears in parentheses

-   **Sex** : General indication of passenger's sex

-   **Age** : Age of passenger (or approximate age). Passengers under the age of 1 year have fractional ages

-   **SibSp** : A count of the passenger's siblings or spouses aboard

-   **Parch** : A count of the passenger's parents or siblings aboard

-   **Ticket** : The number printed on the ticket. The numbering system is not immediately apparent

-   **Fare** : The price for the ticket (presumably in pounds, shillings, and pennies)

-   **Cabin** : Cabin number occupied by the passenger (this field is quite empty)

-   **Embarked** : The port from which the passenger boarded the ship

-   **Dataset** : Whether this particular row was a part of the training or testing dataset

Feature Engineering
-------------------

### Names and Titles

At first glance, the "Name" column doesn't help too much as there are 1307 unique names, however, this column also includes embedded title information that may be of interest. I decided to use [regular expressions](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf) and the `gsub()` functions to extract the titles into a new variable.

``` r
names <- full$Name

titles <- gsub("^.*, (.*?)\\..*$", "\\1", names)

full$Titles <- titles

unique(full$Titles)
```

    ##  [1] "Mr"           "Mrs"          "Miss"         "Master"      
    ##  [5] "Don"          "Rev"          "Dr"           "Mme"         
    ##  [9] "Ms"           "Major"        "Lady"         "Sir"         
    ## [13] "Mlle"         "Col"          "Capt"         "the Countess"
    ## [17] "Jonkheer"     "Dona"

That's a bit more manageable: only 18 unique titles. Time to see how many times each title was used. I decided to make a table separated by sex.

``` r
table(full$Sex, full$Title)
```

    ##         
    ##          Capt Col Don Dona  Dr Jonkheer Lady Major Master Miss Mlle Mme
    ##   female    0   0   0    1   1        0    1     0      0  260    2   1
    ##   male      1   4   1    0   7        1    0     2     61    0    0   0
    ##         
    ##           Mr Mrs  Ms Rev Sir the Countess
    ##   female   0 197   2   0   0            1
    ##   male   757   0   0   8   1            0

It looks like Captain, Don, Dona, Jonkheer, Lady, Madame, Sir and the Countess were each only used once. I'll leave Captain separate, but the rest should be combined with similar categories.

-   **Don** : A Spanish/Portuguese/Italian title used with, but not instead of, a name.
-   **Dona** : Female version of "Don"
-   **Jonkheer** : Dutch honorific of nobility
-   **Lady** : English honorific of nobility
-   **Madame** : French, polite form of address for a woman
-   **Sir** : Honorific address (male)
-   **the Countess** : Rank of nobility (female)

It seems that most of the rarely used titles indicate some form of nobility. That's easy to check with another table comparing `Pclass` and `Titles`.

``` r
table(full$Pclass, full$Titles)
```

    ##    
    ##     Capt Col Don Dona  Dr Jonkheer Lady Major Master Miss Mlle Mme  Mr Mrs
    ##   1    1   4   1    1   6        1    1     2      5   60    2   1 159  77
    ##   2    0   0   0    0   2        0    0     0     11   50    0   0 150  55
    ##   3    0   0   0    0   0        0    0     0     45  150    0   0 448  65
    ##    
    ##      Ms Rev Sir the Countess
    ##   1   0   0   1            1
    ##   2   1   8   0            0
    ##   3   1   0   0            0

Since Don, Jonkheer, and Sir are all of similar usage, and each represent only one first-class man, I combined them into the category "Sir". Dona, Lady, Madame, and the Countess each only represent one first-class woman, so I combined them into the category "Lady". These values were substituted using the `gsub` function.

``` r
full$Titles <- gsub("Dona|Lady|Madame|the Countess", "Lady", 
    full$Titles)
full$Titles <- gsub("Don|Jonkheer|Sir", "Sir", full$Titles)

unique(full$Titles)
```

    ##  [1] "Mr"     "Mrs"    "Miss"   "Master" "Sir"    "Rev"    "Dr"    
    ##  [8] "Mme"    "Ms"     "Major"  "Lady"   "Mlle"   "Col"    "Capt"

**Warning**: If you are planning to replicate the above substitution without any RegEx, make sure that you substitute "Dona" before substituting "Don"! Otherwise, "Dona" becomes "Sira" (as the "Don" part was replaced with "Sir") and your second substitution won't find or replace "Dona".

Lastly for the titles, they should be factors, not character strings.

``` r
full$Titles <- as.factor(full$Titles)
```

These titles could certainly be condensed more, but for the time being, I am going to leave them separated as is.

I have some thoughts about wanting to split up the names further to find family groups, but since many familial relationships (cousins, nieces/nephews, aunts/uncles, fiances, mistresses, in-laws, children with a nanny or close friends) aren't reported in any way in this data set, I'll have to think a little longer about the most appropriate way to find actual family groups.

### SibSp and Parch for Family Size

Since the SibSp and Parch variables each give some indication as to close family members that were also aboard the ship, it would make sense to calculate family size as a combination of SibSp, Parch and the passenger in question.

``` r
full <- mutate(full, FamilySize = SibSp + Parch + 1)
```

Let's visualize family size

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-11-1.png" class="img-responsive" style="display: block; margin: auto;" />

Wow! Lots of people without immediate family with them. Perhaps these people were traveling with other family members/friends that weren't captured in the SibSp / Parch variables.

### Ticket Numbers and Travel Groups

I've decided that another possible way to discern groups that were travelling together is to look at the ticket numbers. It appears that families or groups who purchased their tickets together have identical ticket numbers, thus quantifying the number of families or traveling groups. A quick look at the unique ticket numbers indicates there are 929 of them in the full data set (out of a possible 1309 passengers).

It seems the easiest way to separate these tickets is to create a new column:

``` r
full$TravelGroup <- NA
```

Then arrange the data by ticket number using the `arrange()` function from the `dplyr` package.

``` r
full2 <- arrange(full, Ticket)
```

Take a look at the first few rows of results

``` r
head(full2)
```

    ##   PassengerId Survived Pclass
    ## 1         258        1      1
    ## 2         505        1      1
    ## 3         760        1      1
    ## 4         263        0      1
    ## 5         559        1      1
    ## 6         586        1      1
    ##                                                       Name    Sex Age
    ## 1                                     Cherry, Miss. Gladys female  30
    ## 2                                    Maioni, Miss. Roberta female  16
    ## 3 Rothes, the Countess. of (Lucy Noel Martha Dyer-Edwards) female  33
    ## 4                                        Taussig, Mr. Emil   male  52
    ## 5                   Taussig, Mrs. Emil (Tillie Mandelbaum) female  39
    ## 6                                      Taussig, Miss. Ruth female  18
    ##   SibSp Parch Ticket  Fare Cabin Embarked Dataset Titles FamilySize
    ## 1     0     0 110152 86.50   B77        S   train   Miss          1
    ## 2     0     0 110152 86.50   B79        S   train   Miss          1
    ## 3     0     0 110152 86.50   B77        S   train   Lady          1
    ## 4     1     1 110413 79.65   E67        S   train     Mr          3
    ## 5     1     1 110413 79.65   E67        S   train    Mrs          3
    ## 6     0     2 110413 79.65   E68        S   train   Miss          3
    ##   TravelGroup
    ## 1          NA
    ## 2          NA
    ## 3          NA
    ## 4          NA
    ## 5          NA
    ## 6          NA

To verify that this is working so far, I inspected the first ticket number listed (110152) on the [Titanic Passenger and Crew](https://www.encyclopedia-titanica.org/titanic-passengers-and-crew/) table of Encyclopedia Titanica. That dataset lists the same three passengers owned those tickets, verified that the 3 women were traveling together, and indicated that two of the women (Miss Gladys Cherry and the Countess of Rothes) were cousins and the 3rd woman in their party (Miss Roberta Elizabeth Mary Maioni) was their servant. Looking at unique Ticket ID may be the only way to know that these women were travelling together. I'm feeling good that unique ticket numbers may be a good way to look at family/traveling groups, so full steam ahead!

Next, I need to generate a "TravelGroup" number. To do this, I will use the `transform` function looking for matching unique Ticket numbers.

``` r
full2 <- (transform(full2, TravelGroup = match(Ticket, unique(Ticket))))

# Can't forget to make those Travel Groups into factors!
full2$TravelGroup <- as.factor(full2$TravelGroup)
```

This generates 929 unique Travel Groups, which is the same number of unique Ticket numbers. So far so good.

It may also be of interest to look at group size. We can generate this using the `group_by()` and `mutate` functions in `dplyr`.

``` r
full3 <- full2 %>% group_by(TravelGroup) %>% mutate(GroupSize = n()) %>% 
    ungroup()
```

How does Travel Group Size compare to Family Group Size that we calculated earlier?

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-17-1.png" class="img-responsive" style="display: block; margin: auto;" /><img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-17-2.png" class="img-responsive" style="display: block; margin: auto;" />

They look pretty close, again showing that most people were potentially travelling alone.

Now to check if those with the unique Ticket IDs were really travelling alone:

``` r
filtered <- filter(full3, GroupSize == 1)

# How many were listed as being onboard with siblings or
# spouses?
fSibSp <- filtered[filtered$SibSp > 0, ]
nrow(fSibSp)
```

    ## [1] 42

``` r
# How many were listed as being onboard with parents or
# children?
fParch <- filtered[filtered$Parch > 0, ]
nrow(fParch)
```

    ## [1] 16

``` r
# How many of those people overlapped both groups?
sum(fSibSp$PassengerId %in% fParch$PassengerId)
```

    ## [1] 8

Oops! Looks like we were counting 50 passengers as solo-riders when they were actually riding with family. Given the current information, I'm not sure how to know to tell who was travelling together. Manually summing SibSp and Parch to estimate family size doesn't account for other types of groups that were travelling together and looking only at unique Ticket Number doesn't account for some travelling with family who purchased a separate ticket. I could override the GroupSize for those 50 that weren't actually riding solo, but their TravelGroup number won't be accurate. For the time being, I'm going to leave TravelGroup and GroupSize as is.

Missing Data
------------

At this point, I'm feeling pretty good about the Feature Engineering that I've done so far. Time to correct for missing data!

Let's take a look at what has NA values:

``` r
summary(full3)
```

    ##   PassengerId   Survived   Pclass      Name               Sex     
    ##  1      :   1   0   :549   1:323   Length:1309        female:466  
    ##  2      :   1   1   :342   2:277   Class :character   male  :843  
    ##  3      :   1   NA's:418   3:709   Mode  :character               
    ##  4      :   1                                                     
    ##  5      :   1                                                     
    ##  6      :   1                                                     
    ##  (Other):1303                                                     
    ##       Age            SibSp            Parch          Ticket         
    ##  Min.   : 0.17   Min.   :0.0000   Min.   :0.000   Length:1309       
    ##  1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.000   Class :character  
    ##  Median :28.00   Median :0.0000   Median :0.000   Mode  :character  
    ##  Mean   :29.88   Mean   :0.4989   Mean   :0.385                     
    ##  3rd Qu.:39.00   3rd Qu.:1.0000   3rd Qu.:0.000                     
    ##  Max.   :80.00   Max.   :8.0000   Max.   :9.000                     
    ##  NA's   :263                                                        
    ##       Fare            Cabin           Embarked  Dataset        Titles   
    ##  Min.   :  0.000   Length:1309         :  2    test :418   Mr     :757  
    ##  1st Qu.:  7.896   Class :character   C:270    train:891   Miss   :260  
    ##  Median : 14.454   Mode  :character   Q:123                Mrs    :197  
    ##  Mean   : 33.295                      S:914                Master : 61  
    ##  3rd Qu.: 31.275                                           Dr     :  8  
    ##  Max.   :512.329                                           Rev    :  8  
    ##  NA's   :1                                                 (Other): 18  
    ##    FamilySize      TravelGroup     GroupSize     
    ##  Min.   : 1.000   779    :  11   Min.   : 1.000  
    ##  1st Qu.: 1.000   105    :   8   1st Qu.: 1.000  
    ##  Median : 1.000   776    :   8   Median : 1.000  
    ##  Mean   : 1.884   336    :   7   Mean   : 2.102  
    ##  3rd Qu.: 2.000   455    :   7   3rd Qu.: 3.000  
    ##  Max.   :11.000   460    :   7   Max.   :11.000  
    ##                   (Other):1261

Looks like we are missing values in the "Survived" variable (which is to be expected since this is a combination of the training and test datasets), "Fare", "Embarked", and quite a few in the "Age" column. We'll start with "Fare".

### Missing Fare

Which passenger has no fare information?

``` r
full3[(which(is.na(full3$Fare))), 1]
```

    ## # A tibble: 1 × 1
    ##   PassengerId
    ##        <fctr>
    ## 1        1044

Looks like Passenger number 1044 has no listed Fare.

``` r
# Resort the dataset by Passenger Number
full4 <- arrange(full3, PassengerId)

# Where did this passenger leave from? What was their class?
full4[1044, c(3, 12)]
```

    ## # A tibble: 1 × 2
    ##   Pclass Embarked
    ##   <fctr>   <fctr>
    ## 1      3        S

Looks like he left from 'S' (Southampton) as a 3rd class passenger. Let's see what other people of the same class and embarkment port paid for their tickets.

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-22-1.png" class="img-responsive" style="display: block; margin: auto;" />

``` r
full4 %>% filter(Pclass == "3" & Embarked == "S") %>% summarise(missing_fare = median(Fare, 
    na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   missing_fare
    ##          <dbl>
    ## 1         8.05

Looks like the median cost for a 3rd class passenger leaving out of Southampton was 8.05. That seems like a logical value for this passenger to have paid.

Time to replace that NA with 8.05

``` r
full4$Fare[1044] <- 8.05

summary(full4$Fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   7.896  14.450  33.280  31.280 512.300

Hooray! No more NA values for Fare.

### Missing Embarkment

Which passengers have no listed embarkment port?

``` r
full4$Embarked[full4$Embarked == ""] <- NA

full4[(which(is.na(full4$Embarked))), 1]
```

    ## # A tibble: 2 × 1
    ##   PassengerId
    ##        <fctr>
    ## 1          62
    ## 2         830

Ok, so Passenger numbers 62 and 830 are each missing their embarkment ports. Let's look at their class of ticket and their fare.

``` r
full4[c(62, 830), c(1, 3, 10)]
```

    ## # A tibble: 2 × 3
    ##   PassengerId Pclass  Fare
    ##        <fctr> <fctr> <dbl>
    ## 1          62      1    80
    ## 2         830      1    80

Both passengers had first class tickets that they spent 80 (pounds?) on. Let's see the embarkment ports of others who bought similar kinds of tickets.

``` r
full4 %>% group_by(Embarked, Pclass) %>% filter(Pclass == "1") %>% 
    summarise(mfare = median(Fare), n = n())
```

    ## Source: local data frame [4 x 4]
    ## Groups: Embarked [?]
    ## 
    ##   Embarked Pclass   mfare     n
    ##     <fctr> <fctr>   <dbl> <int>
    ## 1        C      1 76.7292   141
    ## 2        Q      1 90.0000     3
    ## 3        S      1 52.0000   177
    ## 4       NA      1 80.0000     2

Looks like the median price for a first class ticket departing from 'C' (Charbourg) was 77 (in comparison to our 80). While first class tickets departing from 'Q' were only slightly more expensive (median price 90), only 3 first class passengers departed from that port. It seems far more likely that passengers 62 and 830 departed with the other 141 first-class passengers from Charbourg.

Now to replace their NA values with 'C'. And drop any unused levels.

``` r
# Assign empty embark ports to 'C'
full4$Embarked[c(62, 830)] <- "C"

# Drop unused levels (since there should be no more blanks)
full4$Embarked <- droplevels(full4$Embarked)

# Check to make sure there are no NA's or blanks
levels(full4$Embarked)
```

    ## [1] "C" "Q" "S"

Yay! No more NA values for Embarked.

### Missing Age

This one is a bit trickier. 263 passengers have no age listed. Taking a median age of all passengers doesn't seem like the best way to solve this problem, so it may be easiest to try to predict the passengers' age based on other known information.

I've decided to use the `caret` package for predicting age.

Generate a random forest model on the full dataset (minus the age values that are NA)

``` r
predicted_age <- train(Age ~ Pclass + Sex + SibSp + Parch + Fare + 
    Embarked + Titles + FamilySize + GroupSize, tuneGrid = data.frame(mtry = c(2, 
    3, 7)), data = full4[!is.na(full4$Age), ], method = "ranger", 
    trControl = trainControl(method = "cv", number = 10, repeats = 10, 
        verboseIter = TRUE), importance = "impurity")
```

Let's look at what factors were the most important in modeling age:

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-30-1.png" class="img-responsive" style="display: block; margin: auto;" />

Wow! Looks like it was a good idea to split out Titles!

Now to use this information to predict the ages of passengers with missing ages and filling in their NA values.

``` r
full4$Age[is.na(full4$Age)] <- predict(predicted_age, full4[is.na(full4$Age), 
    ])

# Check the summary to make sure there are no more NA values
summary(full4$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.17   22.00   28.50   29.72   37.00   80.00

Let's take a quick look at the age distribution of passengers with originally known ages, and the age distribution of the entire group (known and predicted ages) to make sure we didn't terribly skew the distribution.

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-32-1.png" class="img-responsive" style="display: block; margin: auto;" /><img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-32-2.png" class="img-responsive" style="display: block; margin: auto;" />

Hmm, seems to have shifted a bit, but that could be due to a greater lack of age information collected for middle-aged passengers.

Modeling for Survival
---------------------

First things first, I need to split out the test and training data back into separate data sets, now called `train_complete` and `test_complete`.

``` r
train_complete <- full4[full4$Dataset == "train", ]
test_complete <- full4[full4$Dataset == "test", ]
```

Because I plan on using the `caret` package for all of my modeling, I'm going to generate a standard `trainControl` so that those tuning parameters remain consistent throughout the various models.

### Creating trainControl

I will create a system that will perform 10 repeats of a 10-Fold cross-validation of the data.

``` r
myControl <- trainControl(method = "cv", number = 10, repeats = 10, 
    verboseIter = TRUE)
```

### Fitting a random forest model

The first type of model I'd like to use is a random forest model (using the `ranger` and `caret` packages).

``` r
rf_model <- train(Survived ~ Age + Pclass + Sex + SibSp + Parch + 
    Fare + Embarked + Titles + FamilySize + TravelGroup + GroupSize, 
    tuneGrid = data.frame(mtry = c(2, 5, 8, 10, 15)), data = train_complete, 
    method = "ranger", trControl = myControl, importance = "impurity")
```

### Fitting a glmnet model

Next, we'll try a glmnet model, also from the `caret` package.

``` r
glm_model <- train(Survived ~ Age + Pclass + Sex + SibSp + Parch + 
    Fare + Embarked + Titles + FamilySize + TravelGroup + GroupSize, 
    method = "glmnet", tuneGrid = expand.grid(alpha = 0:1, lambda = seq(1e-04, 
        1, length = 20)), data = train_complete, trControl = myControl)
```

### Comparing model fit

Now that we have a random forest model and a glmnet model, it's time to compare their fit.

``` r
# Create a list of models
models <- list(rf = rf_model, glmnet = glm_model)

# Resample the models
resampled <- resamples(models)

# Generate a summary
summary(resampled)
```

    ## 
    ## Call:
    ## summary.resamples(object = resampled)
    ## 
    ## Models: rf, glmnet 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##          Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## rf     0.7667  0.7893 0.8146 0.8250  0.8669 0.8889    0
    ## glmnet 0.7889  0.8118 0.8531 0.8418  0.8624 0.9101    0
    ## 
    ## Kappa 
    ##          Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## rf     0.5215  0.5375 0.6083 0.6239  0.7092 0.7613    0
    ## glmnet 0.5535  0.6026 0.6842 0.6617  0.7027 0.8117    0

``` r
# Plot the differences between model fits
dotplot(resampled, metric = "Accuracy")
```

<img src="../Titanic_md_files/figure-markdown_github/unnamed-chunk-37-1.png" class="img-responsive" style="display: block; margin: auto;" />

Looks like the glmnet model is slightly more accurate than the random forest model, so we'll use that to predict the survival rate.

Ok, time to make some predictions.

Predicting Survival
-------------------

Although I generated two models above, the glmnet model provided higher accuracy, so I'll use that model to predict survival in the test set.

``` r
# Reorder the data by Passenger ID number
test_complete <- test_complete %>% arrange(PassengerId)

# Make predicted survival values
my_prediction <- predict(glm_model, test_complete)
```

### Preparing the prediction for Kaggle

The instructions on Kaggle indicate that they are expecting a csv file with 2 columns: Passenger ID and Survived. I need to make sure that my data are arranged properly.

``` r
# Create a data frame with two columns: PassengerId &
# Survived where Survived contains my predictions.
my_solution_5 <- data.frame(PassengerID = test$PassengerId, Survived = my_prediction)

# Write the solution to a csv file
write.csv(my_solution_5, file = "my_solution_5.csv", row.names = FALSE)
```

### Testing with Kaggle

Looks like that submission scored 0.80383! Not bad!!

*I'd love to hear any feedback you may have on this process. Thanks in advance!*
