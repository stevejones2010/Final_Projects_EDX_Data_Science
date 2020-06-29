###########################
# Create dataset
###########################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("https://www.kaggle.com/pavanraj159/predicting-a-pulsar-star/download", dl)

star <- unzip(dl,)
dl


# d
dl <- read.csv("C:/Users/stevefuckyou/Desktop/pulsar_stars.csv")


# Tidy up colmn names
colnames(dl) <-  c("MeanIP","SdIP","ExcKurtIP","SkewIP","MeanCurve","SdCurve","ExcKurtCurve","SkewCurve","tClass")

as.factor(levels(dl$tClass,c(0,1)))



# SPlit data into Traina and test set
test_index <- createDataPartition(y = dl$tClass, times = 1, p = 0.1, list = FALSE)
train_set <- dl[-test_index,]
test_set <- dl[test_index,]

head(dl)

# No. of columns
ncol(dl)

#No. of rows
nrow(dl)

#No. of pulsars
sum(dl$tClass)

# Summaries of Mean, Median, 1st & 3rd quartile and min and max
summary(dl)

table(!is.na(dl))

y <- dl$tClass
y <- train_set$tClass

y<- as.factor(ifelse(y == 0,"No","Yes"))

class(y)

Length(dl)

train_knn <- train(y ~ ., method = "knn", preProc = "pca", data = train_set)

