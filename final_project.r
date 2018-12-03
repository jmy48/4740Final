
library(NLP)
library(tm)
library(SnowballC)
library(qdap)
library("fastDummies")
library(caret)
library(stringr)

df <- read.csv("job_skill_short.csv")

drops <- c("X","Company")
df <- df[ , !(names(df) %in% drops)]

df["Location"] <- as.factor(df[["Location"]])
df["Category"] <- as.factor(df[["Category"]])

#initialize new columns 
df$min_bs <-0
df$min_ms <- 0 
df$min_mba <-0 
df$min_phd <-0 
df$min_jd <-0

df$pref_bs <-0
df$pref_ms <- 0 
df$pref_mba <-0 
df$pref_phd <-0 
df$pref_jd <-0
##minimum qualifications columns
df$min_bs <- ifelse(grepl("BS|BA|Bachelors|Bachelor's", df$Minimum.Qualifications), 1, 0)
df$min_ms <- ifelse(grepl("MS|MA|MST|Masters|Master's|MFA",df$Minimum.Qualifications),1,0)
df$min_mba <- ifelse(grepl("MBA",df$Minimum.Qualifications),1,0)
df$min_phd <-ifelse(grepl("PhD|Ph.D",df$Minimum.Qualifications),1,0)
df$min_jd <-ifelse(grepl("JD|J.D.",df$Minimum.Qualifications),1,0)

## preferred qualification columns
df$pref_bs <- ifelse(grepl("BS|BA|Bachelors|Bachelor's", df$Preferred.Qualifications), 1, 0)
df$pref_ms <- ifelse(grepl("MS|MA|MST|Masters|Master's|MFA",df$Preferred.Qualifications),1,0)
df$pref_mba <- ifelse(grepl("MBA",df$Preferred.Qualifications),1,0)
df$pref_phd <-ifelse(grepl("PhD|Ph.D",df$Preferred.Qualifications),1,0)
df$pref_jd <-ifelse(grepl("JD|J.D.",df$Preferred.Qualifications),1,0)

regexp <- "\\d+\\b(?=\\syears)"
df$min_years_exp<-as.numeric(str_extract(df$Minimum.Qualifications, regexp))
df$min_years_exp[is.na(df$min_years_exp)]<-0

regexp2 <- "\\d+\\b(?=\\syears)" 
df$pref_years_exp<-as.numeric(str_extract(df$Preferred.Qualifications, regexp2))
df$pref_years_exp[is.na(df$pref_years_exp)]<-0

tfFromFeature <- function(source_df, colname){
    corpus = VCorpus(VectorSource(source_df[[colname]]))
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, stopwords("en"))

    corpus = tm_map(corpus, stemDocument)
    df = as.matrix(removeSparseTerms(DocumentTermMatrix(corpus), .99))
    colnames(df) <- lapply(colnames(df), function(word){return(paste(colname, "_", word, collapse =""))})
    return(df)
}

text_features <- c("Title", "Responsibilities", "Minimum.Qualifications", "Preferred.Qualifications")
df_bow <- df[ , !(names(df) %in% text_features)]
for (x in text_features){
    df_bow <- cbind(df_bow, tfFromFeature(df, x))
}
df_bow

set.seed(123)
trainIndex <- createDataPartition(df_bow[["Category"]], p = .8, list = FALSE, times = 1)

train <- df_bow[trainIndex,]
test <- df_bow[-trainIndex,]

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

params = data.frame(n.trees = c(60,80,100,120,140), 
                    interaction.depth = c(3,3,3,3,3), 
                    shrinkage = c(0.1, 0.1, 0.1, 0.1, 0.1),
                    n.minobsinnode = c(10,10,10,10,10))
gbmFit1 <- train(Category~., data = train, method = "gbm", trControl = fitControl, tuneGrid = params)
gbmFit1

m_pls <- train(Category~., data=train, method="widekernelpls", trControl = fitControl,
               tuneGrid = data.frame(ncomp = c(50,55,60,65,70,75,80,85,90)), maxit = 1000)
m_pls

m_log <- train(Category~., data=train, method="regLogistic", tuneLength = 10, loss = L1, trControl = fitControl)
m_log

m_gauss <- train(Category~., data=train, method='gaussprLinear')
m_gauss

categories = levels(df_bow$Category)
models = vector(,length(categories))
for (i in 1:length(categories)){
    cat_name <- paste("Category_", categories[i])
    y <- ifelse(as.character(df_bow$Category) == categories[i], 1, 0)
    y <- as.factor(y)
    x <- df_bow[ , !(names(df_bow) == "Category")]
    ith_model <- train(x = x, y = y,
                       method='adaboost', trControl = fitControl, tuneLength = 1, verbose = TRUE)
    models[i] <- ith_model
    print(ith_model)
}

logistic_l1 <- train(Category~., data=train, method='regLogistic', 
                   trControl = fitControl, tuneGrid = data.frame(cost = c(2.0), epsilon = c(.001), loss = c("L1")))



gbmImp <- varImp(gbmFit1, scale = FALSE)
gbmImp

svmFit <- train(Category~., data = train, 
                 method = "svmRadial", 
                 trControl = fitControl,
                 preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")

logistic_l1$finalModel

coeffs = logistic_l1$finalModel$W
categories = levels(df_bow$Category)
selected_vars = setNames(data.frame(matrix(ncol = length(categories), nrow = 1)), categories)
for (feature in coeffs){
#    print(feature)
}

features = colnames(coeffs)
#logistic_l1$finalModel$obsLevels
