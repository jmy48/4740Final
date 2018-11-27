
df <- read.csv("job_skill_short.csv")

library(NLP)
library(tm)
library(SnowballC)
library(qdap)
library("fastDummies")
library(caret)

#Company: unused
#Title: Keywords in job title good predictor for Category. Use TFIDF
#Category: Response
#Location: Categorical variable
#Responsibilities: word embeddings
#Minimum Qualifications: extract keywords of tools, what kind of degree, years of experience
#Preferred Qualifications: Same as minimum qualifications

#First try: TF-IDF of each feature. Not very interpretable
#Second try: word embeddings, neural nets, LSTM responsibilities and qualifications, 
#experience/tools extraction from qualifications

drops <- c("X","Company")
df <- df[ , !(names(df) %in% drops)]
# df <- dummy_cols(df, select_columns = c('Location'), remove_first_dummy = FALSE,
#   remove_most_frequent_dummy = FALSE)

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
corpus <- tfFromFeature(df, "Title")

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
gbmFit1 <- train(Category~., data = train, method = "gbm", trControl = fitControl, verbose = FALSE, tuneGrid = params)
gbmFit1

m_pls <- train(Category~., data=train, method="widekernelpls", trControl = fitControl,
               tuneGrid = data.frame(ncomp = c(35,38,41,44,47)), maxit = 1000)
m_pls

m_log <- train(Category~., data=train, method="regLogistic", tuneLength = 7, loss = L1, trControl = fitControl)
m_log

m_gauss <- train(Category~., data=train, method='gaussprLinear', tuneLength = 5)
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


