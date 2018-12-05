
library(NLP)
library(tm)
library(SnowballC)
library(qdap)
library("fastDummies")
library(caret)
library(stringr)

df <- read.csv("job_skill_short.csv")

drops <- c("X","Company", "Title")
df <- df[ , !(names(df) %in% drops)]

## ED = engineering & design
## OS = operations & support
## DRTS = developer relations & technical solutions
## Sales = sales 
## Marketing = Marketing
## Business = business
## IO = Internal Operations
df$Category<-gsub('Software Engineering|Hardware Engineering|Network Engineering|User Experience & Design|Product & Customer Support|Technical Infrastructure','ED', df$Category)
df$Category<-gsub('Data Center & Network|IT & Data Management|Manufacturing & Supply Chain|Program Management|Technical Writing','OS', df$Category)
df$Category <-gsub('Technical Solutions|Developer Relations','DRTS',df$Category)
df$Category <-gsub('Sales & Account Management|Sales Operations','Sales',df$Category)
df$Category <-gsub('Marketing & Communications|Partnerships','Marketing',df$Category)
df$Category <-gsub('Business Strategy|Finance|Legal & Government Relations','Business',df$Category)
df$Category <-gsub('Administrative|Real Estate & Workplace Services|People Operations','IO',df$Category)

##TURN INTO FACTORS FOR PROCESSING instead of dummy coding
df["Location"] <- as.factor(df[["Location"]])
df["Category"] <- as.factor(df[["Category"]])

##check balances of new categories
w = table(df$Category)
t = as.data.frame(w)
x <- sort(table(df$Category),decreasing=T)
t

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

text_features <- c("Responsibilities", "Minimum.Qualifications", "Preferred.Qualifications")
df_bow <- df[ , !(names(df) %in% text_features)]
for (x in text_features){
  df_bow <- cbind(df_bow, tfFromFeature(df, x))
}
df_bow

set.seed(123)
trainIndex <- createDataPartition(df_bow[["Category"]], p = .8, list = FALSE, times = 1)

train <- df_bow[trainIndex,]
test <- df_bow[-trainIndex,]

### t-SNE for 2D visualization
install.packages('Rtsne')
install.packages('fastDummies')
library('Rtsne')
library('fastDummies')

colors = rainbow(length(unique(train$Category)))
names(colors) = unique(train$Category)

#df2 <- within(train, {
 # df.ct <- C(df$Location, treatment)
#  print(attributes(df.ct))
#})

#train_tsne <- train[,nums]
#train_tsne
#tsne <- Rtsne(dat, dims = 2, perplexity=30, PCA=FALSE, verbose = getOption("verbose", FALSE), max_iter = 500)
#exeTimeTsne<- system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))
####trying other methods

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

params = data.frame(n.trees = c(120,140,160,180,200), 
                    interaction.depth = c(3,3,3,3,3), 
                    shrinkage = c(0.1, 0.1, 0.1, 0.1, 0.1),
                    n.minobsinnode = c(10,10,10,10,10))
gbmFit1 <- train(Category~., data = df_bow, method = "gbm", trControl = fitControl, tuneGrid = params)
f <-predict(gbmFit1,newdata=test)
gbmFit1
gbmFit_TA<- 1-sum(f!=test$Category)/length(f)
gbmFit_TA

m_pls <- train(Category~., data=df_bow, method="widekernelpls", trControl = fitControl,
               tuneGrid = data.frame(ncomp = c(20,30,40,50)), maxit = 1000)
f2<-predict(m_pls,newdata=test)
##5-fold cv results
m_pls 
m_pls_TA<- 1-sum(f2!=test$Category)/length(f2)
m_pls_TA

m_log <- train(Category~., data=df_bow, method="regLogistic", tuneLength = 3, loss = "L1", trControl = fitControl)

m_log

confusionMatrix(gbmFit1)

confusionMatrix(m_pls)

confusionMatrix(m_log)

df_num = cbind(df_bow, fastDummies::dummy_cols(df_bow["Location"]))

x = df_num[, !(names(df_num) %in% c("Category", "Location"))]
y = df_num["Category"]

for (col in colnames(x)){
    if (is.factor(x[col])){
        x[col] <- as.numeric(x[col])
    }
}
#x is completely numeric matrix
df_num = cbind(x,y)


#Feature selection
for (category in levels(df_bow[["Category"]]))
{
    print(category)
    df_num["Category"] = factor(ifelse(as.character(df_bow$Category) == category, "1", "0"))
    outcomeName = "Category"
    predictorsNames = names(df_num)[names(df_num) != outcomeName]
    svm <- train(df_num[,predictorsNames], df_num[,outcomeName],
                      method='svmLinearWeights2', 
                      trControl=trainControl(method = "repeatedcv", number = 3, repeats = 1),
                      preProc = c("center", "scale"))
    print(svm)
    print(varImp(svm))
}