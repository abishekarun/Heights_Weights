library(caret)
library(MLmetrics)
library(rvest)

# url <- "https://topepo.github.io/caret/available-models.html"
# download.file(url, destfile = "scrapedpage.html")
content <- read_html("scrapedpage.html")%>%
              html_nodes("#DataTables_Table_0")%>%
              html_table()

all_models <- content[[1]][["methodmethodmethod Value"]]
libraries <- content[[1]][["Libraries"]]

data <- read.table("data.txt",header = TRUE,sep=",")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n <- as.data.frame(lapply(data[2:3], normalize))

data_n$Index <- data$Index

names(data_n) <- c("Height","Weight","Index")
require(caTools)
set.seed(101) 
sample = sample.split(data$Index, SplitRatio = .75)
train = subset(data_n, sample == TRUE)
test  = subset(data_n, sample == FALSE)

train$Index <- NULL
test$Index <- NULL

model_names <- c("avNNet", "bagEarth", "bagEarthGCV","bayesglm", "blackboost", 
                 "bridge","brnn","bstTree","ctree","cubist","glmboost",
                 "lars" ,"lars2","rlm","rpart","rpart1SE","rpart2",
                 "svmLinear","svmLinear3","treebag",
                 "xgbLinear")

trainer <- function(x,train,test){
  
#Find the required packages
y <- libraries[all_models == x]
y <- strsplit(y,split = ",")  

#Install the required package

lapply(y, 
       function(a)
         if(!(a %in% rownames(installed.packages())) ) 
          {
            install.packages(a)
            library(a,character.only=TRUE)
          } 
       )

trControl <- caret::trainControl(method  = "cv",
                          number  = 5)

model1 <- caret::train(Weight ~ Height,
                data = train,
                method     = x,
                metric = "RMSE",
                trControl  = trControl)

pred1 <- predict(model1,test[1])

print(x)

return(RMSE(pred1,test[2]))

}

output <- lapply(model_names,trainer,train=train,test=test)

results <- data.frame(Model = model_names, Score = unlist(output))

names(results) <- c("Model","Score")

#############Results##########################
# Model     Score
# 1       avNNet 0.1089705
# 2     bagEarth 0.1089700
# 3  bagEarthGCV 0.1089667
# 4     bayesglm 0.1089596
# 5   blackboost 0.1089497
# 6       bridge 0.1089637
# 7         brnn 0.1089659
# 8      bstTree 0.1089546
# 9        ctree 0.1092093
# 10      cubist 0.1089516
# 11    glmboost 0.1089599
# 12        lars 0.1089600
# 13       lars2 0.1089600
# 14         rlm 0.1089521
# 15       rpart 0.1120896
# 16    rpart1SE 0.1108078
# 17      rpart2 0.1108078
# 18   svmLinear 0.1089280
# 19  svmLinear3 0.1090099
# 20     treebag 0.1099423
# 21   xgbLinear 0.1102646