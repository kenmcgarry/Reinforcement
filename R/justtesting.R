


length(test)
doom <- "absent"

if(grep(rules_kyphosis[1,1],doom)==1){}

pima <- read.table('pima-indians-diabetes.csv',header=TRUE,sep=',')
pima <- na.omit(pima)   # remove any NA (missing values)
str(pima)
pima$Class <- as.character(pima$Class)
pima$Class[pima$Class == "1"] <- "diabetes"
pima$Class[pima$Class == "0"] <- "healthy"
pima$Class <- factor(pima$Class)
str(pima)

save(pima,file="pima.RData")
# model_kyphosis
load("pima.RData")
model_pima <- rpart(Class ~., data = pima, minsplit = 10,cp= .01)
rules_pima <- rpart.plot::rpart.rules(model_pima, cover=TRUE, extra=4)
rpart.plot::rpart.rules(model_pima, cover=TRUE, nn=TRUE)
rpart.plot(model_pima)
caret::varImp(model_pima, scale = TRUE)
