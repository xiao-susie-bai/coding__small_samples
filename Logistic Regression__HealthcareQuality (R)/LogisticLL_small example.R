#Logistic Regression example

#read dataset of health care quality
quality = read.csv("~/Desktop/quality.csv")
str(quality)

#compute the baseline accuracy first (naive rule)
BaseAccuracy = max(sum(quality$PoorCare)/nrow(quality), sum(1-quality$PoorCare)/nrow(quality))
BaseAccuracy


#install package of caTools: used for random sampling[***usually for training & testing data splitting!!!]
install.packages("caTools", repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
library(caTools)

#then split data set into training and testing data 'randomly'
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split        #the result "split" is a logical vector with corresponding portion of TRUE or FALSE (indicating whether the entry goes to TRAINING or TESTING)
#table(split)       #summary statistics for training-testing split outcome

#create training and testing data
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# building the logistic regression model
qualitylog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)      #logistic regression--use GLM model with specification "family = binomial"!!!
summary(qualitylog)

#make predictions on the training data to verify the model effectiveness
predictTrain = predict(qualitylog, type = 'response')
predictTrain
summary(predictTrain)

#observe the changes of CONFUSION MATRIX when t thresholds are different:
#get the CONFUSION MATRIX for t threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
#get the CONFUSION MATRIX for t threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
#get the CONFUSION MATRIX for t threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)


#ROCR (Receiver Operator Characteristic Curve analysis):
#install ROCR("Receiver Operator Characteristic") package
install.packages("ROCR", repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
library(ROCR)

#preparation by creating the prediction object
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)     #*be careful about the function semantics
#Performance function
ROCRperformance = performance(ROCRpred, "tpr", "fpr")
#plot ROC curve now!
plot(ROCRperformance)
#plotting option: add colors
plot(ROCRperformance, colorize = TRUE)
#plotting option: add t threshold labels
plot(ROCRperformance, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2, 1.7))     #pending: difficult plotting technique!!!


#AUC analysis:
#install AUC package first
install.packages('AUC', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
library(AUC)

#transform the predicted probability into a numeric type variable for "roc" function use later
predictTrain.num = as.numeric(predictTrain)
PoorCare_train.fac = as.factor(qualityTrain$PoorCare)
#then create an ROC object first for calculating AUC later
ROC_train = roc(predictTrain.num, PoorCare_train.fac)
#calculate the AUC!
AUC_train = auc(ROC_train)
AUC_train









