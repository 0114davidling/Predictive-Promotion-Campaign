# Install required R packages
install.packages("corrplot") # for correlation plot corplot()
install.packages("pdp") # for partial dependency plots
install.packages("ggplot2") # for plotting above
install.packages("dplyr")
install.packages("car")
install.packages("forcats")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("nnet")
install.packages("randomForest")
# Load required R packages
library("dplyr")
library("car")
library("forcats")
library("effects")
library("gplots")
library("rpart")
library("rpart.plot")
library("nnet")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
#Source BCA Functions
source("BCA_functions_source_file.R")
QK <- read.csv("QK_2020.csv")
View(QK)
glimpse(QK)
summary(QK)
variable.summary(QK)
#Clean data
QK$Disc <- as.character(QK$Disc)
QK$Disc <- if_else(is.na(QK$Disc), "NoDiscount", QK$Disc)
QK$Disc <- as.factor(QK$Disc)
#QK$Weeks3Meals <- NULL
QK$Weeks3Meals <- if_else(is.na(QK$Weeks3Meals), 0, as.double(QK$Weeks3Meals))
rownames(QK) <- QK$custid
QK$X <- NULL
QK$custid <- NULL
QK$Title <- NULL
QK <- na.omit(QK)
QK$LastOrder <- as.Date(QK$LastOrder,format = "%Y-%m-%d")
QK$DaySinceLastOrder <- as.integer(as.Date("2018-03-05")-QK$LastOrder)
QK$SUBSCRIBE.Num <- if_else(QK$SUBSCRIBE == "Y",1,0)

#Correlation
corrMatrix <- cor(select_if(QK, is.numeric)) # see ?dplyr::select_if
# temporarily reduce the number of output digits for easier inspection
options(digits = 2)
corrMatrix
options(digits = 7) # then reset output digits
# Visualize correlation
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

#Explore Non-linear relationship
scatterplot(SUBSCRIBE.Num ~ NumDeliv, data = QK) #linear
scatterplot(SUBSCRIBE.Num ~ DA_Income, data = QK) # concave
QK$DA_Income.Log <- log(QK$DA_Income+1)
scatterplot(SUBSCRIBE.Num ~ DA_Income.Log, data = QK)
QK$NumDeliv.Bin <- binVariable(QK$NumDeliv, bins = 5,
            method = "proportions",
            labels = NULL)
plotmeans(SUBSCRIBE.Num ~ NumDeliv.Bin, data = QK) # U-shape
QK$NumDeliv.Sqrd <- (QK$NumDeliv)^2

#paste(names(QK), collapse = " + ")

#Logistic Model
QKLogis <- glm(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 
               + DA_Single + NumDeliv + NumDeliv.Sqrd + DA_Income.Log
               + MealsPerDeliv + Healthy + Veggie + Meaty + 
                 Special + DaySinceLastOrder,
               data = filter(QK, Sample =="Estimation"),
               family = binomial(logit))
summary(QKLogis) #AIC: 474.73
# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2 <- 1 - (QKLogis$deviance / QKLogis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3))


#Run a stepwise regression using the "QKlogis" model
QKStepLogis <- step(QKLogis, direction = "both")
#
summary(QKStepLogis) #AIC: 463.6
# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2 <- 1 - (QKStepLogis$deviance / QKStepLogis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3))

# all variables in random forest
QKForestAllv <- randomForest(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 
                             + DA_Single + NumDeliv + NumDeliv.Sqrd + DA_Income.Log
                             + MealsPerDeliv + Healthy + Veggie + Meaty + 
                               Special + DaySinceLastOrder,
                              data = filter(QK, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)
##Variable importance
varImpPlot(QKForestAllv,type = 2,
           main="QKForestAllv", # title
           cex =0.7) # font size

# stepwise variables in random forest
QKForestStepv <- randomForest(formula = SUBSCRIBE ~  DA_Income.Log + DA_Under20 
                              + NumDeliv + NumDeliv.Sqrd + MealsPerDeliv + Veggie 
                              + DaySinceLastOrder,
                              data = filter(QK, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)

# Compare forests and regression models
lift.chart(modelList = c("QKLogis","QKStepLogis", "QKForestAllv", "QKForestStepv"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.1954023, type = "cumulative",
           sub = "Validation")
#Since we Random Forest outperformes others, there're some nonlinearity between target and predictors

# 4 node nnet
QKNnet4 <- Nnet(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 
                + DA_Single + NumDeliv + NumDeliv.Sqrd + DA_Income.Log
                + MealsPerDeliv + Healthy + Veggie + Meaty + 
                  Special + DaySinceLastOrder,
                 data = filter(QK, Sample =="Estimation"),
                 decay = 0.15, size = 4)

# stepwise variables in 4 node nnet
QKNnet4Stepv <- Nnet(formula = SUBSCRIBE ~  DA_Income.Log + DA_Under20 
                     + NumDeliv + NumDeliv.Sqrd + MealsPerDeliv + Veggie 
                     + DaySinceLastOrder,
                data = filter(QK, Sample =="Estimation"),
                decay = 0.15, size = 4)
# Compare stepwise, tree, and neural network model
lift.chart(modelList = c("QKLogis","QKStepLogis", "QKForestAllv", "QKForestStepv", "QKNnet4", "QKNnet4Stepv"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.1954023,
           type = "cumulative", sub = "Validation")

##plot effects of all predictors
plot(allEffects(QKStepLogis), type="response")

##Raw Estimated Probabilities added to data in ScoreRaw
QK$ScoreRaw <- rawProbScore(model = "QKStepLogis",
                                   data = QK,
                                   targLevel = "Y")

# Rank Order - rank individuals in dataframe from best to worst, in ScoreRank
QK$ScoreRAnk <- rankScore(model = "QKStepLogis",
                                 data = QK,
                                 targLevel = "Y")

# Identify Best Model's response rate
lift.chart(modelList = c("QKStepLogis"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.1954023,
           type = "incremental", sub = "Validation")
lift.chart(modelList = c("QKStepLogis"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.1954023,
           type = "cumulative", sub = "Validation")

#Binning Weeks3Meals to categorize subscribers who continue (Y) or opt-out (N) after promotion
QK$Weeks3Meals<-as.factor(QK$Weeks3Meals)
QK$Weeks3Meals.cat <-as.factor(QK$Weeks3Meals)
levels(QK$Weeks3Meals.cat)

QK$Weeks3Meals.cat2 <- fct_collapse(QK$Weeks3Meals.cat,
                                     N = c("0"),
                                     Y = c("1", "2","3", "4","5", "6","7", "8","9"))

##Extract People who subscribe to promotion
QK_opt2 <- QK %>% filter(SUBSCRIBE == 'Y')
View(QK_opt2)
scatterplot(SUBSCRIBE.Num ~ NumDeliv, data = QK_opt2) #linear

#Identify characteristics of subscribers who stay after promotion
plotmeans(NumDeliv ~ Weeks3Meals.cat2, data = QK_opt2)
plotmeans(DA_Income.Log ~ Weeks3Meals.cat2, data = QK_opt2)
plotmeans(DA_Under20 ~ Weeks3Meals.cat2, data = QK_opt2)
plotmeans(MealsPerDeliv ~ Weeks3Meals.cat2, data = QK_opt2)
plotmeans(Veggie ~ Weeks3Meals.cat2, data = QK_opt2)
plotmeans(DaySinceLastOrder ~ Weeks3Meals.cat2, data = QK_opt2)
