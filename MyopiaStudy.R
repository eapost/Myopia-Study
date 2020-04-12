#################### DATA PREPARATION #################### 

# Import data
myopiaData <-read.csv2("myopia.csv", header = TRUE) 
str(myopiaData)

# Delete Columns ID
myopiaData$ID<-NULL
myopiaData <- na.omit(myopiaData)

# Numerical variables
myopiaData<-transform(myopiaData, STUDYYEAR = as.numeric(STUDYYEAR))
myopiaData<-transform(myopiaData, AGE = as.numeric(AGE))
myopiaData<-transform(myopiaData, SPHEQ = as.numeric(SPHEQ))
myopiaData<-transform(myopiaData, AL = as.numeric(AL))
myopiaData<-transform(myopiaData, ACD = as.numeric(ACD))
myopiaData<-transform(myopiaData, LT = as.numeric(LT))
myopiaData<-transform(myopiaData, VCD = as.numeric(VCD))
myopiaData<-transform(myopiaData, SPORTHR = as.numeric(SPORTHR))
myopiaData<-transform(myopiaData, READHR = as.numeric(READHR))
myopiaData<-transform(myopiaData, COMPHR = as.numeric(COMPHR))
myopiaData<-transform(myopiaData, STUDYHR = as.numeric(STUDYHR))
myopiaData<-transform(myopiaData, TVHR = as.numeric(TVHR))
myopiaData<-transform(myopiaData, DIOPTERHR = as.numeric(DIOPTERHR))

# Factor variables
myopiaData<-transform(myopiaData, MYOPIC = factor(MYOPIC))
myopiaData<-transform(myopiaData, GENDER = factor(GENDER))
myopiaData<-transform(myopiaData, MOMMY = factor(MOMMY))
myopiaData<-transform(myopiaData, DADMY = factor(DADMY))

# Create numeric variable dataset
index1 <- sapply(myopiaData, class) == "numeric"
myopiaNum <- myopiaData[,index1]

# Create factor variable dataset
index2 <- sapply(myopiaData, class) == "factor"
myopiaFact <- myopiaData[,index2]
n <- nrow(myopiaFact)

# Create one new factor variable with levels for parents' myopia
myopiaData$MYOPIC_PARENTS <- NULL
myopiaFact$MYOPIC_PARENTS <- NULL

i <- 1
while(i<=nrow(myopiaData)) {
  if (myopiaData[i,17]==0 & myopiaData[i,16]==0) {
    myopiaData$MYOPIC_PARENTS[i]<-0
    myopiaFact$MYOPIC_PARENTS[i]<-0
  } else if (myopiaData[i,17]==0 & myopiaData[i,16]==1) {
    myopiaData$MYOPIC_PARENTS[i]<-1
    myopiaFact$MYOPIC_PARENTS[i]<-1
  } else if (myopiaData[i,17]==1 & myopiaData[i,16]==0) {
    myopiaData$MYOPIC_PARENTS[i]<-2
    myopiaFact$MYOPIC_PARENTS[i]<-2
  } else {
    myopiaData$MYOPIC_PARENTS[i]<-3
    myopiaFact$MYOPIC_PARENTS[i]<-3
  }
  i <- i+1
}

myopiaData <- transform(myopiaData, MYOPIC_PARENTS = factor(MYOPIC_PARENTS))
myopiaFact <- transform(myopiaFact, MYOPIC_PARENTS = factor(MYOPIC_PARENTS))

# Delete MOMMY and DADMY variables to avoid multicollinearity problem
myopiaData$MOMMY<-NULL
myopiaData$DADMY<-NULL
myopiaFact$MOMMY<-NULL
myopiaFact$DADMY<-NULL

# Structure
str(myopiaData)
summary(myopiaData)

#################### VISUALIZATIONS #################### 

# Numerical variables distribution
par(mfrow=c(2,4))
for (i in c(1:8))
{
  h1 <- hist(myopiaNum[,i], main=names(myopiaNum)[i], border='pink', col='purple', probability = T)
  lines(density(myopiaNum[,i]), col="blue", lwd=2)
}

par(mfrow=c(2,3))
for (i in c(9:13))
{
  h2 <- hist(myopiaNum[,i], main=names(myopiaNum)[i], border='pink', col='purple', probability = T)
  lines(density(myopiaNum[,i]), col="blue", lwd=2)
}

# Factors

    # Genre 
    table1<-table(myopiaFact$MYOPIC, myopiaFact$GENDER)
    prop_table1<-prop.table(table1,2)
    table_genre<- round(prop_table1,2)
    row.names(table_genre)<-c("Myopic","No-myopic")
    colnames(table_genre)<-c("Men","Women")
    
    barplot(table_genre[1,],
            ylab="Myopia",
            #main = "Myopia Per Gender",
            border="purple",
            col="blue",
            density=10)
    
      # X-squared test
      prop.test(table1,2) # p = 0.1582 > 0.05 (no significant difference in myopia between genders)
    
    # Parents' myopia
    table2<-table(myopiaFact$MYOPIC, myopiaFact$MYOPIC_PARENTS)
    prop_table2<-prop.table(table2,1)
    table_parents<-round(prop_table2,2)
    row.names(table_parents)<-c("Myopic","No-Myopic")
    colnames(table_parents)<-c("Non-myopic parents", "Myopic mother", "Myopic father", "Myopic parents")
    
    barplot(table_parents[1,],
            ylab="Myopia",
            # main = "Parents' Myopia",
            border="purple",
            col="blue",
            density=10)
    
    # Correlations of numeric variables
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    library(corrplot)
    corrplot(cor(myopiaNum), method = "color", col = col(200),
                 type = "upper", order = "hclust", number.cex = .7,
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col = "black", tl.srt = 90, # Text label color and rotation
                 # Combine with significance
                 sig.level = 0.05, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag = FALSE)
    
    # Myopia on each numeric variable
    par(mfrow=c(1,3))
    for(j in 1:3){
    plot(myopiaFact[,1], myopiaNum[,j],
         xlab=names(myopiaNum)[j], 
         ylab="MYOPIA",
         border="purple",
         col="blue")
    }
    
####################  MODELS  ####################  

# Null Model
myopia_null<-glm(MYOPIC~1, data=myopiaData, family = binomial(link ="logit"))
summary(myopia_null)

# Initial Logistic Model
myopia_logistic<-glm(MYOPIC~., data=myopiaData, family = binomial(link ="logit"))
summary(myopia_logistic)

    # Collinearity check
    alias(myopia_logistic)
    library(car)
    round(vif(myopia_logistic),1) # Using VIF 

# Final Logistic Model
myopiaData$DIOPTERHR<-NULL
myopiaData$AL<-NULL
myopiaNum$DIOPTERHR<-NULL
myopiaNum$AL<-NULL
myopia_logistic2<-glm(MYOPIC~., data=myopiaData, family = binomial(link ="logit"))
summary(myopia_logistic2)
alias(myopia_logistic2)

    # Collinearity check
    library(car)
    round(vif(myopia_logistic2),1) # Using VIF 

# Test the significance of the factors included in the logistic model
library(aod)
    # Gender
    wald.test(b = coef(myopia_logistic2), Sigma = vcov(myopia_logistic2), Terms = 4)
    # Parents' myopia
    wald.test(b = coef(myopia_logistic2), Sigma = vcov(myopia_logistic2), Terms = 15)

# Stepwise methods for variable selection in the final logistic model

  # AIC Criterion
  step(myopia_logistic2, direction = "both")
  myopia_aicstepwise<-glm(MYOPIC ~ GENDER + SPHEQ + ACD + SPORTHR + READHR + STUDYHR + MYOPIC_PARENTS, data=myopiaData, family = binomial(link ="logit"))
  summary(myopia_aicstepwise)
  # Collinearity check
  round(vif(myopia_aicstepwise),1)

  # BIC Criterion
  step(myopia_logistic2, direction = "both", k=log(nrow(myopiaData)))
  myopia_bicstepwise<-glm(MYOPIC ~ SPHEQ + SPORTHR + MYOPIC_PARENTS, data=myopiaData, family = binomial(link ="logit"))
  summary(myopia_bicstepwise)
  # Collinearity check
  round(vif(myopia_bicstepwise),1)
  
# Lasso for variable selection
  library(glmnet)
  x1 <- model.matrix(MYOPIC~., myopiaData)[,-1]
  lambdas <- 10^seq(8,-4,length=250)
  myopia_model_lasso <- glmnet(x=x1, y=myopiaData$MYOPIC, alpha = 1, lambda=lambdas, family = "binomial")
  plot(myopia_model_lasso, label = T)
  
  # Find the best lambda to minimize the mean classification error by using cross-validation 
  library(glmnet)
  cv.lasso <- cv.glmnet(x1, myopiaData$MYOPIC, alpha = 1, lambda=lambdas, family="binomial", type.measure = "class")
  plot(cv.lasso)
  
  # 1st alternative: lambda.min value
  
    # Final model with lambda.min
    lasso.model <- glmnet(x=x1, y=myopiaData$MYOPIC, alpha = 1, lambda = cv.lasso$lambda.min, family = "binomial")
    # Display regression coefficients
    coef(cv.lasso, s = cv.lasso$lambda.min)
    
  # 2nd alternative: lambda.1se value
    
    # Final model with lambda.1se
    lasso.model_2 <- glmnet(x=x1, y=myopiaData$MYOPIC, alpha = 1, lambda = cv.lasso$lambda.1se, family = "binomial")
    # Display regression coefficients
    coef(cv.lasso, s = cv.lasso$lambda.1se)
    
    # Model after Lasso
    myopia_lasso<-glm(MYOPIC~GENDER + SPHEQ + SPORTHR + MYOPIC_PARENTS, data=myopiaData, family = "binomial")
    summary(myopia_lasso)
    # Collinearity check
    round(vif(myopia_lasso),1)
