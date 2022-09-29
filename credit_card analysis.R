## import data
credit_card <- read.csv("UCI_Credit_Card.csv")
head(credit_card)
str(credit_card)

## packages
packages <- c("arm", "AER", "gridExtra", "reshape2", "tidyr", "grid", "MASS", "knitr", "AUC")
lapply(packages, library, character.only = T)

## look for missing values 
library(naniar)
library(tidyverse)
## check missing values
credit_card %>% miss_var_summary()

## visualize missing values
credit_card %>% vis_miss()

## tranform the data
credit_card$SEX <- as.factor(credit_card$SEX)
levels(credit_card$SEX) <- c("Male", "Female")

credit_card$EDUCATION<- as.factor(credit_card$EDUCATION)
levels (credit_card$EDUCATION)<- c('Unkonwn', 'Graduate school' ,
                               'University','High school','Others','Unknown','Unknown')

# Convert MARRIAGE to Categorical variable
credit_card$MARRIAGE<- as.factor(credit_card$MARRIAGE)
levels(credit_card$MARRIAGE)<-c("Others", 'Married', 'Single', 'Others') 

## analysis with regard to age
df1 %>% ggplot(aes(AGE))+
  geom_histogram(fill = "coral")+
  labs(title = "Distribution of age with regard to gender and default payment")+
  facet_wrap(default.payment.next.month~SEX)

## marital status, age and default payment
df1 %>% ggplot(aes(AGE))+
  geom_histogram(fill = "coral")+
  labs(title = "Distribution of age with regard to marital status and default payment")+
  facet_wrap(default.payment.next.month~MARRIAGE)


### Exploratory data analysis
## remove ID because it is unique
df1 <- credit_card %>% dplyr::select(-ID)
df1 %>% ggplot(aes(factor(SEX), fill = factor(default.payment.next.month)))+
  geom_bar(position = "dodge")+
  labs(title = "Relationship between Gender and default",
       x = "Sex")

## education
df1 %>% ggplot(aes(factor(EDUCATION), fill = factor(default.payment.next.month)))+
  geom_bar(position = "dodge")+
  labs(title = "Relationship between education status and default",
       x = "Education status")

## marital status
df1 %>% ggplot(aes(factor(MARRIAGE), fill = factor(default.payment.next.month)))+
  geom_bar(position = "dodge")+
  labs(title = "Relationship between marital status and default",
       x = "Marital status")

# age
df1 %>% ggplot(aes(factor(default.payment.next.month), AGE))+
  geom_boxplot()+
  labs(title = "Relationship between Age and default",
       x = "Default payment")

# limit balance
df1 %>% ggplot(aes(factor(default.payment.next.month), LIMIT_BAL))+
  geom_boxplot()+
  labs(title = "Relationship between limit balance and default",
       x = "Default payment")

ggplot(credit_card, aes(x = factor(default.payment.next.month), y = LIMIT_BAL)) + facet_wrap(~ SEX + MARRIAGE) + 
  geom_boxplot(aes(fill = factor(default.payment.next.month))) + 
  labs(x = "Default", y = "Amount of given credit", fill = "Default :") + 
  ggtitle("Amount of given credit vs. default (by gender and marriage status)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

df2 = df1 %>% mutate(total_pay_amount = PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6)

## total pay amount
# age
df2 %>% ggplot(aes(factor(default.payment.next.month), log(total_pay_amount), fill = factor(default.payment.next.month)))+
  geom_boxplot()+
  labs(title = "Relationship between total pay amount and default",
       x = "Default payment",
       y = "Log tranformation of total amount")

df2 %>% group_by(default.payment.next.month) %>% 
  summarise(avg_payment_amount = mean(total_pay_amount),
            med_payment_amount = median(total_pay_amount))

df2 = df2 %>% mutate(total_bill_amount = BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6)


## bill amount
df2 %>% ggplot(aes(factor(default.payment.next.month), log(total_bill_amount), fill = factor(default.payment.next.month)))+
  geom_boxplot()+
  labs(title = "Relationship between total bill amount and default",
       x = "Default payment",
       y = "Log tranformation of total bill amount")

df2 %>% group_by(default.payment.next.month) %>% 
  summarise(avg_bill_amount = mean(total_bill_amount),
            med_bill_amount = median(total_bill_amount))

### logistic regression
### split data
library(caret)
sk <- createDataPartition(df1$default.payment.next.month, p = 0.8, list = FALSE)
tr_data <- df1[sk,]
tes_data <- df1[-sk,]
fit1 <- glm(default.payment.next.month~., data = tr_data, family = "binomial")
summary(fit1)

## predict the test set
pred <- fit1 %>% predict(tes_data)
head(pred)
DEFAULT.pred = ifelse(pred < 0.5, 0, 1)
confusionMatrix(factor(tes_data$default.payment.next.month), factor(DEFAULT.pred))

## variable importance
library(vip)
vip(fit1)
