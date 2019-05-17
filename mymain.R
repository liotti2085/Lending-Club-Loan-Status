if (!require("mice")) {
  install.packages("mice")
}

if (!require("caret")) {
  install.packages("caret")
}

library(caret)

library(mice)

train = read.csv("train.csv")
test = read.csv("test.csv")


#Training Data
train1 = subset(train, select = -c(grade, emp_title, emp_length, title, 
                                    addr_state, zip_code, 
                                    earliest_cr_line)) #drop grade b/c basically same as subgrade


train_temp = mice(data = train1, method = "cart", m = 1)

train_imp = complete(train_temp)

train_imp$loan_status <- ifelse(train_imp$loan_status == "Fully Paid", 0, 1)


#Testing Data
test1 = subset(test, select = -c(grade, emp_title, emp_length, title, 
                                 addr_state, zip_code, 
                                 earliest_cr_line)) 


test_temp = mice(data = test1, method = "cart", m = 1)

test_imp = complete(test_temp)


dmy <- dummyVars(" ~ .", data = train_imp)
trsf <- data.frame(predict(dmy, newdata = train_imp))
label1 = trsf$loan_status
trsf = subset(trsf, select = -loan_status)
trsf = as.matrix(trsf)

dmy <- dummyVars(" ~ .", data = test_imp)
tst <- data.frame(predict(dmy, newdata = test_imp))
tst = as.matrix(tst)

xg1 = xgboost(data = trsf, label = label1, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

pred2 = round(predict(xg1, tst, type = "response"), 2) 

write.table(cbind(test_imp$id, pred2), file = "mysubmission1.txt", col.names = c("id", "prob"), row.names = F, sep = ", ", quote = F)
