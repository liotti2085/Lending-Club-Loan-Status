#########################################################################
# log-loss function
logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}

#########################################################################
# Test code begins
start.time = Sys.time()
source('mymain.R')
end.time = Sys.time()
run.time = as.numeric(difftime(end.time, start.time, units = 'min'))

# submission files
allFiles = list.files()
subFiles = grep('mysubmission', allFiles, value = TRUE, 
                ignore.case = TRUE)

# calculate the test error on the test set
test = read.csv('test.csv')

label = read.csv('label.csv', sep = ',')
err = rep(NA, length(subFiles))
for (met in 1:length(subFiles)){
  
  prediction = read.csv(subFiles[met], sep = ',')
  yp = merge(prediction, label, by = 'id', all.y = TRUE)
  err[met] = with(yp, logLoss(y, prob))
  
}

#########################################################################
write.table(err, file = 'proj_3.csv', sep = ',', row.names = FALSE,
            col.names = FALSE)
write.table(run.time, file = 'proj_3.csv', sep = ',', 
            row.names = FALSE, col.names = FALSE, append = TRUE)