## quiz 1 
### question 5


n <- 1000000
prevalence = 1/1000
sens <- .99
spec <- .99
clicked <- prevalence * n
TP <- sens * clicked
Notclicked <- n - clicked
TN <- spec * Notclicked
FP <- Notclicked * sens
FP<- Notclicked-TN
PPV <- TP/ (TP+FP)

PPV