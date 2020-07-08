library(stringr)
library(lubridate)

x <- read.csv('538-predictions.csv', header = TRUE)
x$Date <- ymd(x$Date)
x$Win.Prob1 <- as.double(x$Win.Prob1)
x$Win.Prob2 <- as.double(x$Win.Prob2)
x$Score1 <- as.double(x$Score1)
x$Score2 <- as.double(x$Score2)

## Used to perform the analysis below -------------------------------------------------------------
visit.pred <- x$Win.Prob1 > x$Win.Prob2
visit.win <- x$Score1 > x$Score2
x$Correct.Pred <- (visit.pred & visit.win) | (!visit.pred & !visit.win)
# x$Point.Spread <- ifelse(visit.pred, x$Score1, x$Score2) - ifelse(visit.pred, x$Score2, x$Score1)


## Overall accuracy -------------------------------------------------------------------------------
sum(x$Correct.Pred) / nrow(x)

# Is the accuracy improving over time?
by.date <- split(x, x$Date)
acc.by.date <- sapply(by.date, function(x) sum(x$Correct.Pred) / nrow(x))
plot(ymd(names(acc.by.date)), acc.by.date, type = 'b', pch = 19, cex = .6, col = 'blue')
abline(h = .5, col = 'red', lwd = 3)

# 
plot(mapply(max, x$Win.Prob1, x$Win.Prob2), abs(x$Score1 - x$Score2) * ifelse(x$Correct.Pred, 1, -1),
     type = 'p', pch = 19, col = ifelse(x$Correct.Pred, 'blue', 'red'))
points(mapply(max, x$Win.Prob1, x$Win.Prob2), rep(0, nrow(x))) #abs(mapply(max, x$Points1, x$Points2, na.rm = TRUE)))
abline(h = 0)


## Confusion matrix of home vs visiting team predictions ------------------------------------------
# The visiting team prediciton of FALSE then winning is the greatest error
table(visit.pred, visit.win) / nrow(x)

# Which teams are winning on the road too often?
sort(table(x$Visitor[visit.pred == FALSE & visit.win == TRUE]))


plot()
