#setwd('/home/larry/Github-Public/538-NBA-Predictions-2017/')
library(stringr)
library(lubridate)

print('Date,Home.Abbv,Home,Points1,Win.Prob1,Score1,Visitor.Abbv,Visitor,Points2,Win.Prob2,Score2')
lines <- readLines('538-predictions-thru-11-15-2017.txt')
print_flag <- FALSE
for (line in lines) {
  if (str_detect(line, '[0-9]+/[0-9]+/[0-9]{4}')) date <- mdy(line)
  else if (print_flag == FALSE) {
    home <- str_split(line, "\t", simplify = TRUE)
    if (home[3] == "") home[3] <- "NA"
    print_flag <- TRUE
  } else {
    visitor <- str_split(line, "\t", simplify = TRUE)
    if (visitor[3] == "") visitor[3] <- "NA"
    print(sprintf("%s,%s,%s", date, 
                  paste(home, collapse = ","), 
                  paste(visitor, collapse = ",")))
    print_flag <- FALSE
  }
}

