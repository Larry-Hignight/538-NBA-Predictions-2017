#setwd('/home/larry/Github-Public/538-NBA-Predictions-2017/')
library(stringr)
library(lubridate)

# Some lines in the text file only contain 4, instead of 5, fields; This is the correction.
fix_line <- function(x) c(x[1:2], "", x[3:4])

print('Date,Abbv1,Visitor,Points1,Win.Prob1,Score1,Abbv2,Home,Points2,Win.Prob2,Score2')
lines <- readLines('538-predictions-thru-11-15-2017.txt')
print_flag <- FALSE
for (line in lines) {
  if (str_detect(line, '[0-9]+/[0-9]+/[0-9]{4}')) date <- mdy(line)
  else if (print_flag == FALSE) {
    team1 <- str_split(line, "\t", simplify = TRUE)
    if (length(team1) < 5) team1 <- fix_line(team1)
    if (team1[3] == "PK") team1[3] <- 0
    if (team1[3] == "") team1[3] <- "NA"
    print_flag <- TRUE
  } else {
    team2 <- str_split(line, "\t", simplify = TRUE)
    if (length(team2) < 5) team2 <- fix_line(team2)
    if (team2[3] == "PK") team1[3] <- 0
    if (team2[3] == "") team2[3] <- "NA"
    print(sprintf("%s,%s,%s", date, 
                  paste(team1, collapse = ","), 
                  paste(team2, collapse = ",")))
    print_flag <- FALSE
  }
}

