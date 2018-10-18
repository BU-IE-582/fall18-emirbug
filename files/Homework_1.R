####------------------------###0
#### Initialization & Setup ####
####------------------------###0

# Clear connections, environment
closeAllConnections()
rm(list = ls())
gc()

# Load required packages
library(data.table)
library(anytime)
library(stringr)

# Set working directory and folder paths
setwd("C:\\Users\\emirhan.bugday\\Desktop\\IE 582\\HW-1")

# Read dataset
Matches <- as.data.table(readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"))
Odds <- as.data.table(readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"))



####---------------------------###0
#### Observe & Clean Datasets ####
####---------------------------###0

# Observe data types and overall summary of the datasets
str(Matches)
str(Odds)
summary(Odds)
summary(Matches)

# Check duplicate values of Odds and remove them if there are any
if (dim(unique(Odds))[1] < dim(Odds)[1]) {
  Odds[, RowCount := c(1:.N), list(matchId, betType, oddtype, bookmaker, date, totalhandicap)]
  Odds <- Odds[RowCount == 1]
  Odds[, RowCount := NULL]
} 

# Check duplicate values of Matches and remove them if there are any
if (dim(unique(Matches))[1] < dim(Matches)[1]) {
  Matches[, RowCount := c(1:.N), list(matchId)]
  Matches <- Matches[RowCount == 1]
  Matches[, RowCount := NULL]
} 

# Observe bet types
unique(Odds$betType)

# Observe handicaps
unique(Odds$totalhandicap)

# Since we are only interested in "ou" bet type with 2.5 handicap, filter Odds data to deal with a smaller data
Odds <- Odds[betType == "1x2" ]

# Eliminate matches which are not in Odds dataset, if there any, since they will be redundant
Matches <- Matches[matchId %in% unique(Odds$matchId)]

# Eliminate matches which are not started yet, since we won't be able to include them in our analysis
Matches <- Matches[!is.na(score)]

# Then eliminate the matches from Odds table which are not contained in the remaining Matches dataset
Odds <- Odds[matchId %in% unique(Matches$matchId)]

# Convert Unix Epoct Time to Date
Matches$date <- anytime::anytime(Matches$date)
str(Matches)
Odds$date <- anytime::anytime(Odds$date)
str(Odds)



####---------------------------------------###0
#### Merge Actual Match Info with the Odds ####
####---------------------------------------###0

# Determine total score of the matches by subtracting individual scores from the score column
Matches[, TotalScore := as.numeric(tstrsplit(score, ":")[[1]]) + as.numeric(tstrsplit(score, ":")[[2]])]

# Flag matches if its ended "over" according to handicap = 2.5
Matches[TotalScore > 2.5, IsOver := 1]
Matches[is.na(IsOver), IsOver := 0]
summary(Matches)

# Subtract year info of the matches from date column
Matches[, Year := year(date)]
summary(Matches)

# Merge neccesary info to the Odds dataset
Odds <- merge(Odds, Matches[, .(matchId, IsOver, Year)], by = "matchId")



####-------------------------------------------###0
#### Calculate Bookmarker Implied Probabilites ####
####-------------------------------------------###0

# First determine the initial and the final odds provided by a bookmarker for a match
Odds[, `:=` (MinDate = min(date), MaxDate = max(date)), list(matchId, bookmaker, oddtype)]
Odds <- rbind(cbind(Odds[date == MinDate], Period = "initial"), cbind(Odds[date == MaxDate], Period = "final"))

# Control if there exist two types of bets for each match for each book marker, if there are missing remove that bet
summary(Odds[, .(OddCount = .N), list(matchId, bookmaker, Period)]$OddCount)

# Calculate implied probabilities for the initial odds
Odds[, NormalizationTerm := 1 /sum(1/odd), list(matchId, bookmaker, Period)]
Odds[, ImpliedProbs := (1/odd) * NormalizationTerm]
summary(Odds[oddtype == "over"])


####----------------------###0
#### Decide & Create Bins ####
####----------------------###0

# First select "over" odds
SelectedOdds <- Odds[oddtype == "over"]

# Observe total number of bets per bookmarker
bookmarkers <- SelectedOdds[Period == "initial", .(MatchCount = .N), bookmaker]
bookmarkers <- bookmarkers[order(-MatchCount)]

# Select top 7 bookmarkers in terms of the number of matches
SelectedOdds <- SelectedOdds[bookmaker %in% bookmarkers[1:7]$bookmaker]

# See some historgrams to get insight
hist(SelectedOdds[Period == "initial"]$ImpliedProbs, breaks = 50, xlab = "Implied Probabilities", main = "Histogram of All Implied Probabilities")

# Exclude tails with few data points
SelectedOdds <- SelectedOdds[ImpliedProbs >= 0.35 & ImpliedProbs <= 0.75]

# Determine Bins
SelectedOdds[ImpliedProbs >= 0.35 & ImpliedProbs <= 0.45, Bin := 1]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.45 & ImpliedProbs <= 0.48, Bin := 2]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.48 & ImpliedProbs <= 0.51, Bin := 3]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.51 & ImpliedProbs <= 0.54, Bin := 4]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.54 & ImpliedProbs <= 0.57, Bin := 5]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.57 & ImpliedProbs <= 0.60, Bin := 6]
SelectedOdds[is.na(Bin) & ImpliedProbs > 0.60 & ImpliedProbs <= 0.75, Bin := 7]

summary(SelectedOdds)

o <- SelectedOdds[order(bookmaker, Bin), .(Lb = min(ImpliedProbs), Ub = min(ImpliedProbs), Count = .N), list(bookmaker, Bin, Period)]

hist(SelectedOdds[Period == "initial"]$Bin)
hist(SelectedOdds[Period == "final"]$Bin)


####----------------###0
#### Graph Findings ####
####----------------###0

# Calculate fraction of games fineshed over and avg. implied prob 
Findinds <- SelectedOdds[, .(Fraction = sum(IsOver)/.N, AvgImpliedProb = mean(ImpliedProbs)), list(bookmaker, Period, Bin)]

# Calculate fraction of games fineshed over and avg. implied prob per year
YearBased <- SelectedOdds[, .(Fraction = sum(IsOver)/.N, AvgImpliedProb = mean(ImpliedProbs)), list(bookmaker, Period, Bin, Year)]
YearBased <- YearBased[order(bookmaker, Period, Bin, Year)]




# TASK 1 - A
summary(Findinds$Fraction)
summary(Findinds$AvgImpliedProb)


c <- 2
w <- 1
p <- 1

plot(Findinds[bookmaker == bookmarkers[1]$bookmaker & Period == "initial"]$AvgImpliedProb, 
     Findinds[bookmaker == bookmarkers[1]$bookmaker & Period == "initial"]$Fraction, 
     xlim = c(0.35,0.7), ylim = c(0.35,0.7), col = c, lwd = w, pch = p,
     xlab = "Average Implied Probabilities",
     ylab = "Actual Fraction of the Games")

abline(v = 0.45, col = "orange", lty = 2)
abline(v = 0.48, col = "orange", lty = 2)
abline(v = 0.51, col = "orange", lty = 2)
abline(v = 0.54, col = "orange", lty = 2)
abline(v = 0.57, col = "orange", lty = 2)
abline(v = 0.60, col = "orange", lty = 2)


abline(a=0,b=1,col=1,lwd=1, lty = 2)
title("Bookmarkers Implied Probability Quality")

bm <- bookmarkers[2:7]$bookmaker

for (bm in bookmarkers[2:7]$bookmaker) {
  c <- c + 1
  p <- p + 1
  points(Findinds[bookmaker == bm & Period == "initial"]$AvgImpliedProb, 
         Findinds[bookmaker == bm & Period == "initial"]$Fraction, 
         col = c, lwd = w, pch = p) 
  
}
legend("topleft", legend=c(bookmarkers[1:7]$bookmaker)
       ,col = c(1:7), pch = c(1:7), bg = ("white"), horiz = F, cex = 0.7) 




# TASK 1 - B
summary(YearBased$Fraction)
summary(YearBased$AvgImpliedProb)


plot(YearBased[ bookmaker == bookmarkers[1]$bookmaker & Period == "initial" & Bin == 1]$Year, 
     YearBased[ bookmaker == bookmarkers[1]$bookmaker & Period == "initial" & Bin == 1]$Fraction,
     xlab = "Years",
     ylab = "Probabilities",
     type = "l", ylim = c(0.25,0.80), col = 3, lwd = 2)

lines(YearBased[ bookmaker == bookmarkers[1]$bookmaker & Period == "initial" & Bin == 1]$Year, 
      YearBased[ bookmaker == bookmarkers[1]$bookmaker & Period == "initial" & Bin == 1]$AvgImpliedProb, 
      col = 4, lwd = 2)

title("Yearly Comparison for Bin 1")

legend("topleft", legend=c("Actual Fractions", "Implied Probabilities")
       ,col = c(3:4), pch = c(1:2), bg = ("white"), horiz = F, cex = 0.7) 

