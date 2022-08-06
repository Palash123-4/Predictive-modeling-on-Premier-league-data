match.results <- function(homeGoals, awayGoals){
  #Determines the match outcome (H, D or A) based on goals scored by home and away teams.
   
  home <- homeGoals > awayGoals
  away <- awayGoals > homeGoals
  draws <- homeGoals == awayGoals
     
  results <- character(length(homeGoals))
  results[draws] <- "D"
  results[home] <- "H"
  results[away] <- "A"
 
  return(results)
}
#_________________________________________________________________________________________________ 

league.table <- function(homeTeam, awayTeam, homeGoals, awayGoals){
                          
  #points awarded for a match outcome  
  winPts <- 3
  drawPts <- 1
  loosePts <- 0
   
  if (length(unique(sapply(list(homeTeam, awayTeam, homeGoals, awayGoals), length))) != 1 ){
    warning("input vectors not of same length.")
  }
   
  numMatches <- length(homeTeam)
   
  teams <- levels(factor(c(as.character(homeTeam), as.character(awayTeam))))
  numTeams <- length(teams)
   
  #vector with outcome of a match (H, D or A)
  results <- match.results(homeGoals, awayGoals)
   
  #for output
  homeWins <- numeric(numTeams)
  homeDraws <- numeric(numTeams)
  homeLoss <- numeric(numTeams)
  awayWins <- numeric(numTeams)
  awayDraws <- numeric(numTeams)
  awayLoss <- numeric(numTeams)
  goalsFor <- numeric(numTeams)
  goalsAgainst <- numeric(numTeams)
  goalsDifference <- numeric(numTeams)
  playedMatches <- numeric(numTeams)
  pts <- numeric(numTeams)
 
  for (t in 1:numTeams) {
    #mathc results for a given team
    homeResults <- results[homeTeam == teams[t]]
    awayResults <- results[awayTeam == teams[t]]
 
    playedMatches[t] <- length(homeResults) + length(awayResults)
     
    goalsForH <- sum(homeGoals[homeTeam == teams[t]])
    goalsForA <- sum(awayGoals[awayTeam == teams[t]])
    goalsFor[t] <- goalsForA + goalsForH
    goalsAgainstH <- sum(awayGoals[homeTeam == teams[t]])
    goalsAgainstA <- sum(homeGoals[awayTeam == teams[t]])
    goalsAgainst[t] <- goalsAgainstA + goalsAgainstH
    goalsDifference[t] <- goalsFor[t] - goalsAgainst[t]
     
    homeWins[t] <- sum(homeResults == "H")
    homeDraws[t] <- sum(homeResults == "D")
    homeLoss[t] <- sum(homeResults == "A")
    awayWins[t] <- sum(awayResults == "A")
    awayDraws[t] <- sum(awayResults == "D")
    awayLoss[t] <- sum(awayResults == "H")
       
    totWins <- homeWins[t] + awayWins[t]
    totDraws <- homeDraws[t] + awayDraws[t]
    totLoss <- homeLoss[t] + awayLoss[t]
     
    pts[t] <- (winPts * totWins) + (drawPts * totDraws) + (loosePts * totLoss)
     
    }
 
  table <- data.frame(cbind(playedMatches, homeWins, homeDraws, 
                            homeLoss, awayWins, awayDraws, awayLoss, 
                            goalsFor, goalsAgainst, goalsDifference, pts),
                      row.names=teams)
 
     
  names(table) <- c("PLD", "HW", "HD", "HL", "AW", "AD", "AL", "GF", "GA", "GD", "PTS")
  ord <- order(-table$PTS, -table$GD, -table$GF)
  table <- table[ord, ]
 
  return(table)
 
  }
 
#_____________________________________________________________________________________________________
  
result.matrix <- function(homeTeam, awayTeam, homeGoals, awayGoals, format="score"){
   
  if (length(unique(sapply(list(homeTeam, awayTeam, homeGoals, awayGoals), length))) != 1 ){
    warning("input vectors not of same length.")
  }
   
  teams <- levels(factor(c(as.character(homeTeam), as.character(awayTeam))))
  numTeams <- length(teams)
  numMatches <- length(homeTeam)
   
  if (format == "HDA"){
    results <- match.results(homeGoals, awayGoals)
  }
   
  resultMatrix <- matrix(nrow=numTeams, ncol=numTeams, dimnames=list(teams, teams))
   
  for (m in 1:numMatches){
     
    if (format == "score"){
      cell <- paste(homeGoals[m], "-", awayGoals[m])
      }
    else if (format == "HDA"){
      cell <- results[m]
    }
    else if (format == "difference"){
      cell <- homeGoals[m] - awayGoals[m]
    }
     
    resultMatrix[homeTeam[m], awayTeam[m]] <- cell
  }
     
  return(resultMatrix)
   
}
#____________________________________________________________________________________________________________

league.table(HomeTeam, AwayTeam, FTHG, FTAG) #actual league table for 2015-16 season
league.table(HomeTeam, AwayTeam, z11, z12)  #league table after one simulation
league.table(HomeTeam, AwayTeam, c11, c12)  #league table after 1000 simulation for poisson
league.table(HomeTeam, AwayTeam, d11, d12) #league table after 1000 simulation
                                           #for negative binomial
data1=read.csv(file.choose(),header=T)   #data of 2016_17 league season upto 16/03/2017
data1
attach(data1)
league.table(HomeTeam, AwayTeam, FTHG, FTAG)