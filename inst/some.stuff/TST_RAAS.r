# RAAS scale scoring commands
# Creation date: 2013-04-02
# --------------

testChar <- list(
  acronym="RAAS",
  name="Revised Adult Attachment Scale",
  ref="Collins, 1996",
  n.items=18,
  valid=c(1, 2, 3, 4, 5),
  miss=c(0),
  comm="Public domain: www.openpsychassessment.org/wp-content/uploads/2011/06/AdultAttachmentScale.pdf"
) # end testChar

scoring.fun <- function(answers, sex, age=0, id, date.test, comm)
# "answer" is a *character* vector as introduced through the keyboard.
{
  answers <- as.numeric(answers) # transform to numeric for easier scoring
  answers[answers %in% c(0)] <- NA # missing characters to NA
  blanks <- sum(is.na(answers)) # compute number of missings
  pcnt.blanks <- round((blanks / 18) * 100) # compute % of missings
  reversed.items=c(2, 7, 8, 13, 16, 17, 18)
  answers[reversed.items] <- (5 + 1) - answers[reversed.items] # reverse items
  results <- data.frame(NULL) # Null data frame for results

  toT <- function(raw.score, mean, sd) # compute T score
  {
    T.score <- round(((raw.score - mean) / sd) * 10 + 50)
    return(T.score)
  } # end toT

  makeGraph <- function(T.score) # make graph
  {
    if (!is.na(T.score)) # avoids error
    {
      graph  <- "|    :    :    |    :    |    :    |    :    :    |"
      if (T.score < 0) T.score <- 0
        else if (T.score > 100) T.score <- 100
      position <- round((T.score/2)+1)
      graph <- paste(substr(graph, 1, position-1), substr(graph, position + 1, nchar(graph)), sep="o")
    }
    else graph <- NA
  } # end makeGraph

  # C scale scoring commands
  # --------------
  results[1, "Acronym"] <- "C" # acronym
  results[1, "Scale"] <- "Close" # name of the scale
  items <- c(1, 6, 8, 12, 13, 17) # items making up the scale
  results[1, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[1, "Raw"] <- round(mean(answers[items], na.rm=TRUE), 2) # raw score (mean answered items
  if (sex=="male") results[1, "T"] <- toT(results[1, "Raw"], 21.54, 5.14) # compute T score
    else results[1, "T"] <- toT(results[1, "Raw"], 21.92, 5.19)
  results[1, "Graph"] <- makeGraph(results[1, "T"]) # make the graph

  # D scale scoring commands
  # --------------
  results[2, "Acronym"] <- "D" # acronym
  results[2, "Scale"] <- "Dependent" # name of the scale
  items <- c(2, 5, 7, 14, 16, 18) # items making up the scale
  results[2, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[2, "Raw"] <- round(mean(answers[items], na.rm=TRUE), 2) # raw score (mean answered items
  if (sex=="male") results[2, "T"] <- toT(results[2, "Raw"], 20.59, 5) # compute T score
    else results[2, "T"] <- toT(results[2, "Raw"], 19.51, 5.14)
  results[2, "Graph"] <- makeGraph(results[2, "T"]) # make the graph

  # A scale scoring commands
  # --------------
  results[3, "Acronym"] <- "A" # acronym
  results[3, "Scale"] <- "Anxiety" # name of the scale
  items <- c(3, 4, 9, 10, 11, 15) # items making up the scale
  results[3, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[3, "Raw"] <- round(mean(answers[items], na.rm=TRUE), 2) # raw score (mean answered items
  if (sex=="male") results[3, "T"] <- toT(results[3, "Raw"], 13.86, 5.36) # compute T score
    else results[3, "T"] <- toT(results[3, "Raw"], 15.7, 6.05)
  results[3, "Graph"] <- makeGraph(results[3, "T"]) # make the graph

  # One row data frame with scores, for writing scores into a data file
  # --------------------
  results.scores <- data.frame(t(c(blanks, results[, "Raw"], results[,"T"])))
  names(results.scores) <- c("blanks", paste(results[["Acronym"]], "raw", sep="_"), paste(results[["Acronym"]], "T", sep="_"))

  # Ruler for graph column name
  # --------------------
  names(results)[6] <- "0    10   20   30   40   50   60   70   80   90  100"

  # ===== ADDITIONAL CODE INSERTED MANUALLY
  # Combine C & D scales
  CD <- round(mean(c(results[1, 'Raw'], results[2, 'Raw'])), 2)
  results[4, ] <- c('', '','','','','') # blank row to improve readability
  results[5, ] <- c('CD', 'Close/Dependent', '', CD, '', '') # no data for computing T score
  
  # Attachment style assignement
  style <- 'Located at midline'
  if (is.na(CD)) style <- 'Not evaluable' # if all answers are missings
  else if (CD > 3 & results[3, 'Raw'] < 3) style <- 'Secure'
  else if (CD > 3 & results[3, 'Raw'] > 3) style <- 'Preoccupied'
  else if (CD < 3 & results[3, 'Raw'] < 3) style <- 'Dismissing'
  else if (CD < 3 & results[3, 'Raw'] > 3) style <- 'Fearful'
  else style <- 'Not classificable'

  # Show style as a plot
  windows(title="Attachment style")
  plot(CD, results[3, 'Raw'], xlim=c(1,5), ylim=c(1,5), pch=3, cex=2, col='blue', lwd=5,
       xlab='Close/Dependent', ylab='Anxiety', main=paste(id, date.test),
       font.sub=2, sub='Position of the subject is represented by a blue cross')
  abline(v=3)
  abline(h=3)
  text(2, 2, labels='Dismissing', col='gray60', font=2, cex=2)
  text(4, 2, labels='Secure', col='gray60', font=2, cex=2)
  text(2, 4, labels='Fearful', col='gray60', font=2, cex=2)
  text(4, 4, labels='Preocupied', col='gray60', font=2, cex=2)
  # ===== END ADDITIONAL CODE INSERTED MANUALLY

  # Output in form of list
  # ------------------
  results.lst <- list(paste("Total number of missings: ", blanks, " (", pcnt.blanks, "%)", sep=""),
            # ===== ADDITIONAL CODE INSERTED MANUALLY
                      "",
                      "RAAS Collis, 1996. Public domain document downloadable from:",
                      "http://www.openpsychassessment.org/wp-content/uploads/2011/06/",
                      "AdultAttachmentScale.pdf, downloaded: 29 sept 2012.",
                      "",
                      "According to the author, attach styles assignement 'is quite exploratory...",
                      "[use] with caution, and only in conjunction with the continuous measures.'",
                      "",
                      paste("Attach style:", style),
                      "",
                      "T scores computed using mean and standard deviation from 414 USA college students,",
                      "reported by Ledley et al. J Psychopath Behav Assess 2006, 28:33-40."
            # ===== END ADDITIONAL CODE INSERTED MANUALLY
			          )
  
  # Return results
  # ------------------
  return(list(results.lst=results.lst, results.df=results, results.scores=results.scores))

} # end of scoring.fun