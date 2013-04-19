# MHLC scale scoring commands
# Creation date: 2013-04-02
# --------------

testChar <- list(
  acronym="MHLC",
  name="Multidimensional Health Locus of Control",
  ref="Wallston, Wallston & DeVelis, 1978",
  n.items=18,
  valid=c(1, 2, 3, 4, 5, 6),
  miss=c(0),
  comm="Public domain: www.vanderblit.edu/nursing/kwallston/mhlscales.htm"
) # end testChar

scoring.fun <- function(answers, sex, age=0, id, date.test, comm)
# "answer" is a *character* vector as introduced through the keyboard.
{
  answers <- as.numeric(answers) # transform to numeric for easier scoring
  answers[answers %in% c(0)] <- NA # missing characters to NA
  blanks <- sum(is.na(answers)) # compute number of missings
  pcnt.blanks <- round((blanks / 18) * 100) # compute % of missings
  results <- data.frame(NULL) # Null data frame for results

  # I scale scoring commands
  # --------------
  results[1, "Acronym"] <- "I" # acronym
  results[1, "Scale"] <- "Internal" # name of the scale
  items <- c(1, 6, 8, 12, 13, 17) # items making up the scale
  results[1, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[1, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)

  # C scale scoring commands
  # --------------
  results[2, "Acronym"] <- "C" # acronym
  results[2, "Scale"] <- "Chance" # name of the scale
  items <- c(2, 4, 9, 11, 15, 16) # items making up the scale
  results[2, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[2, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)

  # PO scale scoring commands
  # --------------
  results[3, "Acronym"] <- "PO" # acronym
  results[3, "Scale"] <- "Powerfull Others" # name of the scale
  items <- c(2, 5, 7, 10, 14, 18) # items making up the scale
  results[3, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[3, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)

  # One row data frame with scores, for writing scores into a data file
  # --------------------
  results.scores <- data.frame(t(c(blanks, results[, "Raw"])))
  names(results.scores) <- c("blanks", paste(results[["Acronym"]], "raw", sep="_"))

  # Output in form of list
  # ------------------
  results.lst <- list(paste("Total number of missings: ", blanks, " (", pcnt.blanks, "%)", sep=""))

  # Return results
  # ------------------
  return(list(results.lst=results.lst, results.df=results, results.scores=results.scores))

} # end of scoring.fun