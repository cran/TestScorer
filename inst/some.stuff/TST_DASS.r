# DASS scale scoring commands
# Creation date: 2013-04-02
# --------------

testChar <- list(
  acronym="DASS",
  name="Depression, Anxiety & Stress Scales",
  ref="Lovibond & Lovibond, 1995",
  n.items=42,
  valid=c(0, 1, 2, 3),
  miss=c(4),
  comm="Public domain: ww2.psy.unsw.edu/dass/"
) # end testChar

scoring.fun <- function(answers, sex, age=0, id, date.test, comm)
# "answer" is a *character* vector as introduced through the keyboard.
{
  answers <- as.numeric(answers) # transform to numeric for easier scoring
  answers[answers %in% c(4)] <- NA # missing characters to NA
  blanks <- sum(is.na(answers)) # compute number of missings
  pcnt.blanks <- round((blanks / 42) * 100) # compute % of missings
  results <- data.frame(NULL) # Null data frame for results

  # D scale scoring commands
  # --------------
  results[2, "Acronym"] <- "D" # acronym
  results[2, "Scale"] <- "Depression" # name of the scale
  items <- c(3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42) # items making up the scale
  results[2, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[2, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)
  trans.table <- c("5-30", "35-45", "50-55", "60-65", "70-75", "75-79", "80-83", "84-86", "87-89", "90", "91", "92", "93", "94", "94", "95", "95", "96", "96", "96", "97", "97", "98", "98", "98", "98", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99", "99")
  results[2, "Centil"] <- trans.table[round(results[2, "Raw"]) + 1] # + 1, because trans begins at raw score 0

  # A scale scoring commands
  # --------------
  results[1, "Acronym"] <- "A" # acronym
  results[1, "Scale"] <- "Anxiety" # name of the scale
  items <- c(2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41) # items making up the scale
  results[1, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[1, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)
  trans.table <- c("5-20", "25-35", "40-45", "50-55", "60", "65", "70", "75", "79-78", "79-81", "82-83", "84-85", "86", "87", "88-89", "90", "91", "92", "93", "93", "94", "94", "95", "95", "96", "96", "96", "97", "97", "97", "87", "98", "98", "98", "98", "98", "99", "99", "99", "99", "99", "99", "99")
  results[1, "Centil"] <- trans.table[round(results[1, "Raw"]) + 1] # + 1, because trans begins at raw score 0

  # S scale scoring commands
  # --------------
  results[3, "Acronym"] <- "S" # acronym
  results[3, "Scale"] <- "Stress" # name of the scale
  items <- c(1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39) # items making up the scale
  results[3, "Miss"] <- sum(is.na(answers[items])) # number of missings
  results[3, "Raw"] <- sum(answers[items], na.rm=TRUE) # raw score (sum answered items)
  trans.table <- c("5", "10", "15", "20-25", "30", "35", "40", "45", "50-55", "60", "65", "65", "70", "75-77", "78-80", "81-83", "83-84", "85-86", "87-88", "89", "90", "91", "92", "93", "93", "94", "95", "95", "96", "96", "97", "97", "97", "97", "98", "98", "98", "99", "99", "99", "99", "99", "99")
  results[3, "Centil"] <- trans.table[round(results[3, "Raw"]) + 1] # + 1, because trans begins at raw score 0

  # One row data frame with scores, for writing scores into a data file
  # --------------------
  results.scores <- data.frame(t(c(blanks, results[, "Raw"], results[,"Centil"])))
  names(results.scores) <- c("blanks", paste(results[["Acronym"]], "raw", sep="_"), paste(results[["Acronym"]], "C", sep="_"))

  # Output in form of list
  # ------------------
  results.lst <- list('Centils calculated from 1.771 members of both sexes',
                      'of the UK general adult population. Data from Crawford JR,',
                      'Crawford JD. Br J Clin Psuchol, 2003, 42, 111:131.\n',
                      paste("Total number of missings: ", blanks, " (", pcnt.blanks, "%)", sep=""))

  # Return results
  # ------------------
  return(list(results.lst=results.lst, results.df=results, results.scores=results.scores))

} # end of scoring.fun