#' Read in sequence data
#'
#' @param filepath the filepath to the specific .txt file you are trying to read in
#'
#' @return a dataframe with two columns, column one is for the sequence name, column two is for the sequence data itself, each row is for each sequence entered
#' @export
#'
#' @examples
#' sequencesDF <- readSequences("exampleSequences.txt")
#'
readSequences <- function(filepath) {
  # Read the file as a vector of lines, then join into one string preserving newlines as each sequence is on a newline after the name meaning the newline is crucial to preserve
  sequenceText <- readLines(filepath)
  sequenceText <- paste(sequenceText, collapse = "\n")

  sequenceText <- unlist(strsplit(sequenceText, ">")) # Split on > to separate entries — each split is now one name + sequence
  sequenceText <- sequenceText[sequenceText != ""] # Drop anything before the first > as it is junk or a wrong file, now we can tell them "no sequences found" if their output is empty

  # take the various sequenceTexts and figure out what is the name and what is the sequence
  # this is a function as it needs to run for each entry, not just once or a set number of times, that is what lapply does, don't try and do this any other way it is miserable
  # https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
  sequences <- lapply(sequenceText, function(entry) {

    lines <- strsplit(entry, "\n")[[1]]  # split chunk into individual lines, most of these will be our sequence, the first one will be our name
    lines <- lines[lines != ""]          # drop any blank lines, these are bad junk no need get rid of them the user has uploaded some weird file and we don't want it to gunk up our program

    # The first line is the sequence name everytime, all remaining lines are sequence data everytime
    list(name = lines[1], sequence = paste(lines[-1], collapse = ""))  #extract the first line its the name, then collect the rest and "collapse" them into one big old string
  })

  # this was an error our checks found, we need to prevent junk sequences from being passed through
  sequences <- Filter(function(entry) {
    !is.null(entry$name) && !is.na(entry$name) &&
      nchar(entry$sequence) > 0 &&
      grepl("^[ATGCatgc]+$", entry$sequence)  # reject anything that isn't valid nucleotide characters
  }, sequences)

  # now we have a collection of "sequences"
  # Collect results into a two-column data frame using sapply to go through each entry into sequences
  data.frame(name = vapply(sequences, `[[`, character(1), "name"),
             sequence = vapply(sequences, `[[`, character(1), "sequence"),
             row.names = NULL,
             stringsAsFactors = FALSE)

} # function over boom done
