#' Read in sequence data
#'
#' @param filepath the filepath to the specific .txt file you are trying to read in
#'
#' @return a dataframe with two columns, column one is for the sequence name, column two is for the sequence data itself, each row is for each sequence entered
#' @export
#'
#' @examples
#' path <- system.file("extdata", "exampleSequences.txt", package = "DNAtranslate")
#' sequencesDF <- readSequences(path)
readSequences <- function(filepath) {

  checkmate::assert_true(grepl("\\.txt$", filepath))  # only accept .txt files

  # Read the file as a vector of lines, then join into one string keeping the newlines as each sequence is on a newline after the name meaning the newline is crucial to preserve so we can find sequences later
  sequenceText <- readLines(filepath)
  sequenceText <- paste(sequenceText, collapse = "\n") # take this vector of strings and make it one big string

  sequenceText <- unlist(strsplit(sequenceText, ">")) # Split the string on > to separate sequence names to make individual vectors (unlist makes it a plain vector) — each vector is now one name + sequence
  sequenceText <- sequenceText[sequenceText != ""] # Drop anything before the first > as it is junk or a wrong file, now we can tell them "no sequences found" if their output is empty and we are left with a vector containing each ">sequenceName\nATGCATGC"

  # take the various sequenceTexts and figure out what is the name and what is the sequence
  # this needs to run for each entry, not just once or a set number of times, that is what lapply does, don't try and do this any other way it is miserable (for loop)
  # lappy says for each item in the list sequenceText do the following...
  # https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
  sequences <- lapply(sequenceText, function(listEntry) {

    lines <- strsplit(listEntry, "\n")[[1]]  # split listEntry into individual lines, most of these will be our sequence, the first one will be our name
    lines <- lines[lines != ""]          # drop any blank lines, get rid of them the user has uploaded some weird file

    # The first line is the sequence name everytime, all remaining lines are sequence data every time
    list(name = trimws(lines[1]), sequence = gsub(" ", "", paste(lines[-1], collapse = "")))  #extract the first line its the name, then collect the rest and "collapse" them into one big old string
  }) # updated regex to stop errors but it now allows for " " inside the sequence of .txt files, but " " is not actually a DNA sequence so gsub will replace all " " with "" this is needed as trimws() only removes leading/trailing whitespace and we need to remove ALL spaces from the sequence

  # this was an error our checks found, we need to prevent junk sequences from being passed through
  sequences <- Filter(function(listEntry) {
    !is.null(listEntry$name) && !is.na(listEntry$name) && # check to make sure it has a name, isn't just a sequence
      nchar(listEntry$sequence) > 0 && # check to make sure it actually has a sequence
      grepl("^[ATGCatgc]+$", listEntry$sequence)  # reject anything that isn't valid nucleotide characters
  }, sequences)

  # now we have a list of lists. The inner list has the "sequence name" and "sequence"
  # Condense these lists into a two-column data frame using vapply to go through each entry similar to how we did above but now we use vapply to make it into a vector instead of a list
  data.frame(name = vapply(sequences, `[[`, character(1), "name"), # [[ extracts a single element from a list, if we did [ it would extract a list for the entry instead not the value
             sequence = vapply(sequences, `[[`, character(1), "sequence")) # this does the same as above but for the sequence data
}
