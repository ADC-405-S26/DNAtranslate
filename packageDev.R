install.packages(c("available", "pak"))
library(available,pak)
available("DNAtranslate")
pak::pkg_name_check("DNAtranslate")

install.packages(c("devtools", "checkmate", "tidyverse", "remotes"))
library(devtoools, checkmate, tidyverse, remotes)

usethis::create_package("/cloud/project")
getwd()

usethis::use_r("readSequences")

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
  # now we have a collection of "sequences"
  # Collect results into a two-column data frame using sapply to go through each entry into sequences
  data.frame(name = sapply(sequences, `[[`, "name"), # grab all the names for each entry in sequences
              sequence = sapply(sequences, `[[`, "sequence"), # grab all the sequences within the sequenences list
              row.names = NULL,
              stringsAsFactors = FALSE)
} # function over boom done, everything imported

df <- readSequences("HisD_TA1538_Mutant_Genotype.txt")  # test the function on a normal example that isnt bad
write.csv(df, "sequences.csv") # write the result to a CSV

# okay it works
devtools::load_all()
