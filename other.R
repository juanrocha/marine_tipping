library(tidyverse)
revs <- read_csv(file = "~/Documents/Projects/DATA/MREID/mreid_public_release_1.0.csv")

revs |> 
    pull(country)


## pdf
library(pdf)

pdf_subset("data/Review-MarineTipping.pdf", pages = 1, 
           "paper/cover_letter.pdf")
pdf_subset("data/Review-MarineTipping.pdf", pages = 2:13, 
           "paper/response_reviewers.pdf")
