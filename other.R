library(tidyverse)
revs <- read_csv(file = "~/Documents/Projects/DATA/MREID/mreid_public_release_1.0.csv")

revs |> 
    pull(country)
