

rm(list=ls())

library(tidyverse)
library(glue)
library(AlgDesign)
library(choiceDes)


outputPath <- ("/Users/joshwhelan/repos/ConjointExample/design")
versCount <- 50
screens <- 4
alts <- 3

#######################################################################################
# use a inputs .csv file to read in labels of specific levels
levels <- readr::read_csv(glue("{outputPath}/inputs.csv"))


# grab attribute names and sizes
labels <- names(levels[,2:ncol(levels)])
size <- rep(NA, ncol(levels)-1)






# build a vector of the sizes
for (i in 2:ncol(levels)) {
    varname <- names(levels[,i])
    curLevels <- levels %>%
        dplyr::select(1, all_of(i)) %>%
        tidyr::drop_na()
size[i-1] <- nrow(curLevels)
}
#######################################################################################


design <- dcm.design(size, versCount, screens, alts)


des <- design$levels %>%
    dplyr::select(-card) %>%
    dplyr::group_by(vers, task) %>%
    dplyr::mutate(alt = 1:n()) %>%
    dplyr::select(vers, task, alt, everything())
#row.names(des) <- NULL

colnames(des) <- c("vers", "task", "alt", labels)

write.csv(des, glue("{outputPath}/design.csv"), row.names=FALSE)

#######################################################################################
#######################################################################################


