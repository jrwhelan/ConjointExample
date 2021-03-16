
rm(list=ls())

library(tidyverse)
library(glue)



#######################################################################################
# use a inputs .csv file to read in labels of specific levels
levels <- readr::read_csv("/Users/joshwhelan/repos/ConjointExample/design/inputs.csv")

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




#######################################################################################
#######################################################################################
# use this conjoint package for convenience to start
library(conjointTools)

doe <- conjointTools::makeDoe(
	levels = size,
	varNames = labels
)

survey <- conjointTools::makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

results <- sampleSizer(
    survey   = survey,
    parNames = labels,
    parTypes = c('d', 'c', 'd', 'c', 'd', 'c'),     # Set continuous vs. discrete variables
    interactions = FALSE,             # Add interactions between each attribute
    nbreaks  = 10
)

ggplot(results) +
    geom_point(aes(x = size, y = se, color = coef),
               fill = "white", pch = 21) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = 'Number of observations',
         y = 'Standard Error',
         color = "Variable") +
    theme_bw()

design <- survey %>%
	dplyr::rename(version = respID, task = qID, concept = altID) %>%
	dplyr::select(-obsID)


setwd("/Users/joshwhelan/repos/ConjointExample/design")
write.csv(design, "doe.csv", row.names=FALSE)

#######################################################################################
#######################################################################################


