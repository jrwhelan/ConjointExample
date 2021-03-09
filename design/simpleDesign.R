
rm(list=ls())

library(tidyverse)
library(glue)


#######################################################################################
#######################################################################################
# use this conjoint package for convenience to start
library(conjointTools)

doe <- conjointTools::makeDoe(
	levels = c(2, 2, 2),
	varNames = c("price", "type", "freshness")
)

survey <- conjointTools::makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 6     # Number of questions per respondent
)

results <- sampleSizer(
    survey   = survey,
    parNames = c('price', 'type', 'freshness'),
    parTypes = c('c', 'd', 'd'),     # Set continuous vs. discrete variables
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


