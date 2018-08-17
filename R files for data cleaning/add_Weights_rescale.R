# Data loading and tidying for our sheltered and unsheltered data sets
# Jane Carlen
# July 2018
#
# TO DO
# Fix current stint duration variable for 

# Notes from methodology reports ####

# From the 2017 methodology report footnotes:
# [Original] Survey estimates produced in 2017 overestimated the number of unsheltered youth by 2,746 persons.

# Caveats to shelter data being a "complete census":
# "By applying the HMIS data to the non-HMIS providers and DV shelters, we
# implicitly assumed that the distribution of characteristics in the HMIS
# shelters were the same as those in the non-HMIS and DV shelters. Also note
# veterans are overrepresented in the HMIS population; however, this only poses
# aproblem if the characteristics of veteran families in the HMIS data are
# different compared to non-HMIS veteran providers."

# 0. Setup ####

setwd("~/ERt/ert_data_dive/homeless_LA/")

library(dplyr)
library(questionr)
library(plotrix)
library(foreign)

demographic_11 = read.csv("Final Data and Codebooks/Unsheltered/Demo_2011_Final.csv")
demographic_13 = read.csv("Final Data and Codebooks/Unsheltered/Demo_2013_Final.csv")
demographic_15 = read.csv("Final Data and Codebooks/Unsheltered/Demo_2015_Final.csv")
demographic_16 = read.csv("Final Data and Codebooks/Unsheltered/Demo_2016_Final.csv") #includes youth
demographic_17 = read.csv("Final Data and Codebooks/Unsheltered/Demo_2017_Final.csv")

shelter_16 = read.csv("Final Data and Codebooks/Sheltered/HMIS_2016_Final.csv")
shelter_17 = read.csv("Final Data and Codebooks/Sheltered/HMIS_2017_Final.csv")

multiyear = read.csv("Final Data and Codebooks/Multi-Year/Homelessness_Multi_Year_Final.csv") #doesn't have weights

youth = 18:24

#Point-in-time population estimates for LA CoC
#2011: 34622 (16882 sheltered, 17740 unsheltered) 
#2013: 35524 (12934 sheltered, 22590 unsheltered)
#2015: 41174 (12226 sheltered, 28948 unsheltered)
#2016: 43854 (11073 sheltered, 32781 unsheltered), sheltered youth 1152, unsheltered youth 1152, see table p. 14 of 2016 Methodology Report
#2017: 52442 (13972 sheltered, 38470 unsheltered), sheltered youth 1396, unsheltered youth 1705, see table p. 20 of 2017 Methodology Report (ignore unadjusted total)
 ## Original snapshot reports states 55188 total, 41216 unsheletered, 5645 unsheltered youth
 ## but 2017 methodology reports says orignal reports overestimated unsheltered youth by 2,746 persons. 
 ## Shared 2017 methodology report has adjusted estimates.

# 1a. Reformat 2017 weights ####

# Rescale sheltered entries uniformly to sum to estimated shelter population
# Rescale unsheltered so that youth total weights add to estimated total youth unsheltered, and overall adds to total unsheltred pop

n_unshelter_17 = 38470
n_unshelter_youth_17 = 1705
n_shelter_17 = 13972
n_shelter_youth_17 = 1396
n_youth_17 = n_unshelter_youth_17 + n_shelter_youth_17
shelter_17$Weights = 1

demographic_17 = demographic_17 %>%
                    mutate(Weights_rescale =  ifelse(Age %in% youth,
                    n_unshelter_youth_17* Weights/sum(demographic_17$Weights[demographic_17$Age %in% youth]), 
                    (n_unshelter_17 - n_unshelter_youth_17) * Weights/sum(demographic_17$Weights[!demographic_17$Age %in% youth])))


shelter_17 = shelter_17 %>%
  mutate(Weights_rescale =  ifelse(Age %in% youth,
                                   n_shelter_youth_17* Weights/sum(shelter_17$Weights[shelter_17$Age %in% youth]), 
                                   (n_shelter_17 - n_shelter_youth_17) * Weights/sum(shelter_17$Weights[!shelter_17$Age %in% youth])))

#write.csv(demographic_17, "Final Data and Codebooks/Unsheltered/Demo_2017_Final.csv")
#write.csv(shelter_17, "Final Data and Codebooks/Sheltered/HMIS_2017_Final.csv")
#write.csv(demographic_17$Weights_rescale, "Demo_2017_Weights_rescale.csv")
#write.csv(shelter_17$Weights_rescale, "Shelter_2017_Weights_rescale.csv")

# 1b. Reformat demographic_16 weights ####

n_unshelter_16 = 32781
n_unshelter_youth_16 = 1152
n_shelter_16 = 11073
n_shelter_youth_16 = 1152
n_youth_16 = n_unshelter_youth_16 + n_shelter_youth_16
shelter_16$Weights = 1

demographic_16 = demographic_16 %>%
  mutate(Weights_rescale =  ifelse(Age %in% youth,
                                   n_unshelter_youth_16* Weights/sum(demographic_16$Weights[demographic_16$Age %in% youth]), 
                                   (n_unshelter_16 - n_unshelter_youth_16) * Weights/sum(demographic_16$Weights[!demographic_16$Age %in% youth])))


shelter_16 = shelter_16 %>%
  mutate(Weights_rescale =  ifelse(Age %in% youth,
                                   n_shelter_youth_16* Weights/sum(shelter_16$Weights[shelter_16$Age %in% youth]), 
                                   (n_shelter_16 - n_shelter_youth_16) * Weights/sum(shelter_16$Weights[!shelter_16$Age %in% youth])))

#write.csv(demographic_16, "Final Data and Codebooks/Unsheltered/Demo_2016_Full_Final.csv")
#write.csv(shelter_16, "Final Data and Codebooks/Sheltered/HMIS_2016_Final.csv")
#write.csv(demographic_16$Weights_rescale, "Demo_2016_Weights_rescale.csv")
#write.csv(shelter_16$Weights_rescale, "Shelter_2016_Weights_rescale.csv")

# 2. Weighted vs. Unweighted  ####

# How much difference do the weights make?
  # 2017 ####

# age
demographic_17$dummyWeights = 1
tmp = filter(demographic_17, !is.na(Age))
weighted.hist(tmp$Age, w = tmp$dummyWeights, breaks = 10, freq = F)
weighted.hist(tmp$Age, w = tmp$Weights_rescale, breaks = 10, freq = F, add = T, col = "red", density = 45)

# ethnicity
barplot(prop.table(wtd.table(tmp$Ethnicity, w = tmp$dummyWeights)))
barplot(prop.table(wtd.table(tmp$Ethnicity, w = tmp$Weights_rescale)), add = T, col = "red", density = 45)

# veteran
barplot(prop.table(wtd.table(tmp$Veteran, w = tmp$dummyWeights)))
barplot(prop.table(wtd.table(tmp$Veteran, w = tmp$Weights_rescale)), add = T, col = "red", density = 45)

# Cencus Tract distribution
barplot(sort(prop.table(wtd.table(tmp$Census_Tract, w = tmp$dummyWeights)), decreasing = T), ylim = c(0,.05))
barplot(sort(prop.table(wtd.table(tmp$Census_Tract, w = tmp$Weights_rescale)), decreasing = T),
                    add = T, col = "red", density = 45)


  # 2016 ####

# age
demographic_16$dummyWeights = 1
tmp = filter(demographic_16, !is.na(Age))
weighted.hist(tmp$Age, w = tmp$dummyWeights, breaks = 10, freq = F)
weighted.hist(tmp$Age, w = tmp$Weights_rescale, breaks = 10, freq = F, add = T, col = "red", density = 45)

# ethnicity
barplot(prop.table(wtd.table(tmp$Ethnicity, w = tmp$dummyWeights)))
barplot(prop.table(wtd.table(tmp$Ethnicity, w = tmp$Weights_rescale)), add = T, col = "red", density = 45)

# veteran
barplot(prop.table(wtd.table(tmp$Veteran, w = tmp$dummyWeights)))
barplot(prop.table(wtd.table(tmp$Veteran, w = tmp$Weights_rescale)), add = T, col = "red", density = 45)

# Cencus Tract distribution
barplot(sort(prop.table(wtd.table(tmp$Census_Tract, w = tmp$dummyWeights)), decreasing = T), ylim = c(0,.05))
barplot(sort(prop.table(wtd.table(tmp$Census_Tract, w = tmp$Weights_rescale)), decreasing = T),
        add = T, col = "red", density = 45)


