# Processing duration variables we'll use to build annualized population estimates
#
# Notes:
# For only 2016 and 2017 do we have clean sheltered and unsheltered data WITH sampling weights
##########################################################################################################

# 0. Setup ####

library(dplyr)
library(questionr)
library(weights)
library(foreign)
library(fitdistrplus)
library(compoisson)

# could also load 11, 13 and 15, but they don't have weights ('15 has incomplete weights and some census tracts have only NA weights so not straightforward to reconstruct)
demographic_16 = read.csv("../Final Data and Codebooks/Unsheltered/Demo_2016_Final.csv") #includes youth
demographic_17 = read.csv("../Final Data and Codebooks/Unsheltered/Demo_2017_Final.csv")

shelter_16 = read.csv("../Final Data and Codebooks/Sheltered/HMIS_2016_Final.csv")
shelter_17 = read.csv("../Final Data and Codebooks/Sheltered/HMIS_2017_Final.csv")

table2 <- function(table) {round(prop.table(table), 2)}
##########################################################################################################
# 2017 ####
#########################################################################################################
# 1. Process Unsheltered Duration Variables?####

# compare year to year - pretty similar
table2(table(demographic_16$Current_Stint_Duration))
wtd.table(demographic_16$Current_Stint_Duration, weights = demographic_16$Weights_rescale/sum(demographic_16$Weights_rescale))

table2(table(demographic_17$Current_Stint_Duration))
wtd.table(demographic_17$Current_Stint_Duration, weights = demographic_17$Weights_rescale/sum(demographic_17$Weights_rescale))

#    data by month (chunky) vs. by quarter (smooth) vs. by day (very chunky)

#for consistency:
demographic_17$Current_Stint_Duration_Days = demographic_17$Current_Stint_Duration_Detailed

demographic_17$Current_Stint_Duration_Months = cut(demographic_17$Current_Stint_Duration_Detailed,
                                                   breaks = c(0, 31, 61, 92, 122, 152, 183, 213, 243, 274, 34, 334, 364, Inf),
                                                   labels = c("up to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5", "5 to 6", "6 to 7",
                                                              "7 to 8", "8 to 9", "9 to 10", "10 to 11", "11 to 12", "12+"),
                                                   right = T)

demographic_16$Current_Stint_Duration_Months = cut(demographic_16$Current_Stint_Duration_Detailed,
                                                   breaks = c(0, 31, 61, 92, 122, 152, 183, 213, 243, 274, 34, 334, 364, Inf),
                                                   labels = c("up to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5", "5 to 6", "6 to 7",
                                                              "7 to 8", "8 to 9", "9 to 10", "10 to 11", "11 to 12", "12+"),
                                                   right = T)

demographic_17$Current_Stint_Duration_Quarters = cut(demographic_17$Current_Stint_Duration_Detailed,
                                                   breaks = c(0, 92, 183, 274, 364, Inf),
                                                   labels = c("1","2","3","4", "5+"),
                                                   right = T)

demographic_16$Current_Stint_Duration_Quarters = cut(demographic_16$Current_Stint_Duration_Detailed,
                                                     breaks = c(0, 92, 183, 274, 364, Inf),
                                                     labels = c("1","2","3","4", "5+"),
                                                     right = T)




# 2. Process Sheltered Duration Variables ####

hmis_2017_tmp = read.spss("../Raw Data Files/Sheltered/2017 Individual HMIS data (11,397) 12-3-2017.sav", to.data.frame = T)

#####   What variable to use for current stint duration for sheltered people? ####

# Paul: For HMIS_2017 there are two variables in the raw data that based on the
# documentation seem like they could represent the current stint duration:
# hmls_months_obs and since_hmls_mo. The initial one I used was since_hmls_mo,
# but based on what I found in this data code book I think hmls_months_obs is
# the winner.
# Jane: actually If since_hmls_mo is NA, so is hmls_months_obs: 
# filter(hmis_2017_duration, is.na(since_hmls_mo))$hmls_months_obs %>% table()
# obs = "obsolete"

# since_hmls vars (since_hmls_mo, since_hmls_days, and hmls_start_dt) are derived from "Approximate date this period of homelessness started."
# since_entry vars (entrydt, since_entry_mo, since_entry_days, etc) are derived from "Project entry data. Collects date at first entry into project (can be historical)" --

# Letters are variations of missing: 
# M (blank), N (null), C (safety concern or not collected), D (Don't know),
# V (Valid skip), A (due to age skip), Y (Yes, but missing due to age), X (other value)

#   Exploratory 2017:  ####

hmis_2017_duration = cbind(dplyr::select(hmis_2017_tmp, starts_with("since_")),
                           dplyr::select(hmis_2017_tmp, starts_with("hmls_")),
                           dplyr::select(hmis_2017_tmp, contains("prior")))
dim(.Last.value)

summary(hmis_2017_duration)
sum(hmis_2017_duration$since_entry_days <= hmis_2017_duration$since_hmls_days, na.rm = T)
# What is days since entry significantly greater than duration of homeless episode? (not often)
#filter(hmis_2017_duration, hmis_2017_duration$since_entry_days > hmis_2017_duration$since_hmls_days + 10)[,1:4]
hist(hmis_2017_duration$since_hmls_days - hmis_2017_duration$since_entry_days, breaks = 10000, xlim = c( -20, 100))
hist(as.numeric(hmis_2017_duration$hmls_months_obs) - hmis_2017_duration$since_hmls_mo, breaks = 300, xlim = c(-300, 100))
head(filter(hmis_2017_duration, hmls_months_obs == "N  "), 6)
hist(hmis_2017_duration$since_hmls_mo - hmis_2017_duration$since_entry_mo, breaks = 1000, xlim = c( -5, 20))
#table(hmis_2017_duration$since_hmls_mo - hmis_2017_duration$since_entry_mo, useNA = "always")

# plot
hist(hmis_2017_duration$since_entry_mo[!is.na(hmis_2017_duration$since_hmls_mo)], freq = F, breaks = 40, ylim = c(0, .5), col = "blue", main = "Months since entry")
legend("topright", c("Months since entry; month since hmls not NA", "Months since homeless",
                     "Months since entry; month since hmls NA", "Months since entry overall"),
       fill = c("blue", "green", "red", "purple"), cex = .7)
hist(hmis_2017_duration$since_hmls_mo[!is.na(hmis_2017_duration$since_hmls_mo)], freq = F, col = "green", add = T, breaks = 800, density = 45, angle = -45)
hist(hmis_2017_duration$since_entry_mo[is.na(hmis_2017_duration$since_hmls_mo)], freq = F, col = "red", add = T, breaks = 40, density = 45)
hist(hmis_2017_duration$since_entry_mo, freq = F, add = T, breaks = 40, density = 95, col = "purple")

hist(hmis_2017_duration$since_entry_days[!is.na(hmis_2017_duration$since_hmls_days)],
     freq = F, breaks = 40, ylim = c(0, .01), col = "blue", main = "days since entry")
legend("topright", c("days since entry; days since hmls not NA", "days since homeless",
                     "days since entry; days since hmls NA", "days since entry overall"),
       fill = c("blue", "green", "red", "purple"), cex = .7)
hist(hmis_2017_duration$since_hmls_days[!is.na(hmis_2017_duration$since_hmls_days)],
     freq = F, col = "green", add = T, breaks = 200, density = 45, angle = -45)
hist(hmis_2017_duration$since_entry_days[is.na(hmis_2017_duration$since_hmls_days)],
     freq = F, col = "red", add = T, breaks = 20, density = 45)
hist(hmis_2017_duration$since_entry_days,
     freq = F, add = T, breaks = 40, density = 95, col = "purple")

#   Use a combination ####

shelter_raw_17 = read.spss("../Raw Data Files/Sheltered/2017 Individual HMIS data (11,397) 12-3-2017.sav",
                           to.data.frame= T)
#as.list(attr(shelter_raw_17, "variable.labels"))
#table(shelter_raw_17$exitdt, useNA = "always")
#sort(unique(shelter_raw_17$exitdt))

#check same:
sum(hmis_2017_tmp$age_yrs != shelter_17$Age, na.rm = T)

# 1. Use since_hmls_days if available.
# 2. If not, use since_entry_days <-- why are the day counts so long? would expect 
# 3. If using since_entry_days add prior duration if prior_hmls <- where did this variable come from?
# Emergency shelter, Transitional housing, Safe haven, Hotel or motel paid for w/o emergency shelter voucher, Interim housing (including interim housing (~transitional) based on the HMIS intake form)

shelter_17 = shelter_17 %>%
  mutate(Prior_Homeless = ifelse(Prior_Living_Situation %in%
                                   c("Place not meant for habitation", "Emergency shelter", "Safe haven",
                                     "Hotel or motel paid for w/o emergency shelter voucher", "Interim Housing"), TRUE, FALSE),
         Since_Homeless_Days = hmis_2017_tmp$since_hmls_days,
         Since_Entry_Days = hmis_2017_tmp$since_entry_days,
         Since_Homeless_Months = hmis_2017_tmp$since_hmls_mo,
         Since_Entry_Months = hmis_2017_tmp$since_entry_mo)

#table(shelter_17$Prior_Homeless, useNA = "always")
#filter(shelter_17, Prior_Homeless == T)$Prior_Living_Situation_Duration
hist(shelter_17$Since_Entry_Days[shelter_17$Prior_Homeless == F], xlim = c(0,32), breaks = 4000)

# I don't see a variable to describe if this prior homelessness was in LA CoC, so i can't incorporate that

# Use sample value, repeat so it's smooth

set.seed(1)

n = 3

Prior_Living_Situation_Days = as.numeric(as.character(recode(shelter_17$Prior_Living_Situation_Duration,
                                                             "1-3 months" = sample(30:93, 1), "12+ months" = as.integer(365),
                                                             "4-11 months" = sample(120:364, 1), "One night or less" = as.integer(1),
                                                             "One week or more, but less than one month" = sample(7:30, 1),
                                                             "Two to six nights"= sample(2:6, 1), "Unknown" = as.integer(0))))

for (i in 2:n) {Prior_Living_Situation_Days = Prior_Living_Situation_Days +
  as.numeric(as.character(recode(shelter_17$Prior_Living_Situation_Duration,
                                 "1-3 months" = sample(30:93, 1), "12+ months" = as.integer(365),
                                 "4-11 months" = sample(120:364, 1), "One night or less" = as.integer(1),
                                 "One week or more, but less than one month" = sample(7:30, 1),
                                 "Two to six nights"= sample(2:6, 1), "Unknown" = as.integer(0))))
}

Prior_Living_Situation_Days = Prior_Living_Situation_Days/n

shelter_17$Prior_Living_Situation_Days = Prior_Living_Situation_Days
shelter_17$Prior_Living_Situation_Days[shelter_17$Prior_Living_Situation_Days == 0] = NA


shelter_17 = shelter_17 %>% mutate(Current_Stint_Duration_Days = ifelse(!is.na(Since_Homeless_Days), Since_Homeless_Days, Since_Entry_Days), #1-2
                                   Current_Stint_Duration_Days = ifelse(is.na(Since_Homeless_Days) & shelter_17$Prior_Homeless == TRUE,
                                                                        Since_Entry_Days + Prior_Living_Situation_Days, Since_Entry_Days))

shelter_17$Current_Stint_Duration = cut(shelter_17$Current_Stint_Duration_Days, breaks = c(0, 7, 14, 32, 93, 364, Inf), right = T)
shelter_17$Current_Stint_Duration_Months = cut(shelter_17$Current_Stint_Duration_Days,
                                               breaks = c(0, 31, 61, 92, 122, 152, 183, 213, 243, 274, 34, 334, 364, Inf),
                                               labels = c("up to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5", "5 to 6", "6 to 7",
                                                          "7 to 8", "8 to 9", "9 to 10", "10 to 11", "11 to 12", "12+"),
                                               right = T)

shelter_17$Current_Stint_Duration_Quarters = cut(shelter_17$Current_Stint_Duration_Days,
                                               breaks = c(0, 92, 183, 274, 364, Inf),
                                               labels = c("1", "2", "3", "4", "5+"),
                                               right = T)


#table(shelter_17$Current_Stint_Duration)
#table(shelter_17$Current_Stint_Duration_Days)
#table(shelter_17$Current_Stint_Duration_Months)
plot(table(shelter_17$Current_Stint_Duration_Months), type = "l", xlim = c(0, 15))
plot(table(shelter_17$Since_Entry_Months), type = "l", xlim = c(0, 15))
plot(table(shelter_17$Since_Homeless_Months), type = "l", xlim = c(0, 12))
plot(table(floor(shelter_17$Prior_Living_Situation_Days/30)) , type = "l", xlim = c(0, 12))

# 3. Overall distributions ####
# note data suggests people more likely to go into shelter after longer time homeless

par(mfrow = c(3,3))

#days

wtd.hist(c(shelter_17$Current_Stint_Duration_Days, demographic_17$Current_Stint_Duration_Days),
         weight = c(shelter_17$Weights_rescale, demographic_17$Weights_rescale), xlim = c(0,500),
         breaks = 3000); abline(v = 365, col = "red")

wtd.hist(shelter_17$Current_Stint_Duration_Days,
         weight = c(shelter_17$Weights_rescale), xlim = c(0,500),
         breaks = 1000); abline(v = 365, col = "red")

wtd.hist(demographic_17$Current_Stint_Duration_Days,
         weight = c(demographic_17$Weights_rescale), xlim = c(0,500),
         breaks = 1000); abline(v = 365, col = "red")

#months

wtd.hist(c(shelter_17$Current_Stint_Duration_Months, demographic_17$Current_Stint_Duration_Months),
         weight = c(shelter_17$Weights_rescale, demographic_17$Weights_rescale), xlim = c(0,13),
         breaks = 3000); abline(v = 365, col = "red")

wtd.hist(c(shelter_17$Current_Stint_Duration_Months),
         weight = c(shelter_17$Weights_rescale), xlim = c(0,13),
         breaks = 1000); abline(v = 365, col = "red")

wtd.hist(c(demographic_17$Current_Stint_Duration_Months),
         weight = c(demographic_17$Weights_rescale), xlim = c(0,13),
         breaks = 1000); abline(v = 365, col = "red")

#quarters
wtd.table(c(shelter_17$Current_Stint_Duration_Quarters, demographic_17$Current_Stint_Duration_Quarters),
          weight = c(shelter_17$Weights_rescale, demographic_17$Weights_rescale));

wtd.hist(c(shelter_17$Current_Stint_Duration_Quarters, demographic_17$Current_Stint_Duration_Quarters),
         weight = c(shelter_17$Weights_rescale, demographic_17$Weights_rescale), xlim = c(0,5),
         breaks = 3000); abline(v = 365, col = "red")

wtd.hist(c(shelter_17$Current_Stint_Duration_Quarters),
         weight = c(shelter_17$Weights_rescale), xlim = c(0,5),
         breaks = 1000); abline(v = 365, col = "red")

wtd.hist(c(demographic_17$Current_Stint_Duration_Quarters),
         weight = c(demographic_17$Weights_rescale), xlim = c(0,5),
         breaks = 1000); abline(v = 365, col = "red")

#########################################################################################################
#   Exploratory 2016 ####
  
shelter_raw_16 = read.spss("../Raw Data Files/Sheltered/2016 HMIS dataset with all people in households (13,690) 5-31-2017.sav",
                           to.data.frame= T)

hmis_2016_tmp = read.spss("../Raw Data Files/Sheltered/2016 HMIS dataset with all people in households (13,690) 5-31-2017.sav",
                          to.data.frame = T)

hmis_2016_duration = cbind(dplyr::select(hmis_2017_tmp, starts_with("since_")),
                           dplyr::select(hmis_2017_tmp, starts_with("hmls_")),
                           dplyr::select(hmis_2017_tmp, contains("prior")))


