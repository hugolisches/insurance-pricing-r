
library(dplyr)
library(tidyr)
require(lifecontingencies)
#importing the lifetable of Switzerland (female)
lifetable = read.csv("data/lifetable_CH_female.csv")
str(lifetable)
###only want to use the year 2022 and shorten it to the number of people still
###alive at age x
lifetable = lifetable[lifetable$PERIOD == 2022, ]
lifetable = lifetable[lifetable$INDICATOR == "S(X) ", ]
colnames(lifetable)[colnames(lifetable) == "VALUE"] = "l_x"
### adding p_x and q_x 
lifetable <- lifetable %>%
  mutate(p_x = lead(l_x) / l_x)
lifetable <- lifetable %>%
  mutate(q_x = 1 - p_x)
###since the couple is 60 i am only interested in the table from 60 onwards
lifetable=lifetable[lifetable$AGE..X.>=60,]
################################################
#Basic product
# Define the annual interest rate and compute the annual discount factor
i = 0.0232 # Annual interest rate
v = 1 / (1 + i) # Annual discount factor

# Initialize variables for storing calculations, only for my own understanding and checking ig plausible
calc_df = data.frame(k = integer(), age = integer(), survival_to_month = numeric(), month_qx = numeric(), discounted_month_qx = numeric())

sum_qx = 0
# Initialize variable for storing survival probability up to the start of the current year
survival_up_to_this_year = 1

for(k in 0:59) {
  age = 60 + floor(k / 12)
  
  # At the start of each new year, update the survival probability and q_x
  if(k %% 12 == 0) {
    if(k > 0) { # Update survival probability at the start of each year after age 60
      survival_up_to_this_year =survival_up_to_this_year * (1 - lifetable$q_x[lifetable$AGE..X. == (age - 1)])
    }
    q_x = lifetable$q_x[lifetable$AGE..X. == age]
  }
  
  # Calculate survival probability to the beginning of the month within the current year
  survival_to_month = survival_up_to_this_year * ((1 - q_x/12) ^ (k %% 12))
  
  # Probability of dying in the current month
  month_qx = survival_to_month * (q_x / 12)
  
  # Discount this month's qx back to present value
  discounted_month_qx = (v ^ ((k + 1) / 12)) * month_qx
  sum_qx = sum_qx+discounted_month_qx
  
  # Add calculations to dataframe
  calc_df = rbind(calc_df, data.frame(k = k, age = age, survival_to_month = survival_to_month, month_qx = month_qx, discounted_month_qx = discounted_month_qx))
}

#calculating survival probability to 65 directly 
l_60 = lifetable$l_x[lifetable$AGE..X. == 60]
l_65 = lifetable$l_x[lifetable$AGE..X. == 65]
p_60_5 = l_65 / l_60
v5_p60 = v^5 * p_60_5

# Final calculation of A_60_5_12
A_60_5_12 = sum_qx + v5_p60

# Add _5p60 calculation to dataframe
calc_df = rbind(calc_df, data.frame(k = 'p_60_5', age = NA, survival_to_month = NA, month_qx = p_60_5, discounted_month_qx = v5_p60))

# Output the final value of A_60_5_12 
A_60_5_12
# monthly premium
A = (200000 * (1 - v^(1/12)) * v^5) / (1 - A_60_5_12)
A
#################second moment
sum_qx = 0
survival_up_to_this_year = 1

for(k in 0:59) {
  age = 60 + floor(k / 12)
  
  if(k %% 12 == 0) {
    if(k > 0) {
      survival_up_to_this_year = survival_up_to_this_year * (1 - lifetable$q_x[lifetable$AGE..X. == (age - 1)])
    }
    q_x = lifetable$q_x[lifetable$AGE..X. == age]
  }
  
  survival_to_month = survival_up_to_this_year * ((1 - q_x/12) ^ (k %% 12))
  month_qx = survival_to_month * (q_x / 12)
  #####adjust here times 2
  discounted_month_qx = (v ^ (2*(k + 1) / 12)) * month_qx
  sum_qx = sum_qx + discounted_month_qx
  
  calc_df = rbind(calc_df, data.frame(k = k, age = age, survival_to_month = survival_to_month, month_qx = month_qx, discounted_month_qx = discounted_month_qx))
}

l_60 = lifetable$l_x[lifetable$AGE..X. == 60]
l_65 = lifetable$l_x[lifetable$AGE..X. == 65]
p_60_5 = l_65 / l_60
#adjust here: 5 to 10
v5_p60 = v^10 * p_60_5
two_A_60_5_12 = sum_qx + v5_p60
two_A_60_5_12
A_60_5_12^2
Variance=two_A_60_5_12-A_60_5_12^2
Variance
factor=(A * (1/v)^5 / (1 - v^(1/12)))^2
Variance_final=Variance*factor
Variance_final

#############################
#complex product 
lifetablefemale = read.csv("lifetable_CH_female.csv")
lifetablemale = read.csv("lifetable_CH_male.csv")

# Filtering for the year 2022 and the indicator "S(X)"
lifetablefemale = lifetablefemale[lifetablefemale$PERIOD == 2022 & lifetablefemale$INDICATOR == "S(X) ", ]
lifetablemale = lifetablemale[lifetablemale$PERIOD == 2022 & lifetablemale$INDICATOR == "S(X) ", ]

# Renaming the "VALUE" column to "l_x"
colnames(lifetablefemale)[colnames(lifetablefemale) == "VALUE"] = "l_x"
colnames(lifetablemale)[colnames(lifetablemale) == "VALUE"] = "l_x"

# Since interested in ages from 65 onwards
lifetablefemale = lifetablefemale[lifetablefemale$AGE..X. >= 65, ]
lifetablemale = lifetablemale[lifetablemale$AGE..X. >= 65, ]

# Selecting relevant columns
lifetablefemale_selected = lifetablefemale[, c("AGE..X.", "l_x")]
colnames(lifetablefemale_selected) = c("AGE", "l_x_f")  # Renaming columns for clarity

lifetablemale_selected = lifetablemale[, c("AGE..X.", "l_x")]
colnames(lifetablemale_selected) = c("AGE", "l_x_m")  # Renaming columns for clarity

# Merging the life tables based on AGE
lifetable = merge(lifetablefemale_selected, lifetablemale_selected, by = "AGE")
# assuming independence of death for couple 
# prob of both surviving is product of each surviving
lifetable$l_x =lifetable$l_x_f*lifetable$l_x_m
lifetable = lifetable[, !(names(lifetable) %in% c("l_x_f", "l_x_m"))]
#including fractional ages

# Initialize lifetable_final with columns from lifetable
lifetable_final = data.frame(AGE = numeric(), l_x = numeric(), year = numeric(), year.frac = numeric())
initial_year=1
# Loop through each age from 65 to the last age in lifetable
for(i in 1:nrow(lifetable)) {
  age = lifetable$AGE[i]
  l_x_current <- ifelse(i < nrow(lifetable), lifetable$l_x[i], NA) # Current l_x, NA for last
  l_x_next <- ifelse(i < nrow(lifetable), lifetable$l_x[i + 1], NA) # Next l_x, NA for last
  
  # Calculate new l_x values for fractional ages
  l_x_7_12 <- ifelse(!is.na(l_x_next), l_x_current - (l_x_current - l_x_next) * 7/12, NA)
  l_x_11_12 <- ifelse(!is.na(l_x_next), l_x_current - (l_x_current - l_x_next) * 11/12, NA)
  
  # Determine the year and fractional years
  year <- initial_year + (age - 65)
  
  # Append current age row
  lifetable_final <- rbind(lifetable_final, data.frame(AGE = age, l_x = l_x_current, year = year, year.frac = year))
  
  # Append fractional age rows if not the last age
  if(!is.na(l_x_next)) {
    lifetable_final <- rbind(lifetable_final, 
                             data.frame(AGE = age + 7/12, l_x = l_x_7_12, year = year, year.frac = year + 7/12),
                             data.frame(AGE = age + 11/12, l_x = l_x_11_12, year = year, year.frac = year + 11/12))
  }
}
lifetable_final = lifetable_final[order(lifetable_final$AGE),]
#year-1
lifetable_final$year=lifetable_final$year-1
lifetable_final$year.frac=lifetable_final$year.frac-1
#i use the same v as in the exercise before (does not really make
#sense to take 5 year mortgage for life long insurance but for simplicity i don't change it)
#adding discounted cash flow for all payment including the geometrically increasing part
lifetable_final$dcf=(v^lifetable_final$year.frac)*((1.01)^lifetable_final$year)
#stor l_60
l_60=lifetable_final$l_x[1]
p = numeric(nrow(lifetable_final))
for (i in 1:(nrow(lifetable_final))) {
  p[i] = (lifetable_final$l_x[i] / l_60) * ((lifetable_final$l_x[i] - lifetable_final$l_x[i + 1]) / l_60)
}
#adding probability
lifetable_final$p=p
#calculating the cumulative cashflows
lifetable_final$cum_dcf = cumsum(lifetable_final$dcf)
#multiplying them with the probability
lifetable_final$p_x_cumsum=lifetable_final$p*lifetable_final$cum_dcf
APV=sum(na.omit(lifetable_final$p_x_cumsum))
#scaling for the result
APV*4000
###variance
#second moment with squaring the cumulative cashflows
lifetable_final$dcf_squared=lifetable_final$cum_dcf^2
#multiplying with probability
lifetable_final$p_cum_squared=lifetable_final$dcf_squared*lifetable_final$p
#second moment :=A2
A2=sum(na.omit(lifetable_final$p_cum_squared))
var=A2-APV^2
var
#scaling 
var_fin=var*4000^2
var_fin
sqrt(var_fin)
