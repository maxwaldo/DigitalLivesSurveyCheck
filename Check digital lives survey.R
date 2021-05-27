#################################################################################################
############################# Recode Digital Live Second Survey #################################
#################################################################################################

library(dplyr)

getSurvey <- function() {
  
  ### Load the data
  data <- read.csv("digital lives 2nd survey - master_May 26, 2021_08.05.csv")
  data <- data[-c(1,2),]
  
  ### keep respondents that answer in the test
  data <- data[as.Date(data$StartDate)>="2021-05-25",]
  
  return(data)

}

data <- getSurvey()

## Check the sociodemographics frontend block

table(data$year_of_birth) 


table(data$gender)


table(data$region)
table(data$region, data$Q_Language)


table(as.numeric(data$highest_ed))
table(as.numeric(data$highest_ed), data$Q_Language)


## Check the german politische lage

table(data$emotions_1)
table(data$emotions_2)
table(data$emotions_3)
table(data$emotions_4)

table(data$emotions_1, data$Q_Language) ## checks with language
table(data$emotions_2, data$Q_Language)
table(data$emotions_3, data$Q_Language)
table(data$emotions_4, data$Q_Language)

## Check electoral knowledge and participation

table(data$interest_in_pol)

table(data$left_right_1)

table(data$voted)
table(data$party_vote)
table(data$voted, data$party_vote)

table(data$party_member)
table(data$which_member)
table(data$party_member, data$which_member)

table(data$close_party)
table(data$somewhat_close_party)
table(data$which_cl_party)

table(data$most_imp_topics_1)
table(data$most_imp_topics_2)
table(data$most_imp_topics_3)
table(data$most_imp_topics_4)
table(data$most_imp_topics_5)
table(data$most_imp_topics_6)
table(data$most_imp_topics_7)


table(data$nr_parties_guess)
table(data$l_party_guess)
table(data$perc_w_guess)


table(data$complexity_pol_1)
table(data$complexity_pol_2)
table(data$complexity_pol_3)


## Check use of social media

table(data$sm_use_1)
table(data$sm_use_2)
table(data$sm_use_3)
table(data$sm_use_4)

## Check the local VS not local randomization

table(data$local_or_notlocal_text) 
# The randomization has worked but I would add an embedeed variable (like loca_or_not_local_value)
# to have a clear idea what treatment respondents were assigned to. 

## Check transparancy randomization

data_transp <- as.data.frame(data[,75:126]!="")


for (i in 1:ncol(data_transp)) {
  print(table(data_transp[,i])) ## Seems to have worked out 
}


## Check conjoint experiment

data_conj <- data[,c(127:131, 256:319)]

for (i in c(3+(3*1:8))) {
  print(table(data_conj[i])) ### Checnks the name attribute of each attribute
}

## Check position and values for each attribute
# age
table(data_conj$c_age_pos)
table(data_conj$c_age_num_pol1)
table(data_conj$c_age_val_pol1)
table(data_conj$c_age_num_pol2)
table(data_conj$c_age_val_pol2)

# career
table(data_conj$c_career_pos)
table(data_conj$c_career_num_pol1)
table(data_conj$c_career_val_pol1)
table(data_conj$c_career_num_pol2)
table(data_conj$c_career_val_pol2)

# gender
table(data_conj$c_gender_pos)
table(data_conj$c_gender_num_pol1)
table(data_conj$c_gender_val_pol1)
table(data_conj$c_gender_num_pol2)
table(data_conj$c_gender_val_pol2)

# legeff
table(data_conj$c_legeff_pos)
table(data_conj$c_legeff_num_pol1)
table(data_conj$c_legeff_val_pol1)
table(data_conj$c_legeff_num_pol2)
table(data_conj$c_legeff_val_pol2)

# voteror
table(data_conj$c_voteror_pos)
table(data_conj$c_voteror_num_pol1)
table(data_conj$c_voteror_val_pol1)
table(data_conj$c_voteror_num_pol2)
table(data_conj$c_voteror_val_pol2)

# voterknow
table(data_conj$c_voterknow_pos)
table(data_conj$c_voterknow_num_pol1)
table(data_conj$c_voterknow_val_pol1)
table(data_conj$c_voterknow_num_pol2)
table(data_conj$c_voterknow_val_pol2)

# prof
table(data_conj$c_prof_pos)
table(data_conj$c_prof_num_pol1)
table(data_conj$c_prof_val_pol1)
table(data_conj$c_prof_num_pol2)
table(data_conj$c_prof_val_pol2)

# smedia
table(data_conj$c_smedia_pos)
table(data_conj$c_smedia_num_pol1)
table(data_conj$c_smedia_val_pol1)
table(data_conj$c_smedia_num_pol2)
table(data_conj$c_smedia_val_pol2)


### Check video treatment

table(data$vid_cgroup_check1)
table(data$vid_cgroup_check2)

# Issue to identify which respondents have which treatment

# Solution: Play with the timer.

data <- data %>% 
  mutate(vid1 = ifelse(vid_1_timer_Click.Count=="", 0, 1),
         vid2 = ifelse(vid_2_timer_Click.Count=="", 0, 1),
         vid3 = ifelse(vid_3_timer_Click.Count=="", 0, 1),
         vidcgroup = ifelse(vid_cgroup_timer_Click.Count=="", 0, 1))


table(data$vid1)
table(data$vid2)
table(data$vid3)
table(data$vidcgroup)


## check randomization in Module D: Inequality and environment

table(data$cc_timer_Click.Count)
table(data$timer_cc_2_Click.Count)
table(data$timer_ineq_Click.Count)
table(data$timer_ineq2_Click.Count)

# Issues with the control group. Have to deduce who was in it.

data <- data %>% 
  mutate(cc_group = ifelse(cc_temp=="", 0, 1),
         cc_experts_group = ifelse(cc_timer_Click.Count=="", 0, 1),
         cc_voters_group = ifelse(timer_cc_2_Click.Count=="", 0, 1),
         cc_control_group = ifelse(cc_group==1 & cc_experts_group==0 & cc_voters_group==0, 1, 0),
         ineq_group = ifelse(ineq_us_e=="", 0, 1),
         ineq_experts_group = ifelse(timer_ineq_Click.Count=="", 0, 1),
         ineq_voters_group = ifelse(timer_ineq2_Click.Count=="", 0, 1),
         ineq_control_group = ifelse(ineq_group==1 & ineq_experts_group==0 & ineq_voters_group==0, 1, 0))


table(data$cc_experts_group, data$cc_voters_group, data$cc_control_group)
table(data$ineq_experts_group, data$ineq_voters_group, data$ineq_control_group)
# approximatively 20 respondents per group.

## Issue with the randomization of modul E. Not possible to see which treatment respondents were assigned. 

## See all the variables. 

data <- getSurvey()
names_col <- colnames(data)

for (i in 18:221){
  print(names_col[i])
  print(table(data[,i]))
}




