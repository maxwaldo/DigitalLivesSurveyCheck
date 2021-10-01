####### Organise data for codebook ########

data_bilendi <- read.csv("digital+lives+2nd+survey+-+master_September+29%2C+2021_05.06.csv")
data_bilendi <- data_bilendi[-c(1,2),]
data_bilendi <- data_bilendi[data_bilendi$m!="",]

### 0. Meta-data ###

# A start-date
# B End-date
# C Status
# D IPAdress
# E Progress
# F RecordedDate
# G RecipientEmail
# H ExternalReference
# I LocationLatitude
# J LocationLongitude
# K DistributionChannel
# L UserLanguage

## 1. Socio Demographics ##

# A year_of_birth
# B gender
data_bilendi <- data_bilendi %>% 
  mutate(gender = ifelse(gender==1, 1, 
                         ifelse(gender==8, 2, 
                                ifelse(gender==9, 3, NA))))
# C region
# D highest_ed

## 2. Emotion regation the political situation (DE and CH-DE only)

# A emotion_1
# B emotion_2
# C emotion_3
# D emotion_4


## 3. Electoral and Political participation (whole sample)

# A interest_in_pol
# B left_right_1
# C voted
data_bilendi <- data_bilendi %>% 
  mutate(voted = ifelse(voted==4, 1, 
                         ifelse(voted==5, 2, NA)))
# D party_vote
# E party_vote_text
data_bilendi <- data_bilendi %>% 
  mutate(party_vote_text = ifelse(party_vote_27_TEXT!="", party_vote_27_TEXT,
                                  ifelse(party_vote_46_TEXT!="", party_vote_46_TEXT,
                                         ifelse(party_vote_104_TEXT!="", party_vote_104_TEXT, ""))))
# F party_member
data_bilendi <- data_bilendi %>% 
  mutate(party_member = ifelse(party_member==1, 1, 
                               ifelse(party_member==3, 2, NA)))
# G which_member
# H which_member_text
data_bilendi <- data_bilendi %>% 
  mutate(which_member_text = ifelse(which_member_27_TEXT!="", which_member_27_TEXT,
                                  ifelse(which_member_46_TEXT!="", which_member_46_TEXT,
                                         ifelse(which_member_47_TEXT!="", which_member_47_TEXT, ""))))
# I close_party
data_bilendi <- data_bilendi %>% 
  mutate(close_party = ifelse(close_party==1, 1, 
                              ifelse(close_party==4, 2,
                                     ifelse(close_party==5, 3, NA))))
# J somewhat_close_party
data_bilendi <- data_bilendi %>% 
  mutate(somewhat_close_party = ifelse(somewhat_close_party==1, 1, 
                                       ifelse(somewhat_close_party==2, 2,
                                              ifelse(somewhat_close_party==4, 3, NA))))
# K which_cl_party
# L which_cl_party_text
data_bilendi <- data_bilendi %>% 
  mutate(which_cl_party_text = ifelse(which_cl_party_27_TEXT!="", which_cl_party_27_TEXT,
                                    ifelse(which_cl_party_46_TEXT!="", which_cl_party_46_TEXT,
                                           ifelse(which_cl_party_47_TEXT!="", which_cl_party_47_TEXT, ""))))
# M most_imp_topic_1-7
# N Timer political knowledge
# O nr_party_guess -> text entry
# P l_party_guess
# Q perc_w_guess
# R complexity_pol_1-3

## 4 (social) Media use ##

# A sm_use_1-4
data_bilendi <- data_bilendi %>% 
  mutate(sm_use_1 = ifelse(sm_use_1<=4, sm_use_1, 
                           ifelse(sm_use_1 == 6, 5, 
                                  ifelse(sm_use_1 == 8, 6, NA))),
         sm_use_2 = ifelse(sm_use_2<=4, sm_use_2, 
                           ifelse(sm_use_2 == 6, 5, 
                                  ifelse(sm_use_2 == 8, 6, NA))),
         sm_use_3 = ifelse(sm_use_3<=4, sm_use_3, 
                           ifelse(sm_use_3 == 6, 5, 
                                  ifelse(sm_use_3 == 8, 6, NA))),
         sm_use_4 = ifelse(sm_use_4<=4, sm_use_4, 
                           ifelse(sm_use_4 == 6, 5, 
                                  ifelse(sm_use_4 == 8, 6, NA))))

## 5 Attention check ##

# A attention_c

## 6. Local not Local

# A local_mal_out_1-4
# B local_femal_out_1-4
# C local_or_notlocal_social_network
# D local_or_notlocal_value

## 7 Transparancy module

## 8 Conjoint experiment module

# A conj_vote_1-2
# B conj_choice
# C conj_better
# D conj_why
# E c_age_pos
# F c_age_num_pol1
# G c_age_num_pol2
# E c_career_pos
# F c_career_num_pol1
# G c_career_num_pol2
# H c_gender_pos
# I c_gender_num_pol1
# J c_gender_num_pol2
# K c_legeff_pos
# L c_legeff_num_pol1
# M c_legeff_num_pol2
# N c_voteror_pos
# O c_voteror_num_pol1
# P c_voteror_num_pol2
# Q c_voterknow_pos
# R c_voterknow_num_pol1
# S c_voterknow_num_pol2
# T c_prof_pos
# U c_prof_num_pol1
# V c_prof_num_pol2
# W c_smedia_pos
# X c_smedia_num_pol1
# Y c_smedia_num_pol2

## 9. Module B. Video presentation

# A vid_cgroup_timer
# B vid_cgroup_check1
# C vid_cgroup_check1_3_TEXT
# D vid_cgroup_check2
data_bilendi <- data_bilendi %>% 
  mutate(vid_cgroup_check2 = ifelse(vid_cgroup_check2==1, 1, 
                                    ifelse(vid_cgroup_check2==4, 2, 
                                           ifelse(vid_cgroup_check2==5, 3, 
                                                  ifelse(vid_cgroup_check2==6, 4, NA)))))
# E vid_outc_comp
# F vid_outc_people
# G vid_outc_parl
### All the outcomes variables above have to be recoded
data_bilendi <- data_bilendi %>% 
  mutate(vid_outc_comp = as.numeric(substr(as.character(vid_outc_comp),  2, 2)),
         vid_outc_people = as.numeric(substr(as.character(vid_outc_comp),  2, 2)),
         vid_outc_parl = as.numeric(substr(as.character(vid_outc_comp),  2, 2)))
# H vid_outc_gov
# H vid_outc_parl_people
# I vid_1_timer
# J vid_2_timer
# K vid_3_timer
# L modul_B_treatment


## 10. Module C: Left-right positioning of candidates

# A lr_1_1
# B lr_1_2
# C lr_regierung_1
# D lr_2_1
# E lr_2_2
# F lr_LGBT_1
# G lr_exp_control
# H lr_personal_1
# I lr_personal_2
# J lr_personal_3


## Module D. Info-seeking behaviour

# A cc_temp
# B cc_importance_1
# C cc_importance_2
# D cc_importance_3
# E cc_importance_4
# F cc_importance_5
# G cc_importance_6
# H cc_importance_7
# I cc_petition
# J cc_eval
# K cc_timer_ --> Timer question
# L cc_text1_who
# M timer_cc --> Timer question
# N cc_argument
# O cc_time_choice
# P ineq_us_e
# Q ineq_importance_1
# R ineq_importance_2
# S ineq_importance_3
# T ineq_importance_4
# U ineq_importance_5
# V ineq_importance_6
# W ineq_importance_7
# X ineq_petition                   
# Y ineq_eval
# Z timer_ineq_ <- Timer question
# AA timer_ineq2_
# AB ineq_argument
# AC ineq_time_choice

## Module E 
# A trade_vote
data_bilendi <- data_bilendi %>% 
  mutate(trade_vote = as.numeric(trade_vote) - 7)

# B trade_profit
data_bilendi <- data_bilendi %>% 
  mutate(trade_profit = ifelse(trade_profit==6, 5,
                               ifelse(trade_profit==7, 6,
                                      ifelse(trade_profit==8, 7, 
                                             ifelse(trade_profit==9, 8, trade_profit)))))
# C trade_profit_9_TEXT
# D trade_pers_benefit
# E trade_pers_benefit_5_TEXT
# F trade_vote_again
# G trade_vote.1
# H trade_profit.1
data_bilendi <- data_bilendi %>% 
  mutate(trade_profit.1 = ifelse(trade_profit.1==10, 4,
                               ifelse(trade_profit.1==9, 5,
                                      ifelse(trade_profit.1==5, 6, 
                                             ifelse(trade_profit.1==6, 7, 
                                                    ifelse(trade_profit.1==7, 8, trade_profit.1))))))
# I trade_profit_7_TEXT
# J trade_p_benefit
# K trade_p_benefit_5_text
# L trade_goals.1
data_bilendi <- data_bilendi %>% 
  mutate(trade_goals.1 = as.numeric(trade_goals.1) - 3)
# M trade_goals_8_TEXT
# N trade_resp


## Sociodemographics backend

# A income
# B subj_status
# C status_mobility_ambi_1

## Module A 

# A ed_year
# B educ_field_EN
# C educ_field_NL
# D educ_field_DE
# E educ_field_FR
# F educ_field_PL
# G time_spend_module_A1
# H cur_paid_empl
# I cur_job_name
# J cur_job_content                            
# K cur_job_org
# L cur_job_startyear
# M cur_job_satisf_1
# N any_prev_empl                               
# O last_job_name
# P last_job_content
# Q last_job_org
# R last_job_starty
# S last_job_endy
# T time_spend_module_A2
# U previous_job
# V major_job_where
# W major_job_so_name                           
# X major_job_so_cont
# Y major_job_so_starty
# Z major_job_so_endy
# AA major_job_oo_name                            
# AB major_job_oo_cont
# AC major_job_oo_org                             
# AD major_job_oo_starty
# AE major_job_oo_endy
# AF time_spend_module_A3
# AG cur_act_for_party  
# AH act_party_name
# AI acti_act_party_raw
# AJ curr_act_when 
# AK prev_act_when
# AL act_ever_cons_1
# AM time_spend_module_A4
# AN ever_held_puboff
# AO ever_held_puboff_1_TEXT                      
# AP ever_ran_puboff
# AQ ever_ran_puboff_1_TEXT                       
# AR ever_cons_puboff_1
# AS interest_pipel_prof                          
# AT running_factors_1
# AU running_factors_2
# AV running_factors_3
# AW running_factors_4
# AX running_factors_5
# AY attit_towa_running                          
# AZ attit_running_again
# BA reaso_not_run_again


## End of survey

# A Comments



data_bilendi <- data_bilendi[,-c(426:458)]


load("id number Bielndi complete.rda")


data_bilendi <- data_bilendi[data_bilendi$m %in% vec_num,]


write.csv(data_bilendi, "Digital Lives Second survey_allrecoded.csv")

