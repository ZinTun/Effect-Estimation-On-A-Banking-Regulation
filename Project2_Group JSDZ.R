library(readr)
dat <- read_csv("C:/Users/SONG/Desktop/DiD_data2.csv")

data_clean$w == data_clean$affect

#clean data without na for all fields
data_clean <- dat[complete.cases(dat),]

data_clean_full <- data_clean
data_clean <- subset(data_clean,rssd9999>20070331)

summary(data_clean)


#clean data without na for avgtradingratio column
data_clean_avgtradingratio <- dat[!is.na(dat$bhc_avgtradingratio),]

data_clean_avgtradingratio <- subset(data_clean_avgtradingratio,rssd9999>20070331)

#for later placebo test benchmarking
model_0 <-plm(bhc_avgtradingratio~after_DFA_1:treat_3_b_avg + dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_clean,index=c("rssd9001","rssd9999"),model="within")
summary(model_0)


##########################################################
################### Baseline tests #######################
##########################################################

# model 1: OLS, no control, no fixed effect
model_DFA <- lm(bhc_avgtradingratio~after_DFA_1,data=data_clean_avgtradingratio)
summary(model_DFA)

# model 2: OLS, with control,  no fixed effect
model_controls_noFE <- lm(bhc_avgtradingratio~after_DFA_1+ dep_roa1+dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_clean)
summary(model_controls_noFE)

# model 3: OLS, interaction of after DFA * trading ratio, with control, no fixed effect
model_Affect_DFA <- lm(bhc_avgtradingratio~after_DFA_1*treat_3_b_avg+dep_roa1+dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_clean)
summary(model_Affect_DFA)

#model 4: PLM, after DFA:trading ratio, with control & fixed effect
library(plm)
model_Affect_DFA_plm <- plm(bhc_avgtradingratio~ after_DFA_1:treat_3_b_avg +dep_roa1 + dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_clean,index=c("rssd9001","rssd9999"),model="within")
summary(model_Affect_DFA_plm)


########################################################################
#################   Robustness tests  ##################################
########################################################################

#Robustness test model 1 : Pre-2007 effect
model_pre_2007 <- plm(bhc_avgtradingratio~ after_DFA_1:Pre_2007_Affect+ dep_roa1 + dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_clean_full,index=c("rssd9001","rssd9999"),model="within")
summary(model_pre_2007)


#Robustness test model 2 : Prosensity score matching
library("MatchIt")
data_20070331<-subset(data_clean,rssd9999==20070930)
summary(data_20070331)

model_match<-matchit(treat_3_b_avg~ dep_roa1+dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter,data=data_20070331, method ="nearest", ratio=3,replace=TRUE)
summary(model_match)
data_match<-match.data(model_match)

## matched data model
selectedRows <- (data_clean$rssd9001 %in% data_match$rssd9001)
data_withMatchBanks <- data_clean[selectedRows,]

model_w2.2 <- plm(bhc_avgtradingratio~after_DFA_1:treat_3_b_avg + dep_roa1+ dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_withMatchBanks,index=c("rssd9001","rssd9999"),model="within")
summary(model_w2.2)


#Robustness test model 3: Excluding non-trading BHCs
data_withMatchBanks_new = subset(data_withMatchBanks,bhc_avgtradingratio>0)

model_w2.3 <- plm(bhc_avgtradingratio~after_DFA_1:treat_3_b_avg+ dep_roa1 + dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_withMatchBanks_new,index=c("rssd9001","rssd9999"),model="within")
summary(model_w2.3)


#######################################################################
##################### Placebo Test ####################################
#######################################################################


#placebo test 1
data_DFA <- data_clean
data_DFA <- data_DFA[data_DFA$bhc_avgtradingratio > 0,]


set.seed(0230036)

bank <- unique(data_DFA$rssd9001)
bank

RUN <- 100
model_coeff <- vector()
model_p <- vector()

#placing treatment time from quarter22 to quarter30
for(j in c(22:30)){
  
  data_DFA$after_DFA_1 = ifelse(data_DFA$quarterID<j,0,1)
  
  
  for(i in 1:RUN){
    random <- sample(bank, 100)
    
    random1 <-split(random, sample(1:2, 100,replace = TRUE))
    
    selectedRows <- (data_DFA$rssd9001 %in% random1[[1]])
    summary(selectedRows)
    data_treated <- data_DFA[selectedRows,]
    data_treated$treat_3_b_avg <- 1
    #View(data_treated)
    
    selectedRows <- (data_DFA$rssd9001 %in% random1[[2]])
    data_control <- data_DFA[selectedRows,]
    data_control$treat_3_b_avg <- 0
    #View(data_control)
    
    data_placebo <- rbind(data_treated,data_control)
    #View(data_placebo)
    #model
    model_placebo <- plm(bhc_avgtradingratio~after_DFA_1:treat_3_b_avg + dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir, data = data_placebo,index=c("rssd9001","rssd9999"),model="within")
    model_coeff <- append(model_coeff,model_placebo$coefficients["after_DFA_1:treat_3_b_avg"])
    #model_p <- append(model_p,summary(model_placebo)$coefficients["after_DFA_1:treat_3_b_avg",4])
  }
  
}



plot()
hist(model_coeff, xlim=c(-0.07,0.07))

#green line: w continous
#abline(v=model_Affect_DFA_plm$coefficients["after_DFA_1:new_w"],untf = FALSE,col=3)

#red line: w=0/1
abline(v=model_0$coefficients["after_DFA_1:treat_3_b_avg"],untf = FALSE,col=2)


#######################################################################
############# Continuous Covariates ###################################
#######################################################################

#Model 1: Continuous Effect (w)
model_1 <- plm(bhc_avgtradingratio~after_DFA_1:new_w  + dep_roa1+ dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_withMatchBanks,index=c("rssd9001","rssd9999"),model="within")
summary(model_1)


#Model 2: Continuous time (t)
model_t2 <- plm(bhc_avgtradingratio~t*treat_3_b_avg  + dep_roa1+ dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_withMatchBanks,index=c("rssd9001","rssd9999"),model="within")
summary(model_t2)


#Model 3: Continuous effect and time (w&t)
model_wt2 <- plm(bhc_avgtradingratio~t*new_w  + dep_roa1+ dep_lnassets+dep_leverage+dep_liquidity+dep_depositratio+dep_creditrisk_total3+dep_loans_REratio+dep_cir+dep_cpp_bankquarter, data = data_withMatchBanks,index=c("rssd9001","rssd9999"),model="within")
summary(model_wt2)
