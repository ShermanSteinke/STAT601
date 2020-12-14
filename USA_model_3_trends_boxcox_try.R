###################                           ######################
###################      GROUP 3 project      ######################
###################                           ######################
###################                           ######################

#Ouyang Xu, Zhenpeng Shi

rm(list = ls())
library(car)
library(Hmisc)
library(gvlma)

library(xlsx)

#data
data <- read.csv(file = 'all-states-history.csv', header = TRUE)
modata <- read.csv(file = '2020_US_Region_Mobility_Report.csv', header = TRUE)





#info: lockdown states and timelines 
info <- list(state = c("MI", "CA","IL","CT"),
             state_full_name = c("Michigan","California","Illinois","Connecticut"),
             start_date = as.Date("2020-03-20", "%Y-%m-%d"), 
             end_date = as.Date("2020-11-20", "%Y-%m-%d"))

trends_data = c('trends_Michigan.csv','trends_California.csv','trends_Illinois.csv',
                'trends_Connecticut.csv')


coef.result <- data.frame(state = info$state, 
                    #  retail_recreation = NA, 
                    #  grocery_pharmacy = NA,
                    #  parks = NA, 
                    #  #transit = NA, 
                    #  workplace = NA,
                     face_mask = NA,
                     hand_sanitizer = NA,
                     quarantine = NA,
                     testing = NA)
vif.result <- data.frame(state = info$state, 
                    #  retail_recreation = NA, 
                    #  grocery_pharmacy = NA,
                    #  parks = NA, 
                    #  #transit = NA, 
                    #  workplace = NA,
                     face_mask = NA,
                     hand_sanitizer = NA,
                     quarantine = NA,
                     testing = NA)
diagnostics.result <- data.frame(state = info$state,
                               r.squared = NA,
                               f.statistic.p.value = NA)
lambda <- c()
##########################################################

for (i in 1:length(info$state)) {
# for (i in 1:1) {  
  ##### select state
  data.state <- data[data$state == info$state[i], ]
  data.state$date <- as.Date(data.state$date, "%Y-%m-%d")
  
  modata.state <- modata[modata$sub_region_1 == info$state_full_name[i]&modata$sub_region_2=="",]
  modata.state$date <- as.Date(modata.state$date, "%Y-%m-%d")
  #trends data
  trendata = read.csv(file = trends_data[i], header = TRUE)
  
  trendata$Week = as.Date(trendata$Week, "%d/%m/%y")
  trendata = trendata[trendata$Week > info$start_date-22 & trendata$Week < info$end_date+22, ]
  trendata[,2:5] = apply(trendata[,2:5], 2, as.numeric)
  
  
  trends = data.frame(date = seq.Date(as.Date(info$start_date-15,format = "%Y/%m/%d"), by = "day", length.out = (info$end_date-info$start_date+15)),
                      face_mask = NA, Hand_sanitizer = NA, Quarantine = NA, COVID_19_testing = NA)
  #week -> date
  for (d in 1:(length(trendata$Week)-1)) {
    trends[trends$date>=trendata$Week[d]&trends$date<trendata$Week[d+1],2:5] = trendata[d,2:5]
  }
  
  
  
  
  ###### select time
  data.lock <- data.state[data.state$date > info$start_date & data.state$date < info$end_date, ]
  data.lock <- data.lock[order(data.lock$date), ] # sort the data by dates
  
  ###### select explanatory variable
  N = length(data.lock$date)
  
  
  # time lag
  explanatory = matrix(data = numeric(9*N), nrow = N, ncol = 9)
  for (j in 6:8) {
    modata.lock <- modata.state[modata.state$date > info$start_date-j & modata.state$date < info$end_date-j, 8:13]

    modata.lock <- modata.lock[order(modata.lock$date), ] # sort the data by dates
    trends.lock <- trends[trends$date > info$start_date-j & trends$date < info$end_date-j, 2:5]
    tmp = cbind(modata.lock,trends.lock)
    
    explanatory=explanatory+1/3*tmp[2:10]
    
  }
  
  #regression data
  data.l <- data.frame(new_cases = data.lock$positiveIncrease,
                 retail_recreation = explanatory[,1],
                 grocery_pharmacy = explanatory[,2],
                 parks = explanatory[,3],
                 transit = explanatory[,4],
                 workplace = explanatory[,5],
                 Term_face_mask = explanatory[,6],
                 Term_hand_sanitizer = explanatory[,7],
                 Term_Quarantine = explanatory[,8],
                 Term_testing = explanatory[,9]
                 )
  
  #regression coefficient, vif
  
  #1.with transit
  #lm.reg.l <- lm(new_cases ~ retail_recreation+grocery_pharmacy+parks+transit+workplace, data = data.l)
  
  #2.without transit
  # lm.reg.l <- lm(new_cases ~ retail_recreation+grocery_pharmacy+parks+workplace+
  #                  Term_face_mask+Term_hand_sanitizer+Term_Quarantine+Term_testing, data = data.l)

  data_selected<-data.l
  data_selected <- data_selected[data_selected$new_cases > 0, ] # remove non-positives
  data_selected
  bc <- boxCox(
        data = data_selected, lambda = seq(-5, 5, 0.02),
        data_selected$new_cases ~
            data_selected$Term_face_mask +
            data_selected$Term_hand_sanitizer +
            data_selected$Term_Quarantine +
            data_selected$Term_testing
    )
  lambda[i] <- bc$x[which.max(bc$y)]
    names(lambda)[i] <- info$state[i]
    data_selected$new_cases_bc <- (data_selected$new_cases^lambda[i] - 1) / lambda[i]
  #3.only trends
  lm.reg.l <- lm(new_cases_bc ~ Term_face_mask+Term_hand_sanitizer+Term_Quarantine+Term_testing, data = data_selected)
########################################################################################
############################### Diagnostics ############################################
  
  #                          VIF and Pearson
  #                      (to test collinearity)
  
  vif.l <- vif(lm.reg.l)
  #print(info$state[i])
  #cor.l <- rcorr(as.matrix(data.l[,c(2,3,4,6)]), type = "pearson")
  #print(cor.l)
  
  #                      Components+Residual plot
  #                         (to test linearity)
  
  #crPlots(lm.reg.l)
  
  #                   Residual QQ-plot Cook's_distance
  #                        (to test Normality)
  #par(mfrow=c(2,2))
  #plot(lm.reg.l,which=c(1:4))
  
  #                    (to test Heteroscedasticity) 
  #gvmodel <- gvlma(lm.reg.l)
  #print(summary(gvmodel))
  
  #                             DurbinWaston-Test  
  #                      (to test independence)
  
  #print(durbinWatsonTest(lm.reg.l))
  
  #                         r-squared and p-value
  #                       (to test prediction accuracy)
  diagnostics.result[i,2] = summary(lm.reg.l)$r.squared
  f <- summary(lm.reg.l)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  diagnostics.result[i,3] = p  
  
  
  #result
  #print(summary(lm.reg.l))
  # for (k in 2:9) {  
  for (k in 2:5) {
    coef.result[i,k] = lm.reg.l$coefficients[k]
    vif.result[i,k] = vif.l[k-1]
  }
  


}
# print('coef.result')
# print(coef.result)
# # print('vif.result')
# # print(vif.result)
# print(diagnostics.result)

# write.csv(x = coef.result, file = "trendsonly_coefficient.csv")

# write.xlsx(x = coef.result, file = "trendsonly_coefficient.xlsx")
# write.xlsx(x = c(vif.result,diagnostics.result$r.squared), file = "trendsonly_vif.xlsx")


                    # (to test Heteroscedasticity) 
gvmodel <- gvlma(lm.reg.l)
print(summary(gvmodel))


write.xlsx(x = coef.result, file = "03-20-11-20trendsUSA_coefficient_without0.xlsx")
