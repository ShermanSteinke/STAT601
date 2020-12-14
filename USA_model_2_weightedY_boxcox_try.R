###################                           ######################
###################      GROUP 3 project      ######################
###################                           ######################
###################                           ######################

# Zhenpeng Shi, Shubo Lin, Ouyang Xu, Mengqi Xu

rm(list = ls())
#library(dplyr)
library(stringr)
library(car)
library(tidyverse)
library(Hmisc)
library(gvlma)

#library(xlsx)
# set dates
startdata=c("2020-04-30")
enddata=c("2020-09-20")
start <- as.Date(startdata)
end <- as.Date(enddata)


# read data
case <- read.csv(file = "all-states-history.csv", header =  TRUE)
mobility <- read.csv(file = "2020_US_Region_Mobility_Report.csv", header = TRUE)
codes <- read.csv(file = "states.csv", header = TRUE,encoding="UTF-8")
colnames(codes) <- c("state", "abbrev", "code")

# modify date col
case$Date <- as.Date(case$date)
mobility$date <- as.Date(mobility$date)

state <- unique(mobility$sub_region_1)[-1]

result <- data.frame(state = state)


lambda <- c()
model <- list()
regression <- list()
vif <- data.frame(state = state)
corr <- list()
# layout(matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE))
S.W_test <- c()
equal_variance <- c()
r_f <- data.frame(state = state, r.squared = NA, f.statistic.p.value = NA)

# regression by states
for (i in 1:length(state)) {
# for (i in 1:1) { 
    
    mobility_by_state <- mobility[mobility$sub_region_1 == state[i] &
        mobility$sub_region_2 == "" &
        mobility$date >= start &
        mobility$date <= end, ]
    mobility_by_date <- mobility_by_state[order(mobility_by_state$date), ]

    case_by_state <- case[case$state == codes$code[codes$state == state[i]] &
        case$date >= (start + 7) & case$date <= (end + 14), ]
    case_by_date <- case_by_state[order(case_by_state$date), ]
    for (j in 1:(length(case_by_date$date) - 7)) {
        case_by_date$positiveIncrease[j] <- mean(case_by_date$positiveIncrease[j:(j + 6)])
    }
    case_by_date <- case_by_date[case_by_date$date >= (start + 7) &
        case_by_date$date <= (end + 7), ]
    
    #death 
    deathdata <- case[case$state == codes$code[codes$state == state[i]] &
                          case$date >= (start + 14) & case$date <= (end + 28), ]
    
    
    
    for (k in 1:(length(deathdata)-14)){
        deathdata$deathIncrease[k]  = mean(deathdata$deathIncrease[k:(k+13)])
    }
    
    deathdata = deathdata[deathdata$date >= (start + 14) & deathdata$date <= (end + 14),]
    
    
    #weighted response variable: 0.5*average_new_cases + 0.5*average_death/death_rate
    #(average_death is an average of death after 14-28 days)
    #(death rate is 1.9% according to Johns-hopkins' research)
    weighted_y = 0.5*case_by_date$positiveIncrease + 0.5*deathdata$deathIncrease/0.019
        
    data_selected <- data.frame(
        positiveIncrease = weighted_y,
        retail_and_recreation = mobility_by_date$retail_and_recreation_percent_change_from_baseline,
        grocery_and_pharmacy = mobility_by_date$grocery_and_pharmacy_percent_change_from_baseline,
        parks = mobility_by_date$parks_percent_change_from_baseline,
        # transit_stations = mobility_by_date$transit_stations_percent_change_from_baseline,
        workplaces = mobility_by_date$workplaces_percent_change_from_baseline
        # residential = mobility_by_date$residential_percent_change_from_baseline
    )
   
    # data_selected$positiveIncrease[data_selected$positiveIncrease <= 0] <- 1e-20
    data_selected <- data_selected[data_selected$positiveIncrease > 0, ] # remove non-positives
    bc <- boxCox(
        data = data_selected, lambda = seq(-5, 5, 0.02),
        data_selected$positiveIncrease ~
            data_selected$retail_and_recreation +
            data_selected$grocery_and_pharmacy +
            data_selected$parks +
            data_selected$workplaces
    )
    lambda[i] <- bc$x[which.max(bc$y)]
    names(lambda)[i] <- state[i]
    data_selected$positiveIncrease_bc <- (data_selected$positiveIncrease^lambda[i] - 1) / lambda[i]


    model[[state[i]]] <- lm(
        data = data_selected,
        positiveIncrease_bc ~ retail_and_recreation + grocery_and_pharmacy + parks + workplaces
    )
    regression[[state[i]]] <- summary(model[[state[i]]])


    result[result$state == state[i], 2:length(model[[state[i]]]$coefficients)] <-
        model[[state[i]]]$coefficients[2:length(model[[state[i]]]$coefficients)]

    colnames(result)[2:length(model[[state[i]]]$coefficients)] <-
        names(model[[state[i]]]$coefficients)[2:length(model[[state[i]]]$coefficients)]

    vif[vif$state == state[i], 2:length(model[[state[i]]]$coefficients)] <-
        vif(model[[state[i]]])

    colnames(vif)[2:length(model[[state[i]]]$coefficients)] <-
        names(model[[state[i]]]$coefficients)[2:length(model[[state[i]]]$coefficients)]

    ############################ Diagnostics #######################################
    # crPlots(model[[state[[i]]]])
    # Pearson's r
    corr[[state[i]]] <- rcorr(as.matrix(data_selected), type = "pearson")[[1]]

    # QQplot
    # plot(model[[state[i]]], which = 2)

    # Shapiro–Wilk normality test
    S.W_test[i] <- shapiro.test(model[[state[[i]]]]$residuals)$p.value
    names(S.W_test)[i] <- state[i]

    # equal variance
    if (gvlma(model[[state[i]]])[length(gvlma(model[[state[i]]]))]$GlobalTest$DirectionalStat4$Decision == 0) {
        equal_variance[i] <- FALSE
    } else {
        equal_variance[i] <- TRUE
    }
    names(equal_variance)[i] <- state[i]
    # r-squared and p-value
    r_f[r_f$state == state[[i]], 2] <- regression[[state[i]]]$r.squared
    f <- regression[[state[i]]]$fstatistic
    r_f[r_f$state == state[[i]], 3] <- pf(f[1], f[2], f[3], lower.tail = FALSE)
}
############################# Output ###########################################
# cat(sep = "", "lambda:\n")
# print(lambda)
# cat("\n")
# write.csv(x = cbind(result, equal_variance), file = "USA_regression.csv")
# # write.csv(x = vif, file = "USA_vif.csv")

# cat(sep = "", "result:\n")
print(result)
# cat("\n")

# cat(sep = "", "vif:\n")
# print(vif)
# cat("\n")

# # cat(sep = "", "corr:\n")
# # print(corr)
# # cat("\n")

# cat(sep = "", "S.W_test:\n")
# print(S.W_test)
# cat("\n")

# cat(sep = "", "Heteroscedasticity: TRUE indicates Heteroscedasticity exists\n")
# print(equal_variance)
# cat("\n")

# cat(sep = "", "r-squared and p-value:\n")
# print(r_f)
# cat("\n")


# ############################ File Write###########################################
#write.xlsx(x = cbind(result,equal_variance), file = "04-30-9-20USA_coeffcient_without0.xlsx")
# write.csv(x = vif, file = "04-30-11-20USA_vif.csv")
# write.csv(x = equal_variance, file = "04-30-11-20USA_Heteroscedasticity.csv")

# write.csv(x = corr, file = "04-30-11-20USA_pearson.csv")
