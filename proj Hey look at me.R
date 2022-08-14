# Access data from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/IR4SBY
# Download file called: fundraising_results_coded

amir<- fundraising_results_coded
summary(amir$giftamount>0) 


amir$giveless1totalall <- amir$total_cash>0 & amir$total_cash<100
amir$ltotal_cash <- log(amir$total_cash+1)
amir$ltotal_nonanoncash <- log(amir$total_nonanoncash+1)


amir$nonmissaveragegift <- amir$averagegift
amir$nonmissaveragegift[is.na(amir$nonmissaveragegift)] <- 0


amir$laveragegift <-log(amir$averagegift+1)
amir$laveragegift[is.na(amir$laveragegift)] <- 0 


amir$large_lastgift <- pmax(amir$lastgift1,amir$lastgift2,amir$lastgift3,amir$lastgift4,amir$lastgift5,amir$lastgift6,amir$lastgift7,amir$lastgift8,amir$lastgift9,na.rm = TRUE) 
amir$missinglast <- ifelse(is.na (amir$large_lastgift) ,1,0)


amir$largest_100to500 = amir$large_lastgift<500 & !is.na(amir$large_lastgift) & amir$large_lastgift>100
amir$largest_0to100 = amir$large_lastgift<100 & !is.na(amir$large_lastgift) & amir$large_lastgift>0
amir$missing_anydemos  <- ifelse(is.na(amir$age)|is.na( amir$married) 
                                 | amir$cnbio_gender=="" ,1,0)




amir$married [amir$cnbio_marital_status==""]<-NA
amir$male[amir$cnbio_gender==""]<-NA

amir$any_treatment<-amir$newsletter
amir$control<-amir$treatment_1
amir$treat_100<-amir$treatment_2
amir$treat_500<-amir$treatment_3
amir$treat_100_500<-amir$treatment_4

#vectors - 
mean_married <-c(mean(amir$married[amir$treatment==1],na.rm = TRUE),
                 mean(amir$married[amir$treatment==2],na.rm = TRUE)
                 ,mean(amir$married[amir$treatment==3],na.rm = TRUE)
                 ,mean(amir$married[amir$treatment==4],na.rm = TRUE))

sd_married <-c(sd(amir$married[amir$treatment==1],na.rm = TRUE),
                    sd(amir$married[amir$treatment==2],na.rm = TRUE),
                    sd(amir$married[amir$treatment==3],na.rm = TRUE),
                    sd(amir$married[amir$treatment==4],na.rm = TRUE))


mean_male <- c(mean(amir$male[amir$treatment==1],na.rm = TRUE),
               mean(amir$male[amir$treatment==2],na.rm = TRUE),
               mean(amir$male[amir$treatment==3],na.rm = TRUE),
               mean(amir$male[amir$treatment==4],na.rm = TRUE))

sd_male <- c(sd(amir$male[amir$treatment==1],na.rm = TRUE),
             sd(amir$male[amir$treatment==2],na.rm = TRUE),
             sd(amir$male[amir$treatment==3],na.rm = TRUE),
             sd(amir$male[amir$treatment==4],na.rm = TRUE))


mean_age <- c(mean(amir$age[amir$treatment==1],na.rm=TRUE),
              mean(amir$age[amir$treatment==2],na.rm=TRUE),
              mean(amir$age[amir$treatment==3],na.rm=TRUE),
              mean(amir$age[amir$treatment==4],na.rm=TRUE))


sd_age <- c(sd(amir$age[amir$treatment==1],na.rm=TRUE),
            sd(amir$age[amir$treatment==2],na.rm=TRUE),
            sd(amir$age[amir$treatment==3],na.rm=TRUE),
            sd(amir$age[amir$treatment==4],na.rm=TRUE))


mean_averagegift <- c(mean(amir$averagegift[amir$treatment==1],na.rm = TRUE),
                      mean(amir$averagegift[amir$treatment==2],na.rm = TRUE),
                      mean(amir$averagegift[amir$treatment==3],na.rm = TRUE),
                      mean(amir$averagegift[amir$treatment==4],na.rm = TRUE))


sd_avergegift <- c(sd(amir$averagegift[amir$treatment==1],na.rm = TRUE),
                   sd(amir$averagegift[amir$treatment==2],na.rm = TRUE),
                   sd(amir$averagegift[amir$treatment==3],na.rm = TRUE),
                   sd(amir$averagegift[amir$treatment==4],na.rm = TRUE))

mean_lragest_to_100 <- c(mean(amir$largest_0to100[amir$treatment==1]),
                         mean(amir$largest_0to100[amir$treatment==2]),
                         mean(amir$largest_0to100[amir$treatment==3]),
                         mean(amir$largest_0to100[amir$treatment==4]))

sd_lragest_to_100 <- c(sd(amir$largest_0to100[amir$treatment==1]),
                       sd(amir$largest_0to100[amir$treatment==2]),
                       sd(amir$largest_0to100[amir$treatment==3]),
                       sd(amir$largest_0to100[amir$treatment==4]))


mean_Largest_lastgift_100_to_500 <- c(mean(amir$largest_100to500[amir$treatment==1]),
                                      mean(amir$largest_100to500[amir$treatment==2]),
                                      mean(amir$largest_100to500[amir$treatment==3]),
                                      mean(amir$largest_100to500[amir$treatment==4]))

sd_Largest_lastgift_100_to_500 <- c(sd(amir$largest_100to500[amir$treatment==1]),
                                    sd(amir$largest_100to500[amir$treatment==2]),
                                    sd(amir$largest_100to500[amir$treatment==3]),
                                    sd(amir$largest_100to500[amir$treatment==4]))


mean_missing_any_demo <- c(mean(amir$missing_anydemos[amir$treatment==1],na.rm = TRUE),
                           mean(amir$missing_anydemos[amir$treatment==2],na.rm = TRUE),
                           mean(amir$missing_anydemos[amir$treatment==3],na.rm = TRUE),
                           mean(amir$missing_anydemos[amir$treatment==4],na.rm = TRUE))


sd_missing_any_demo <- c(sd(amir$missing_anydemos[amir$treatment==1],na.rm = TRUE),
                         sd(amir$missing_anydemos[amir$treatment==2],na.rm = TRUE),
                         sd(amir$missing_anydemos[amir$treatment==3],na.rm = TRUE),
                         sd(amir$missing_anydemos[amir$treatment==4],na.rm = TRUE))



sum_table <- data.frame(mean_age,sd_age,mean_male,sd_male,mean_married,sd_married
                        ,mean_averagegift,sd_avergegift,mean_lragest_to_100  ,sd_lragest_to_100  ,mean_Largest_lastgift_100_to_500
                        ,sd_Largest_lastgift_100_to_500,mean_missing_any_demo,sd_missing_any_demo)


rownames(sum_table) <- c("control","100_circle","500_circle","100_circle_and_500_circle")



data <- Experiments_March12


data$Decision1 <- 5-data$Decision1
data$Decision2 <- 5-data$Decision2
data$Decision3 <- 5-data$Decision3



data$Converse[data$Converse=="missing"]<-NA
data$friends <- ifelse(data$Converse >0 ,1,0)


#vectors for table 2

gift_amount_round1_mean <- c(mean(data$Decision1[data$Image==1],na.rm =TRUE),
                             mean(data$Decision1[data$Image==0],na.rm =TRUE))

gift_amount_round1_sd <- c(sd(data$Decision1[data$Image==1],na.rm =TRUE),
                           sd(data$Decision1[data$Image==0],na.rm =TRUE))

gift_amount_round2_mean <- c(mean(data$Decision2[data$Image==1],na.rm =TRUE),
                             mean(data$Decision2[data$Image==0],na.rm =TRUE))

gift_amount_round2_sd <- c(sd(data$Decision2[data$Image==1],na.rm =TRUE),
                           sd(data$Decision2[data$Image==0],na.rm =TRUE))


gift_amount_round3_mean <- c(mean(data$Decision3[data$Image==1],na.rm =TRUE),
                             mean(data$Decision3[data$Image==0],na.rm =TRUE))

gift_amount_round3_sd <- c(sd(data$Decision3[data$Image==0],na.rm =TRUE),
                           sd(data$Decision3[data$Image==0],na.rm =TRUE))


friend_in_room_mean <- c(mean(data$friends[data$Image==1],na.rm = TRUE),
                         mean(data$friends[data$Image==0],na.rm = TRUE))

friends_in_room_sd   <-     c(sd(data$friends[data$Image==1],na.rm = TRUE),
                              sd(data$friends[data$Image==0],na.rm = TRUE))


number_of_sesseion <- c(nrow(as.data.frame(table(data$Session[data$Image==1]))),
                        nrow(as.data.frame(table(data$Session[data$Image==0]))))

#different answer in row 2 
avg_of_session<- c(mean(data$N[data$Image==1], na.rm = TRUE),
                   mean(data$N[data$Image==0], na.rm = TRUE))

sum_table2 <- data.frame(gift_amount_round1_mean,gift_amount_round1_sd,gift_amount_round2_mean,
                         gift_amount_round2_sd,
                         gift_amount_round3_mean
                         ,gift_amount_round3_sd,friend_in_room_mean,
                         friends_in_room_sd,avg_of_session,number_of_sesseion)


rownames(sum_table2)<- c("image","Influence")





#regression 1+2

rg1 <- lm(amir$gavetotalall ~ amir$any_treatment)

rg6 <- lm(amir$gavetotalall ~ amir$treat_100+amir$treat_500+amir$treat_100_500)
lm(amir$gavetotalall ~ amir$treat_100)
rg1

differences <- aov(amir$gavetotalall~ amir$treat_100+amir$treat_500+amir$treat_100_500)
summary(differences)

rg2 <- lm(amir$giveless1totalall ~ amir$any_treatment)

rg7 <- lm (amir$giveless1totalall ~ amir$treat_100+amir$treat_500+amir$treat_100_500)

rg4 <- lm (amir$give5totalall ~ amir$any_treatment)

rg9 <- lm (amir$give5totalall ~ amir$treat_100+amir$treat_500+amir$treat_100_500)

rg3 <- lm (amir$give1totalall ~ amir$any_treatment)

rg8 <- lm (amir$give1totalall ~ amir$treat_100+amir$treat_500+amir$treat_100_500)

rg5 <- lm(amir$ltotal_cash ~ amir$any_treatment)

rg10 <- lm(amir$ltotal_cash~ amir$treat_100+amir$treat_100_500+amir$treat_500)
 
#sd different because of the clustering model


#regression 3

rg11 <- lm(amir$gavetotalall ~ 
            amir$treat_100*amir$laveragegift + 
            amir$treat_100_500*amir$laveragegift+
           amir$treat_500*amir$laveragegift+amir$missinglast)


rg12 <- lm(amir$giveless1totalall ~ 
            amir$treat_100*amir$laveragegift + 
            amir$treat_100_500*amir$laveragegift+
            amir$treat_500*amir$laveragegift+amir$missinglast)


rg13 <- lm(amir$give1totalall ~ 
                amir$treat_100*amir$laveragegift + 
                amir$treat_100_500*amir$laveragegift+
                amir$treat_500*amir$laveragegift+amir$missinglast)

rg14  <- lm(amir$give5totalall ~ 
                    amir$treat_100*amir$laveragegift + 
                    amir$treat_100_500*amir$laveragegift+
                    amir$treat_500*amir$laveragegift+amir$missinglast)

rg15 <-lm(amir$ltotal_cash ~ 
                    amir$treat_100*amir$laveragegift + 
                    amir$treat_100_500*amir$laveragegift+
                    amir$treat_500*amir$laveragegift+amir$missinglast)

#regression 4

rg16 <- lm(amir$gavetotalall ~ amir$laveragegift*amir$any_treatment+amir$missinglast)

rg17 <- lm(amir$giveless1totalall~amir$laveragegift*amir$any_treatment+amir$missinglast)

rg19 <- lm(amir$give5totalall ~amir$laveragegift*amir$any_treatment+amir$missinglast)

rg18 <- lm(amir$give1totalall ~amir$laveragegift*amir$any_treatment+amir$missinglast)

rg20 <- lm(amir$ltotal_cash ~amir$laveragegift*amir$any_treatment+amir$missinglast)

#sd different because of the clustering model



##Use stargazer package to format results into publishable quality

install.packages("stargazer")
library(stargazer)
stargazer(sum_table,type = "text",summary = FALSE,   title = "Summary statistics:",flip = TRUE)
stargazer(sum_table2,type = "text",summary = FALSE,   title = "Summary statistics:",flip = TRUE,rownames = TRUE)



stargazer(rg1,rg2,rg3,rg4,rg5,type = "text",title = "regression 1",
          covariate.labels = c("any treatment" ),
          dep.var.labels   = c("gave>0", "gave>100","gave <500","gave>500","log 1+gift"),digits = 3)


stargazer(rg6,rg7,rg8,rg9,rg10,type = "text",title = "regression 2", covariate.labels = c("
$100 giving circle ","500$ giving circle","100 and 500"),
          dep.var.labels   = c("gave>0", "gave>100","gave <500","gave>500","log 1+gift"),digits = 3)

stargazer(rg11,rg12,rg13,rg14,rg15,type = "text",title = "regression 3",
covariate.labels = c("announced$100","log prior gift","both $100
and $500*log","announced 500$","missing prior gift","100*average prior","both 100 and 500","500*average prior"),
dep.var.labels   = c("gave>0", "gave>100","gave <500","gave>500","log 1+gift"),digits = 3)

stargazer(rg16,rg17,rg18,rg19,rg20,type = "text",title = "regression 4",
covariate.labels = c("log prior gift","any treatment","Missing prior gift","Any treatment?average prior"),
          dep.var.labels   = c("gave>0", "gave>100","gave <500","gave>500","log 1+gift"),digits = 3)

label(table1) <- data.frame(mean_age,sd_age,mean_male,sd_male,mean_married,sd_married
                            ,mean_averagegift,sd_avergegift,mean_lragest_to_100  ,sd_lragest_to_100  ,mean_largest_to_500
                            ,sd_largest_to_500,mean_missing,sd_missing)





rownames(sum_table) <- c("control","100 circle","500 circle","100 circle and 500 circle")








