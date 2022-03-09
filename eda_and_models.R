library('data.table')
library('ggplot2')
library('reshape')
library('dplyr')
library(GGally)
# library(tmap)
# library(tmaptools)
library(OpenStreetMap)
# library(sf)
# library(osmdata)
# library(osmextract)
library(caret)

setwd('~/PycharmProjects/covid_vaccinare/')

data <- fread('vacc_vote_age_ed_rel_data.csv')

vacc_centers <- fread('centre_covid.csv')


#########################
## Feature engineering ##
#########################

data$nuts = 'other'
data[judet %in% c('bihor','cluj','bistrita-nasaud','maramures','satu mare','salaj'), nuts := 'NV']
data[judet %in% c('alba','brasov','covasna','harghita','mures','sibiu'), nuts := 'centru']
data[judet %in% c('bacau','botosani','iasi','neamt','suceava','vaslui'), nuts := 'NE']
data[judet %in% c('braila','buzau','constanta','galati','tulcea','vrancea'), nuts := 'SE']
data[judet %in% c('arges','calarasi','dambovita','giurgiu','ialomita','prahova','teleorman'), nuts := 'Sud-Muntenia']
data[judet %in% c('bucuresti','ilfov'), nuts := 'Bucuresti-Ilfov']
data[judet %in% c('dolj','gorj','mehedinti','olt','valcea'), nuts := 'SV Oltenia']
data[judet %in% c('arad','caras-severin','hunedoara','timis'), nuts := 'Vest']

data[,partid_grup :='other']
data[partid %in% c('PARTIDUL SOCIAL DEMOCRAT', 'PARTIDUL NAȚIONAL LIBERAL', 'UNIUNEA DEMOCRATĂ MAGHIARĂ DIN ROMÂNIA', 'PARTIDUL MIȘCAREA POPULARĂ', 'PARTIDUL PRO ROMÂNIA', 'ALIANȚA USR PLUS'), partid_grup := partid]
data[partid %in% c('UNIUNEA SALVAȚI ROMÂNIA', 'ALIANȚA UNIUNEA SALVAȚI ROMÂNIA PARTIDUL LIBERTATE UNITATE ȘI SOLIDARITATE (ALIANȚA USR PLUS)'), partid_grup := 'ALIANȚA USR PLUS']

data[localitate == 'BUCURESTI', tip_uat:='M']

data$irreligious_high = 0
data[irreligious_ratio > 0.009, irreligious_high := 1]
data$neoprot_high = 0
data[neoprot_ratio > 0.2, neoprot_high := 1]

# Relabel comunes with >= 9000 inhabitants as cities
data[total >= 9000 & tip_uat == 'C', tip_uat:='O']

# Relable capital city since its population is much higher than that of any other city
data[localitate == 'BUCURESTI', tip_uat:='BUC']



data$tip_uat <- as.factor(data$tip_uat)
data$judet <- as.factor(data$judet)
data$partid_grup <- as.factor(data$partid_grup)
data$nuts <- as.factor(data$nuts)

#Should produce N-1 variables for a factor with N levels
data$tip_uat_c <- C(data$tip_uat, treatment)
data$judet_c <- C(data$judet, treatment)
data$partid_grup_c <- C(data$partid_grup, treatment)
# 
# data$min_dist_std <- scale(data$min_dist)
# data$alt_std <- scale(data$alt)
# data$tertiary_std <- scale(data$tertiary_ratio)
# data$total_log_std <- scale(log(data$tertiary_ratio))
# data$procent_vacc_std <- scale(data$procent_vacc)
# data$nearest_count_std <- scale(data$nearest_count)
# data$nearest_std_std <- scale(data$nearest_std)
# data$tertiary_log <- log(data$tertiary_ratio)
# data$illiterate_std <- scale(data$illiterate_ratio)
# data$over65_std <- scale(data$over65_ratio)
# data$irreligious_std <- scale(data$irreligious_ratio)
# data$protestant_std <- scale(data$protestant_ratio)



#########
## EDA ##
#########

ggplot(data, aes(x=illiterate_ratio, y=procent_vacc)) + geom_smooth() + geom_point(alpha=0.15) + facet_wrap(~tip_uat, scales='free_x')

ggplot(data[judet != 'ilfov',], aes(x=nearest_count, y=procent_vacc)) + geom_smooth() + geom_point() + facet_wrap(~tip_uat, scales='free_x')

ggplot(data, aes(x=partid_grup, y=procent_vacc)) + geom_smooth() + geom_boxplot()

data[nearest_count >= 100 & tip_uat=='O',]

ggplot(data, aes(x=long, y=lat, color=procent_vacc)) + geom_point(aes(size=log(total), alpha=procent_vacc), stroke=0.0) + scale_color_gradient(low = "#56B1F7", high = "#132B43")

ggplot(vacc_centers, aes(x=longitude, y=latitude)) + geom_point(aes(size=log(center_count)), alpha=0.75, stroke=0.0)


################
##  MODELING  ##
################

#######################
## Train test splits ##
#######################

data_c = data[!(localitate %in% c('COSTACHE NEGRI', 'SUBCETATE')) & tip_uat %in% c('C'),]

ic_train = sample(data_c[,.N], trunc(0.7 * data_c[,.N]))

c_train = data_c[ic_train,]
c_test = data_c[-ic_train,]


model_c = lm(procent_vacc ~ min_dist_std + alt_std + min_dist_std*alt_std + I(tertiary_ratio^3) + I(min_dist^2) + I(alt^2) + I(tertiary_ratio^2)
             + tertiary_std + total_log_std + judet_c + nearest_std_std + over65_std + partid_grup_c + irreligious_high + protestant_std + neoprot_high, data=c_train)


preds_c = predict(model_c, c_test)

mean(abs(preds_c - c_test$procent_vacc), na.rm=TRUE)



summary(model_c)
hist(model_c$residuals, bins=50)
AIC(model_c)
BIC(model_c)
plot(model_c)

#cities
data_om = data[!(localitate %in% c('BORSEC', 'TOPLITA', 'TANDAREI', 'CAMPENI')) & tip_uat %in% c('M', 'O'),]

iom_train = sample(data_om[,.N], trunc(0.7 * data_om[,.N]))

om_train = data_om[iom_train,]
om_test = data_om[-iom_train,]


model_om = lm(procent_vacc ~ min_dist + alt + tertiary_ratio + total_log_std + judet_c + over65_ratio + I(min_dist^2) + irreligious_std + neoprot_ratio, data=om_train)
 
preds_om = predict(model_om, om_test)

mean(abs(preds_om - om_test$procent_vacc), na.rm=TRUE)

summary(model_om)
hist(model_om$residuals)
AIC(model_om)
BIC(model_om)
plot(model_om)

################
## COVID DATA ##
################

covid_data <- fread('all_with_covid_rate_14oct21.csv')

covid_data[,partid_grup :='other']
covid_data[partid %in% c('PARTIDUL SOCIAL DEMOCRAT', 'PARTIDUL NAȚIONAL LIBERAL', 'UNIUNEA DEMOCRATĂ MAGHIARĂ DIN ROMÂNIA', 'PARTIDUL MIȘCAREA POPULARĂ', 'PARTIDUL PRO ROMÂNIA', 'ALIANȚA USR PLUS'), partid_grup := partid]
covid_data[partid %in% c('UNIUNEA SALVAȚI ROMÂNIA', 'ALIANȚA UNIUNEA SALVAȚI ROMÂNIA PARTIDUL LIBERTATE UNITATE ȘI SOLIDARITATE (ALIANȚA USR PLUS)'), partid_grup := 'ALIANȚA USR PLUS']

ggplot(covid_data[tip_uat=='C',], aes(x=procent_vacc, y=incidenta)) + geom_point() + facet_wrap(~judet, scales='free') + geom_smooth()

ggplot(covid_data[tip_uat %in% c('C'),], aes(x=procent_vacc, y=incidenta)) + geom_point() + facet_wrap(~partid_grup, scales='free') + geom_smooth()


##############
## maps 



## https://stackoverflow.com/questions/58607146/unable-to-run-a-simple-jni-program-error-message-when-installing-rjava-on-r-3/58647104#58647104
##################

ro <- bb("Romania")

map <- openmap(upperLeft=c(ro[4], ro[1]), lowerRight=c(ro[2], ro[3]), zoom = NULL,
               type = c("nps"),
               mergeTiles = TRUE)


# +proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# map.latlon <- openproj(map)

proj = data.table(projectMercator(data$lat, data$long))

data$lat_adj = proj$y
data$long_adj = proj$x


OSMap <- autoplot(map)  + ggtitle("Vaccination rates vs # of nearby centers") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=data, aes(x=long_adj, y=lat_adj, size=nearest_count, colour=procent_vacc), stroke=0.0, alpha=0.85) + scale_colour_gradient(low = "#54ADF2", high = "#122B43")

OSMap



