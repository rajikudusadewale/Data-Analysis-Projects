###############
library(tidyverse)
library(readr)
general <- read.csv('oil data.csv', stringsAsFactors = FALSE, header = TRUE)
View(general)
new_general <- general%>%
  filter(na.omit(Estmated.Quantity))%>%
  select(YEAR,Status, Cause,Contaminat,States.Affected,Estmated.Quantity)
View(new_general)

new_gen <- general%>%
  mutate(Est_qty = as.numeric(Estmated.Quantity))%>%
  filter(na.omit(Est_qty))%>%
  select(YEAR,Status, Cause,Contaminat,States.Affected,Estmated.Quantity)
View(new_general)


sapply(new_gen, function(x) sum(is.na(x), na.rm = TRUE)/ length(x) * 100)
mean(new_gen$Est_qty, na.rm = T)
new_gen$Est_qty[is.na(new_gen$Est_qty)] <- mean(new_gen$Est_qty, na.rm = T)

View(new_gen)


oil <- read_csv('oil data.csv',col_names = TRUE, na = c("","NA"))
new_gen <- oil%>%
  mutate(Est_qty = as.numeric( `Estmated Quantity`))%>%
  filter(na.omit(Est_qty))
View(new_general)

View(oil)
str(oil)
#oil$`Estmated Quantity` <- as.factor(oil$`Estmated Quantity`)
oil$Status <- as.factor(oil$Status)
oil$Cause <- as.factor(oil$Cause)
oil$Contaminat <- as.factor(oil$Contaminat)
oil$YEAR <- as.factor(oil$YEAR)
oil$`States Affected` <- as.factor(oil$`States Affected`)
new_oil <- oil%>%
  select(YEAR,Status, Cause,Contaminat,`States Affected`,`Estmated Quantity`)
View(new_oil)
str(new_oil)
sapply(new_oil, function(x) sum(is.na(x)) / length(x) * 100)
#sum(is.na(new_oil$States.Affected))
#sapply(new_oil, function(x) sum(x == "") /length(x) *100)
###############################################################
new_oil$Estmated_Quantity <- NA
new_oil$`Estmated Quantity` <- new_oil$Estmated_Quantity

sapply(new_oil$`Estmated Quantity`, function(x) sum(is.na(x), na.rm = TRUE)/ length(x) * 10)
new_oil$`Estmated Quantity`[is.na(new_oil$`Estmated Quantity`)] <- mean(new_oil$`Estmated Quantity`, na.rm = T)
mean(new_oil$`Estmated Quantity`, na.rm = T)

sapply(new_oil, function(x) sum(is.na(x)) / length(x) * 100)

View(new_oil)
########################################################################################33
#cleaning Cause variable
table(new_oil$Cause, useNA = 'always')
replace(new_oil$Cause,'','unknown')
new_oil$Cause[is.na(new_oil$Cause)] <- 'unknown'
new_oil$CAUSE <- NA

new_oil$CAUSE[new_oil$Cause %in% c('NA')] <- 'unknown'
new_oil$CAUSE[new_oil$Cause %in% c('other: mystery','MYSTERY SPILL','other: Mystery Sheen',
                             'other: mys','other: Mystery')] <- 'mystery'
new_oil$CAUSE[new_oil$Cause %in% c('cor')] <- 'corrosion'
new_oil$CAUSE[new_oil$Cause %in% c('ytd')] <- 'yet to determine'
new_oil$CAUSE[new_oil$Cause %in% c('sab')] <- 'sabotage'
new_oil$CAUSE[new_oil$Cause %in% c('ome')] <- 'operational error'
new_oil$CAUSE[new_oil$Cause %in% c('eqf',' other: STRUCURAL FAILURE')] <- 'equipment failure'
new_oil$CAUSE[new_oil$Cause %in% c('other:natural disaster (flood)','other:natural disaster','other: Heavy downpour resulted in overflow of oily water','other: High Sea Tide')] <- 'Natural Disaster'
new_oil$CAUSE[new_oil$CAUSE %in% c('other: Unknown','other:',
                                    'other:Trapped crude as a result of previous spill',
                                    'other:overflow of bundwall')] <- 'other'

table(new_oil$CAUSE, useNA = 'always')
new_oil$CAUSE[is.na(new_oil$CAUSE)] <- 'unknown'
table(new_oil$CAUSE, useNA = 'always')

####################################################################################3
sapply(new_oil, function(x) sum(is.na(x))/length(x) * 100)
###########################################
#cleaning contaminant var
table(new_oil$Contaminat, useNA = 'always')
new_oil$Contaminat[is.na(new_oil$Contaminat)] <- 'cr'

#remove(new_oil$contaminant)
new_oil$contaminat <- NA
new_oil$contaminat[new_oil$Contaminat %in% c('cr')] <- 'crude oil'
new_oil$contaminat[new_oil$Contaminat %in% c('gs','gas')] <- 'gas'
new_oil$contaminat[new_oil$Contaminat %in% c('co','CON','co,ga')] <- 'Condense'
new_oil$contaminat[new_oil$Contaminat %in% c('no')] <- 'no spill'
new_oil$contaminat[new_oil$Contaminat %in% c('re (PMS)','re(pms)','re','re(AGO)','re(PMS)')] <- "refined products"
new_oil$contaminat[new_oil$Contaminat %in% c('ch')] <- "chemical"
new_oil$contaminat[new_oil$Contaminat %in% c('other:waste oil ','other:oily water','other:OILY BASED MATERIAL','other:non','
                                              other:Burnt Gas','other:(unknown)','other:(oily water base on seen appearance)',
                                              'other:(oily materia)','other: Oily Water','other: none','other: Mystery sheen',
                                              'other: Hydraulic Oil','other: (water based mud)',' other: (oily water)',
                                              'other: (fire)','other(oily material):','other: ','other: (oily material)',
                                              'other: (produced water)','other: gaseous emision','other: Hydrocarbon gas with condensate',
                                              'other: Non aqueous based mud','other: NONE','other:(circulation sludge)','other:(oily material)',
                                              'other:(oily water)','other:(used engine oil)','other:Hydraulic oil','other:Oil/gel mixture',
                                              'other:OILY MATERIAL','oth','boom','other:OILY WATER','other:WATER FROM OIL PIPELINE','0.01')] <- 'other'

new_oil$contaminat[is.na(new_oil$contaminat)] <- 'crude oil'

sapply(new_oil$contaminat, function(x) sum(is.na(x))/length(x) * 100)
table(new_oil$contaminat, useNA = 'always')
sapply(new_oil, function(x) sum(is.na(x)/length(x)) * 100)
new_oil <- new_oil[,-7]
View(new_oil)
################################################################################################
#cleaning state affected
new_oil$state_affected <- NA
table(new_oil$`States Affected`, useNA = 'always')
#new_oil$`States Affected`[is.na(new_oil$`States Affected`)] <- 'unknown'
new_oil$state_affected[new_oil$`States Affected` %in% c('AB')] <- 'ABIA'
new_oil$state_affected[new_oil$`States Affected` %in% c('AK')] <- 'AKURE'
new_oil$state_affected[new_oil$`States Affected` %in% c('BY')] <- 'BAYELSA'
new_oil$state_affected[new_oil$`States Affected` %in% c('DE')] <- 'DELTA'
new_oil$state_affected[new_oil$`States Affected` %in% c('ED')] <- 'EDO'
new_oil$state_affected[new_oil$`States Affected` %in% c('FCT')] <- 'FCT'
new_oil$state_affected[new_oil$`States Affected` %in% c('IM','IM,IM')] <- 'IMO'
new_oil$state_affected[new_oil$`States Affected` %in% c('LA','KADUNA','KADUNA-NORTH','OG','KO','KW')] <- 'OTHER STATES'
new_oil$state_affected[new_oil$`States Affected` %in% c('ON')] <- 'ONDO'
new_oil$state_affected[new_oil$`States Affected` %in% c('RI','RI,RI','RI,undefined','undefined')] <- 'RIVER'
new_oil$state_affected[new_oil$`States Affected` %in% c('C R O S S R I V E R')] <- 'CROSSRIVER'
new_oil$state_affected[is.na(new_oil$state_affected)] <- 'unknown'
new_oil$CAUSE <- as.factor(new_oil$CAUSE)
new_oil$contaminat <- as.factor(new_oil$contaminat)
new_oil$state_affected <- as.factor(new_oil$state_affected)



ggplot(new_oil, aes(x= CAUSE, fill= factor(Status)))+
  geom_bar()

ggplot(new_oil, aes(x= YEAR, fill= factor(contaminat)))+
  geom_bar()

ggplot(new_oil, aes(x= YEAR, fill= factor(state_affected)))+
  geom_bar()+
  #facet_wrap(~Status)+
  geom_density(alpha=0.5)

ggplot(new_oil, aes(x= CAUSE, fill= factor(state_affected)))+
  geom_bar()

ggplot(new_oil, aes(x= CAUSE, fill= factor(YEAR)))+
  geom_bar()

ggplot(new_oil, aes(x= YEAR , fill= factor(CAUSE)))+
  geom_bar()

ggplot(new_oil, aes(x = Status))+
  theme_bw()+
  geom_bar()
  
