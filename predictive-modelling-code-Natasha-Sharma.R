#Installing all the packages
install.packages('expss')
install.packages('dplyr')
install.packages('labelled')
install.packages('ggplot2')
install.packages('reshape')
install.packages('data.table')
install.packages('gridExtra')
install.packages('DescTools')
install.packages('MASS')
install.packages('ISLR')
install.packages('caret')
install.packages('e1071')
install.packages('leaps')

#load the expss library that will be used for variable labeling
library(expss)
library(dplyr)
library(labelled)
library(ggplot2) #powerful graphics generator for R
library(reshape)
library(data.table)
library(gridExtra)
library(DescTools)
library(MASS)
library(ISLR)
library(caret)
library(e1071)
library(leaps)


#find your working directory for R
getwd()

#change your working directory to the location where your files are
#  (make sure you put the folder path for your new directory between the 
#   quotation marks)
setwd("C:/Users/abhih/Documents/Natasha/624/Assignment 4")

#load data
a<-readRDS("./training_final.RDS")

### Data Transform

#label initial_call_type
val_lab(a$initial_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 ACC
6 Active Shooter
7 ADM
8 Altered Mental Status
9 Alt Mental Status-Fever&Cough
10 Amputation, Arm, Hand,Leg,Foot
11 Amputation, Fingers Or Toes
12 Anaphylactic Shock-Fever&Cough
13 Anaphylaxis
14 Card Or Resp Arrest-Fevercough
15 Cardiac Arrest
16 Asthma Attack - Fever&Cough
17 Asthma Patient Fever-Travel
18 Asthma Attack
19 BBP
20 Major Burns 18% Adlt 10% Child
21 Minor Burns <18% Adlt Or <10%
22 Cardiac Condition
23 CARDBR
24 Cardiac Condition-Fever&Cough
25 CDBRFC
26 Child Abuse
27 Choking
28 Choking Fever&Cough
29 Hypothermia
30 COVINF
31 Stroke
32 CVA (Stroke)
33 Stroke Critical - Fever&Cough
34 Stroke - Fever & Cough
35 Death Confirm By Medical Auth
36 Difficult Breather
37 Diff Breathing - Fever&Cough
38 Difficult Breathing Fever-Travel
39 Difficult Breather Rf
40 Death Confirm By Medical Auth
41 DRILL
42 Drowning
43 Hx Drug Or Alcohol Abuse
44 Hx Drug Or Alchl Abuse-Fev&Cou
45 Psychiatric Patient
46 EDPC
47 EDPW
48 Electrocution
49 Evac 
50 Fire75 Working Fire
51 Fire76 High Rise Commercial
52 Fire77 High Rise Residential
53 Gyn Bleeding-Pt Not Pregnant
54 Gyn-Severe Pain-Bleeding
55 Heat Exhaustion
56 Hypertension
57 Internal Bleeding
58 Internal Bleeding-Fever&Cough
59 Inhalation Of Smoke
60 Injury Lower Ext In Elderly
61 Major Injury
62 Minor Injury
63 Non-Critical Injury
64 Jumper Down
65 Jumper Up
66 One Alarm Fire
67 One Alarm Fire
68 Two Alarm Fire
69 Two Alarm Fire
70 Three Alarm Fire
71 Four Alarm Fire
72 MCI25
73 Five Alarm Fire Or Greater
74 Occupied High-Rise Building
75 Occupied High-Rise Building
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Ground Transport Incident
82 Ground Transport Incident
83 Structural Collaspe [Specify]
84 Construction-Demolition Incid
85 Construction-Demolition Incid
86 Confined Space Incident
87 MCI37
88 MCI40
89 Aircraft Incident - Crash
90 Civil Distrubance
91 Hostage Situation - Barricaded
92 Hostage Situation - Barricaded
93 Power Failure - Blackout
94 Active Shooter
95 Active Shooter
96 All Other MCIs
97 All Other MCIs
98 MCI76
99 MCI77
100 Hazardous Materials Incident
101 Hazardous Materials Incident
102 MECHE
103 MECHV
104 Reaction To Med - Fever&Cough
105 Reaction To Medication
106 Medevac, T-C Authority Only
107 MOSILL
108 MOSINJ
109 Auto Accident, No Confirmd Inj
110 Auto Acc W-Injuries
111 MVAINM
112 NOVEH
113 Obstetric Complications
114 Female In Labor
115 Major Obstetrical Complaint
116 Miscarriage
117 Baby Out Or Imminent Birth
118 Unknown Condition
119 Police 10-13, Unconfirmed
120 Police 10-13, Confirmed
121 Sick Ped<5 Yrs-Fever & Cough
122 Sick Ped<5 Yrs-Rash & Fever
123 Pedestrian Struck
124 PEDSTS
125 RADIO
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 Sick - Minor - Fever & Cough
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 STUCK
151 T-ARST
152 T-DFBR
153 T-EDP
154 T-INJ
155 T-SICK
156 T-TEXT
157 T-TRMA
158 T-UNC
159 T-UNKN
160 Test Kdt-Modat
161 Multiple Trauma Patient
162 TRAUMS
163 Unconscious Patient
164 Unc Patient - Fever & Cough
165 Unconscious Fever-Travel Patient
166 Unconscious Patient-Rash&Fever
167 Caller Has No Pt Medical Info
168 Venom (Snake Bites)
")

#check that initial call type labels loaded correctly
data.frame(table(a$initial_call_type))

#label final_call_type
val_lab(a$final_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 Active Shooter
6 ALMNFC
7 Altered Mental Status
8 Alt Mental Status-Fever&Cough
9 Amputation, Arm, Hand,Leg,Foot
10 Amputation, Fingers Or Toes
11 Anaphylactic Shock-Fever&Cough
12 Anaphylaxis
13 Card Or Resp Arrest-Fevercough
14 Cardiac Arrest
15 ARSTFC
16 Asthma Attack - Fever&Cough
17 Asthma Attack
18 Major Burns 18% Adlt 10% Child
19 Minor Burns <18% Adlt Or <10%
20 Cardiac Condition
21 CARDBR
22 Cardiac Condition-Fever&Cough
23 CDBRFC
24 Child Abuse
25 Choking
26 Choking Fever&Cough
27 Hypothermia
28 COVINF
29 Stroke
30 CVA (Stroke)
31 Stroke Critical - Fever&Cough
32 Stroke - Fever & Cough
33 Difficult Breather
34 Diff Breathing - Fever&Cough
35 Difficult Breathing Fever-Travel
36 Difficult Breather Rf
37 Death Confirm By Medical Auth
38 Drowning
39 Hx Drug Or Alcohol Abuse
40 Hx Drug Or Alchl Abuse-Fev&Cou
41 Psychiatric Patient
42 EDPC
43 EDPW
44 Electrocution
45 Evac 
46 Fire75 Working Fire
47 Fire77 High Rise Residential
48 Gyn Bleeding-Pt Not Pregnant
49 Gyn-Severe Pain-Bleeding
50 Heat Exhaustion
51 Hypertension
52 Internal Bleeding
53 Internal Bleeding-Fever&Cough
54 Inhalation Of Smoke
55 Injury Lower Ext In Elderly
56 Major Injury
57 Minor Injury
58 Non-Critical Injury
59 Jumper Down
60 Jumper Up
61 One Alarm Fire
62 One Alarm Fire
63 Two Alarm Fire
64 Two Alarm Fire
65 Three Alarm Fire
66 Three Alarm Fire
67 Four Alarm Fire
68 Four Alarm Fire
69 MCI25
70 Five Alarm Fire Or Greater
71 Occupied High-Rise Building
72 Occupied High-Rise Building
73 MCI27
74 Medical Facility Evacuation
75 Criminal Detection Facil Incid
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Rapid Transit-Rail Incident
82 Ground Transport Incident
83 Ground Transport Incident
84 Structural Collaspe [Specify]
85 Structural Collaspe [Specify]
86 Construction-Demolition Incid
87 Construction-Demolition Incid
88 MCI35
89 Confined Space Incident
90 MCI37
91 Marine - Harbor Incident
92 Marine - Harbor Incident
93 MCI40
94 Aircraft Incident - Crash
95 MCI42
96 Civil Distrubance
97 Hostage Situation - Barricaded
98 Hostage Situation - Barricaded
99 Power Failure - Blackout
100 Power Failure - Blackout
101 Active Shooter
102 Active Shooter
103 All Other MCIs
104 All Other MCIs
105 MCI76
106 MCI77
107 Hazardous Materials Incident
108 Hazardous Materials Incident
109 Reaction To Med - Fever&Cough
110 Reaction To Medication
111 Medevac, T-C Authority Only
112 Auto Accident, No Confirmd Inj
113 Auto Acc W-Injuries
114 MVAINM
115 Obstetric Complications
116 Female In Labor
117 Major Obstetrical Complaint
118 Miscarriage
119 Baby Out Or Imminent Birth
120 Unknown Condition
121 Police 10-13, Unconfirmed
122 Police 10-13, Confirmed
123 Sick Ped<5 Yrs-Fever & Cough
124 Sick Ped<5 Yrs-Rash & Fever
125 Pedestrian Struck
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 SICMFC
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 T-ABDP
151 T-ARST
152 T-CARD
153 T-CDBR
154 T-DFBR
155 T-EDP
156 T-INBL
157 T-INJ
158 T-OBST
159 T-OTHR
160 T-SICK
161 T-TEXT
162 T-TRMA
163 T-UNC
164 T-UNKN
165 Multiple Trauma Patient
166 Unconscious Patient
167 Unc Patient - Fever & Cough
168 Unconscious Fever-Travel Patient
169 Unconscious Patient-Rash&Fever
170 Caller Has No Pt Medical Info
171 Venom (Snake Bites)
")

#check that final call type labels loaded correctly
data.frame(table(a$final_call_type))

#Create Date Variable
a$date_m <- as.Date(a$incident_dt, format="%m/%d/%Y")

#create month variable
a$month <- format(as.Date(a$incident_dt), "%m")
a$month_name <- format(as.Date(a$incident_dt), "%b")
a$month_name <- factor(a$month_name,levels=month.abb)

#create a day variable
a$day <- format(as.Date(a$incident_dt), "%d")

#create day of week variable
a$dow <- weekdays(as.Date(a$incident_dt))
a$dow <- factor(a$dow,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#create hour of day variable
a$hour <- format(a$incident_dt, "%H")

#label incident disposition code
val_lab(a$incident_disposition_code)=num_lab("
82	transporting patient
83	patient pronounced dead
87	cancelled
90	unfounded
91	condition corrected
92	treated not transported
93	refused medical aid
94	treated and transported
95	triaged at scene no transport
96	patient gone on arrival
")

#Taking all the columns that are needed from Calls for Service Dataset
columns_needed<-c("incident_response_seconds_qy", "initial_severity_level_code", "final_severity_level_code", "held_indicator", "borough", 
                  "special_event_indicator", "standby_indicator", "transfer_indicator", "initial_call_type", "month", "day", "dow", "hour", "date_m", 
                  "incident_year")
View(a[columns_needed])
a<-a[columns_needed]

#Combining the weather data with the Calls For Service Data

#Loading Weather Data
b<-read.csv("./GHCND_NY_Central_Park_20080101_20161231.csv",header=TRUE)

#labelling different variables
var_label(b$AWND) <- "Average Daily Wind Speed (mi/h)"
var_label(b$FMTM) <- "Time of Fastest 1-minute wind (HHMM)"
var_label(b$PGTM) <- "Peak Gust Time (HHMM)"
var_label(b$PRCP) <- "Precipitation (in)"
var_label(b$SNOW) <- "Snowfall (in)"
var_label(b$SNWD) <- "Snow depth (in)"
var_label(b$TMAX) <- "Max Temp (F)"
var_label(b$TMIN) <- "Min Temp (F)"
var_label(b$WDF2) <- "Direction of fastest 2-minute wind (degrees)"
var_label(b$WDF5) <- "Direction of fastest 5-second wind (degrees)"
var_label(b$WSF2) <- "Fastest 2-minute wind speed (mph)"
var_label(b$WSF5) <- "Fastest 5-second wind speed (mph)"
var_label(b$WT01) <- "Fog"
var_label(b$WT02) <- "Heavy Fog"
var_label(b$WT03) <- "Thunder"
var_label(b$WT04) <- "Ice pellets"
var_label(b$WT05) <- "Hail"
var_label(b$WT06) <- "Glaze or rime"
var_label(b$WT07) <- "Dust or sand"
var_label(b$WT08) <- "Smoke or haze"
var_label(b$WT09) <- "Blowing or drifting snow"
var_label(b$WT11) <- "High or damaging winds"
var_label(b$WT13) <- "Mist"
var_label(b$WT14) <- "Drizzle"
var_label(b$WT16) <- "Rain"
var_label(b$WT17) <- "Freezing Rain"
var_label(b$WT18) <- "Snow"
var_label(b$WT19) <- "Unknown Source of Precipitation"
var_label(b$WT22) <- "Freezing Fog"

#replace blanks with zeros for binary variables
wts <- grep('^WT', names(b), value = TRUE) #creates a new vector of all the binary variables related to weather type
b[wts][is.na(b[wts])] <- 0 #replaces all of the "NA" values in those binary variables with 0s

#Creating Date variable to merge data with Calls for service data
b$date_m <- as.Date(b$DATE, "%m/%d/%Y")

#Taking all the columns from Weather data that are needed
columns_needed<-c("date_m","AWND", "SNWD", "WT01", "WT16", "WT18", "WT04")
b<-b[columns_needed]

#Mergind Calls data with weather data
merged_data_a_b <- left_join(a,b,by="date_m")

#Loading the CHP Data Subset for use in Modelling
c<-read.csv("./Community_Data_subset.csv",header=TRUE)

colnames(c)

#Joining CHP Data with the Calls for service data
df<-left_join(merged_data_a_b,c,by="borough")

#Dummy coding top 10 specific initial_call_types
initial_call_type_table<-table(df$initial_call_type)
initial_call_type_counts<-data.frame(dimnames(initial_call_type_table)) 
initial_call_type_counts$count<-initial_call_type_table
colnames(initial_call_type_counts)<-c("initial_call_type_name", "count")
initial_call_type_counts<-initial_call_type_counts[order(initial_call_type_counts$count, decreasing=TRUE)[1:10],]

table(df$initial_call_type)

#135 Sick
df$Sick <- ifelse(df$initial_call_type == 135, 1, 0)
table(df$Sick)

#63 Non-Critical Injury
df$NonCriticalInjury <- ifelse(df$initial_call_type == 63, 1, 0)
table(df$NonCriticalInjury)

#36 Difficult Breather
df$DifficultBreather <- ifelse(df$initial_call_type == 36, 1, 0)
table(df$DifficultBreather)

#45 Psychiatric Patient
df$PsychiatricPatient <- ifelse(df$initial_call_type == 45, 1, 0)
table(df$PsychiatricPatient)

#43 Hx Drug Or Alcohol Abuse
df$HxDrugOrAlcoholAbuse <- ifelse(df$initial_call_type == 43, 1, 0)
table(df$HxDrugOrAlcoholAbuse)

#163 Unconscious Patient
df$UnconsciousPatient <- ifelse(df$initial_call_type == 163, 1, 0)
table(df$UnconsciousPatient)

#4 Abdominal Pain
df$AbdominalPain <- ifelse(df$initial_call_type == 4, 1, 0)
table(df$AbdominalPain)

#167 Caller Has No Pt Medical Info
df$CallerHasNoPtMedicalInfo <- ifelse(df$initial_call_type == 167, 1, 0)
table(df$CallerHasNoPtMedicalInfo)

#22 Cardiac Condition
df$CardiacCondition <- ifelse(df$initial_call_type == 22, 1, 0)
table(df$CardiacCondition)

#110 Auto Acc W-Injuries
df$AutoAccWInjuries <- ifelse(df$initial_call_type == 110, 1, 0)
table(df$AutoAccWInjuries)

#Dummy coding special_event_indicator
df$special_event_indicator <- ifelse(df$special_event_indicator == "Y", 1, 0)
table(df$special_event_indicator)

#Dummy coding special_event_indicator
df$standby_indicator <- ifelse(df$standby_indicator == "Y", 1, 0)
table(df$standby_indicator)

#Dummy coding transfer_indicator
df$transfer_indicator <- ifelse(df$transfer_indicator == "Y", 1, 0)
table(df$transfer_indicator)

#Dummy Coding borough
#Bronx
df$BRONX <- ifelse(df$borough == "BRONX", 1, 0)
table(df$BRONX)

#BROOKLYN
df$BROOKLYN <- ifelse(df$borough == "BROOKLYN", 1, 0)
table(df$BROOKLYN)

#MANHATTAN
df$MANHATTAN <- ifelse(df$borough == "MANHATTAN", 1, 0)
table(df$MANHATTAN)

#QUEENS 
df$QUEENS <- ifelse(df$borough == "QUEENS", 1, 0)
table(df$QUEENS)

#RICHMOND / STATEN ISLAND
df$RICHMOND_STATEN_ISLAND <- ifelse(df$borough == "RICHMOND / STATEN ISLAND", 1, 0)
table(df$RICHMOND_STATEN_ISLAND)

table(df$borough)

#Taking out columns that are needed
columns_needed<-c("initial_severity_level_code", "final_severity_level_code", "held_indicator", "special_event_indicator", "standby_indicator",
                  "transfer_indicator", "month", "incident_year", "day", "hour", "AWND", "SNWD",
                  "WT01", "WT04", "WT16", "WT18","OverallPopulation_rate", "Age0to17_rate", "Age18to24_rate", "Age25to44_rate", "Age45to64_rate",
                  "Age65plus_rate",  "Sick", "NonCriticalInjury", "DifficultBreather", "PsychiatricPatient",
                  "HxDrugOrAlcoholAbuse", "UnconsciousPatient" ,"AbdominalPain" ,"CallerHasNoPtMedicalInfo", 
                  "CardiacCondition", "AutoAccWInjuries", "BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "RICHMOND_STATEN_ISLAND","incident_response_seconds_qy")
df_final<-df[columns_needed]

#Dropping all the rows in the final merged Data df where incident_response_seconds_qy is NA
df_final<-df_final[complete.cases(df_final$incident_response_seconds_qy),]

#Getting the NA counts for all columns in X
na_counts <- colSums(is.na(df_final))
na_counts


#Checking the rows where borough is not NA but OverallPopulation_rate is NA
View(df_final[is.na(df_final$OverallPopulation_rate),])

#The OverallPopulation_rate is NA in data for CommunityDistrict codes which were not present in the CHP data. 
#There are 116 rows where this column has the value NA. Since the removal of these rows should not affect the data,
#we can remove all the rows where OverallPopulation_rate is NA

df_final<-df_final[complete.cases(df_final$OverallPopulation_rate),]

#Changing DataTypes
#Converting Month datatype to Number
df_final$month<-as.numeric(df_final$month)
table(df_final$month)

#Converting Day datatype to Number
df_final$day<-as.numeric(df_final$day)
table(df_final$day)

#Converting held_indicator column to boolean
df_final$held_indicator <- ifelse(df_final$held_indicator == "Y", 1, 0)

#Coverting Hour of day into numeric
df_final$hour<-as.numeric(df_final$hour)

#Imputing NA values in the AWND column using Grouped Mean of AWND by the month
df_final <- df_final %>%
  group_by(month) %>%
  mutate(AWND := if_else(is.na(AWND), mean(AWND, na.rm = TRUE), AWND))

#Getting the NA counts for all columns in X
na_counts <- colSums(is.na(df_final))
na_counts

#Boxplot of incident_response_seconds_qy
boxplot(df_final$incident_response_seconds_qy,
        ylab = "incident_response_seconds_qy",
        main = "Vertical Boxplot")

#Removing outliers from incident_response_seconds_qy by taking the values that are in the Interquartile Range
quartiles <- quantile(df_final$incident_response_seconds_qy, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df_final$incident_response_seconds_qy)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

df_final_no_outlier <- subset(df_final, df_final$incident_response_seconds_qy > Lower & df_final$incident_response_seconds_qy < Upper)

#Correlation Matrix - a correlation analysis between all predictor and response variables
res <- cor(df_final_no_outlier)
res <- round(res, 2)

#There are some rows where incident_response_seconds_qy is zero. These maybe the cases where an ambulance might not have been dispatched
#Since we don't require such cases in our modelling we can remove them.
df_final_no_outlier <- df_final_no_outlier[df_final_no_outlier$incident_response_seconds_qy > 0,]

#Fitting the Linear Regression model with all the predictors
lm.fit<-lm(incident_response_seconds_qy ~ initial_severity_level_code + held_indicator + standby_indicator + incident_year + hour + SNWD + Age65plus_rate +
             Sick + NonCriticalInjury + DifficultBreather + UnconsciousPatient + AbdominalPain + CardiacCondition + BRONX + RICHMOND_STATEN_ISLAND, data = df_final_no_outlier)
summary(lm.fit)
View(summary(lm.fit)$coefficients)
AIC(lm.fit)
BIC(lm.fit)

#Taking a sample out of data for plotting
df_final_no_outlier_sample<-sample_n(df_final_no_outlier, 50)
attach(df_final_no_outlier_sample)

#incident_response_seconds_qy vs SNWD Plot
plot(df_final_no_outlier_sample$incident_response_seconds_qy, df_final_no_outlier_sample$SNWD, main="incident_response_seconds_qy VS SNWD",
     xlab="incident_response_seconds_qy ", ylab="SNWD", pch=19)

#incident_response_seconds_qy vs hour plot
plot(df_final_no_outlier_sample$incident_response_seconds_qy, df_final_no_outlier_sample$hour, main="incident_response_seconds_qy VS hour",
     xlab="incident_response_seconds_qy ", ylab="hour", pch=19)

#Best Subset Selection
df_final_no_outlier <- df_final_no_outlier[c("incident_response_seconds_qy", "initial_severity_level_code", "held_indicator", "standby_indicator", "incident_year", "hour", "SNWD", "Age65plus_rate",
                                               "Sick", "NonCriticalInjury", "DifficultBreather", "UnconsciousPatient", "AbdominalPain", "CardiacCondition", "BRONX", "RICHMOND_STATEN_ISLAND")]

df_final_no_outlier <- subset(df_final_no_outlier, df_final_no_outlier$initial_severity_level_code > 0 & df_final_no_outlier$initial_severity_level_code < 9)
df_final_no_outlier <- na.omit(df_final_no_outlier)
regfit.full<-regsubsets(incident_response_seconds_qy ~ ., data=df_final_no_outlier, nvmax = 15)
reg.summary<-summary(regfit.full)
names(reg.summary)

#Plotting all the parameters
#rsq
plot(reg.summary$rsq, xlab="Number of Variables", ylab="rsq")
points(which.max(reg.summary$rsq), max(reg.summary$rsq), col = "red", pch = 16)

#rss
plot(reg.summary$rss, xlab="Number of Variables", ylab="rss")
points(which.min(reg.summary$rss), min(reg.summary$rss), col = "red", pch = 16)

#adjr2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="adjr2")
points(which.max(reg.summary$adjr2), max(reg.summary$adjr2), col = "red", pch = 16)

#cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")
points(which.min(reg.summary$cp), min(reg.summary$cp), col = "red", pch = 16)

#bic
plot(reg.summary$bic, xlab="Number of Variables", ylab="bic")
points(which.min(reg.summary$bic), min(reg.summary$bic), col = "red", pch = 16)


#Splitting the data into two samples: 2008-2014, and 2015-2016
train<-subset(df_final_no_outlier, df_final_no_outlier$incident_year >= 2008 & df_final_no_outlier$incident_year <= 2014)
test<-subset(df_final_no_outlier, df_final_no_outlier$incident_year >= 2015)

#Running n regression models and regressing the response variable [incident_response_seconds_qy] 
# on the predictors [x], using the training dataset.  
regfit.train<-regsubsets(incident_response_seconds_qy ~ ., data=train, nvmax = 15)
reg.summary<-summary(regfit.train)
names(reg.summary)

#7.	Creating a matrix of model parameters (model.matrix function) using the test data, 
# as well as a matrix to capture the prediction error between your training and test models.
test_mat = model.matrix (incident_response_seconds_qy~., data = test)
val_errors = rep(NA,15)

#8.	Running a for loop to extract the coefficients from the trained models, the predicted values for the test data, 
# and the Mean Squared Error (MSE) in prediction between the two.
# Iterates over each size i
for(i in 1:15){
  
  # Extract the vector of predictors in the best fit model on i predictors
  coefi = coef(regfit.train, id = i)
  
  # Make predictions using matrix multiplication of the test matirx and the coefficients vector
  pred = test_mat[,names(coefi)]%*%coefi
  
  # Calculate the MSE
  val_errors[i] = mean((test$incident_response_seconds_qy-pred)^2)
}

# Find the model with the smallest error
min = which.min(val_errors)

# Plot the errors for each model size
plot(val_errors, type = 'b')
points(min, val_errors[min][1], col = "red", cex = 2, pch = 20)

#Model with best prediction is the once with 14 variables. 
regfit_best = regsubsets(incident_response_seconds_qy~., data = df_final_no_outlier, nvmax = 14)
coef(regfit_best, 14)
lm.fit<-lm(incident_response_seconds_qy ~ initial_severity_level_code + held_indicator + standby_indicator + incident_year + hour + SNWD + Age65plus_rate +
             Sick + NonCriticalInjury + DifficultBreather + UnconsciousPatient + AbdominalPain + CardiacCondition + RICHMOND_STATEN_ISLAND, data = df_final_no_outlier)
View(summary(lm.fit)$coefficients)
AIC(lm.fit)
BIC(lm.fit)

#Running the predict.regsubsets function provided in the sample code for the lab work in Module 6.  This code will be used in the cross-validation.
predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]]) # Extract the formula used when we called regsubsets()
  mat = model.matrix(form,newdata)    # Build the model matrix
  coefi = coef(object,id=id)          # Extract the coefficiants of the ith model
  xvars = names(coefi)                # Pull out the names of the predictors used in the ith model
  mat[,xvars]%*%coefi               # Make predictions using matrix multiplication
}


#Cross Validation
k = 13        # number of folds
set.seed(1)   # set the random seed so we all get the same results

# Assign each observation to a single fold
folds = sample(1:k, nrow(df_final_no_outlier), replace = TRUE)

# Creating a matrix to store the results of our upcoming calculations
cv_errors = matrix(NA, k, 15, dimnames = list(NULL, paste(1:15)))

# Outer loop iterates over all folds
for(j in 1:k){
  
  # The perform best subset selection on the full dataset, minus the jth fold
  best_fit = regsubsets(incident_response_seconds_qy~., data = df_final_no_outlier[folds!=j,], nvmax=15)
  
  # Inner loop iterates over each size i
  for(i in 1:15){
    
    # Predict the values of the current fold from the "best subset" model on i predictors
    pred = predict(best_fit, df_final_no_outlier[folds==j,], id=i)
    
    # Calculate the MSE, store it in the matrix we created above
    cv_errors[j,i] = mean((df_final_no_outlier$incident_response_seconds_qy[folds==j]-pred)^2)
  }
}

# Take the mean of over all folds for each model size
mean_cv_errors = apply(cv_errors, 2, mean)

# Find the model size with the smallest cross-validation error
min = which.min(mean_cv_errors)

# Plot the cross-validation error for each model size, highlight the min
plot(mean_cv_errors, type='b')
points(min, mean_cv_errors[min][1], col = "red", cex = 2, pch = 20)

#According to the cross-validation approach the model with 15 predictors is the best
lm.fit<-lm(incident_response_seconds_qy ~ ., data = df_final_no_outlier)
View(summary(lm.fit)$coefficients)
AIC(lm.fit)
BIC(lm.fit)
summary(lm.fit)
