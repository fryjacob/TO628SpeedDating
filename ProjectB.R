speed <- read.csv("Speed Dating Data.csv")
speed$condtn <- as.factor(speed$condtn)
speed$gender <- as.factor(speed$gender)
speed$match <- as.factor(speed$match)
speed$field_cd <- as.factor(speed$field_cd)
levels(speed$field_cd) <- c("Law","Math","SocScie/Psych", "MedSci", "Engineering", "English", "History", "Business", "Education", "Bio","SocialWork","Undergrad", "PoliSci", "Film","FineArts","Lang","Architecture","Other")
speed$race <- as.factor(speed$race)
speed$goal <- as.factor(speed$goal)
levels(speed$goal) <- c("FunNightOut", "MeetNewPpl", "GetADate","SRSRelationship", "ToSayIDidIt","Other")
speed$date <- as.factor(speed$date)
levels(speed$date) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
speed$go_out <- as.factor(speed$go_out)
levels(speed$go_out) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
speed$career_c <-as.factor(speed$career_c)
levels(speed$career_c) <- c("Lawyer","Academic/Research","Psychologist","DocMed", "Engineer", "Entertainment", "Banking/Consulting", "RealEstate","IntlAffairs","Undecided","SocialWork","SpeechPath","Politics", "ProSports", "Other", "Journalism", "Architecture")
speed$race_o <-as.factor(speed$race_o) 
speed$dec_o <- as.factor(speed$dec_o)
speed$samerace <- as.factor(speed$samerace)

sd2 <- speed
sd2 <- sd2[ , -1] #IID  
sd2 <- sd2[, -1] #ID  
sd2 <- sd2[, -2] #IDG
sd2 <- sd2[, -3] #Wave
sd2 <- sd2[, -3] #Round
sd2 <- sd2[, -3] #Position
sd2 <- sd2[, -3] #Postion1
sd2 <- sd2[, -4] #Partner 
sd2 <- sd2[, -4] #PID
sd2 <- sd2[, -26]#Field
sd2 <- sd2[, -(27:29)]#Academics
sd2 <- sd2[,-(30:32)]#Socioeconomic 
sd2 <- sd2[,-33]#Career
sd2 <- sd2[,-(59:64)]#What others look for
sd2 <- sd2[,-(70:74)]#Others perception
sd2 <- sd2[,-(81:92)]#Data gathered after intitial
sd2 <- sd2[,(1:79)]
sd2 <- sd2[,-(70:79)] #Removes Post First Date
sd2 <- sd2[,-52]#exclude expnum

sdrandom <- sd2[sample(nrow(sd2), nrow(sd2)),] #Get a random sample since the data is organized by participant

sdclean <- na.omit(sdrandom) #Remove rows with NA values to create a "clean" set

prop.table(table(sdclean$match))


