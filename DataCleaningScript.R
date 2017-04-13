speed <- read.csv("Speed Dating Data.csv")
speed$gender <- as.factor(speed$gender)
speed$match <- as.factor(speed$match)
speed$field_cd <- as.factor(speed$field_cd)
levels(speed$field_cd) <- c("Law","Math","SocScie/Psych", "MedSci", "Engineering", "English", "History", "Business", "Education", "Bio","SocialWork","Undergrad", "PoliSci", "Film","FineArts","Lang","Architecture","Other")
speed$race <- as.factor(speed$race)
speed$date <- as.factor(speed$date)
levels(speed$date) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
speed$go_out <- as.factor(speed$go_out)
levels(speed$go_out) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
speed$career_c <-as.factor(speed$career_c)
levels(speed$career_c) <- c("Lawyer","Academic/Research","Psychologist","DocMed", "Engineer", "Entertainment", "Banking/Consulting", "RealEstate","IntlAffairs","Undecided","SocialWork","SpeechPath","Politics", "ProSports", "Other", "Journalism", "Architecture")
speed$race_o <-as.factor(speed$race_o) 
speed$samerace <- as.factor(speed$samerace)

sd2 <- speed[ , c(
    "iid", 
    "gender", 
    "pid", 
    "match", 
    "int_corr", 
    "samerace", 
    "age_o", 
    "race_o",
    "dec_o",
    "age", 
    "field_cd", 
    "race", 
    "imprace", 
    "imprelig", 
    "date", 
    "go_out",
    "sports",
    "tvsports",
    "exercise",
    "dining",
    "museums",
    "art",
    "hiking",
    "gaming",
    "clubbing",
    "reading",
    "tv",
    "theater",
    "movies",
    "concerts",
    "music",
    "shopping",
    "yoga",
    "attr1_1",
    "sinc1_1",
    "intel1_1",
    "fun1_1",
    "amb1_1",
    "shar1_1",
    "attr2_1",
    "sinc2_1",
    "intel2_1",
    "fun2_1",
    "amb2_1",
    "shar2_1",
    "wave")]

waveNum <- which(colnames(sd2)=="wave")
sd2 <- subset(sd2, sd2[ , waveNum] > 9 | sd2[ , waveNum] < 6)

sd2 <- sd2[, -45]
sdrandom <- sd2[sample(nrow(sd2), nrow(sd2)),] #Get a random sample since the data is organized by participant

sdclean <- na.omit(sdrandom) #Remove rows with NA values to create a "clean" set

interestTable <- unique(data.frame(sdclean$iid))
interestTable$iid <- interestTable$sdclean.iid
interestTable$sdclean.iid <- NULL

for (x in interestTable$iid) {
  interestTable$go_out[interestTable$iid == x] <- sdclean$go_out[x == sdclean$iid]
  interestTable$age[interestTable$iid == x] <- sdclean$age[x == sdclean$iid]
  interestTable$field_cd[interestTable$iid == x] <- sdclean$field_cd[x == sdclean$iid]
  interestTable$date[interestTable$iid == x] <- sdclean$date[x == sdclean$iid]
  interestTable$sports[interestTable$iid == x] <- sdclean$sports[x == sdclean$iid]
  interestTable$tvsports[interestTable$iid == x] <- sdclean$tvsports[x == sdclean$iid]
  interestTable$exercise[interestTable$iid == x] <- sdclean$exercise[x == sdclean$iid]
  interestTable$dining[interestTable$iid == x] <- sdclean$dining[x == sdclean$iid]
  interestTable$museums[interestTable$iid == x] <- sdclean$museums[x == sdclean$iid]
  interestTable$art[interestTable$iid == x] <- sdclean$art[x == sdclean$iid]
  interestTable$hiking[interestTable$iid == x] <- sdclean$hiking[x == sdclean$iid]
  interestTable$gaming[interestTable$iid == x] <- sdclean$gaming[x == sdclean$iid]
  interestTable$clubbing[interestTable$iid == x] <- sdclean$clubbing[x == sdclean$iid]
  interestTable$reading[interestTable$iid == x] <- sdclean$reading[x == sdclean$iid]
  interestTable$tv[interestTable$iid == x] <- sdclean$tv[x == sdclean$iid]
  interestTable$theater[interestTable$iid == x] <- sdclean$theater[x == sdclean$iid]
  interestTable$movies[interestTable$iid == x] <- sdclean$movies[x == sdclean$iid]
  interestTable$concerts[interestTable$iid == x] <- sdclean$concerts[x == sdclean$iid]
  interestTable$music[interestTable$iid == x] <- sdclean$music[x == sdclean$iid]
  interestTable$shopping[interestTable$iid == x] <- sdclean$shopping[x == sdclean$iid]
  interestTable$yoga[interestTable$iid == x] <- sdclean$yoga[x == sdclean$iid]
}

