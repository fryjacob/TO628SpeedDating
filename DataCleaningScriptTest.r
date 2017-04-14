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

#fill in blanks for "other" in order to keep all participants
sd2$field_cd <- ifelse(is.na(sd2$field_cd), "Other", sd2$field_cd)
sd2$attr1_1 <- ifelse(is.na(sd2$attr1_1), 0, sd2$attr1_1)
sd2$sinc1_1 <- ifelse(is.na(sd2$sinc1_1), 0, sd2$sinc1_1)
sd2$intel1_1 <- ifelse(is.na(sd2$intel1_1), 0, sd2$intel1_1)
sd2$fun1_1 <- ifelse(is.na(sd2$fun1_1), 0, sd2$fun1_1)
sd2$amb1_1 <- ifelse(is.na(sd2$amb1_1), 0, sd2$amb1_1)
sd2$shar1_1 <- ifelse(is.na(sd2$shar1_1), 0, sd2$shar1_1)
sd2$attr2_1 <- ifelse(is.na(sd2$attr2_1), 0, sd2$attr2_1)
sd2$sinc2_1 <- ifelse(is.na(sd2$sinc2_1), 0, sd2$sinc2_1)
sd2$intel2_1 <- ifelse(is.na(sd2$intel2_1), 0, sd2$intel2_1)
sd2$fun2_1 <- ifelse(is.na(sd2$fun2_1), 0, sd2$fun2_1)
sd2$amb2_1 <- ifelse(is.na(sd2$amb2_1), 0, sd2$amb2_1)
sd2$shar2_1 <- ifelse(is.na(sd2$shar2_1), 0, sd2$shar2_1)

sd2 <- sd2[!is.na(sd2$pid), ] #get rid of the blank places where 118 was supposed to date

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

'%!in%' <- function(x,y)!('%in%'(x,y))


#######After this, it gets iffy.
sd2 <- sd2[, -46] #get rid of wave - why not 45 anymore?

problemIDs <- sd2$iid[which(is.na(sd2$age))]
problemIDs <- c(problemIDs, sd2$iid[which(is.na(sd2$sports))])
problemIDs <- c(problemIDs, sd2$iid[which(is.na(sd2$date))])
problemIDs <- unique(problemIDs)

sd3 <- sd2[sd2$iid %!in% problemIDs, ]
sd3 <- sd3[sd3$pid %!in% problemIDs, ]
problemIDs <- c(problemIDs, 118)
problemIDs <- c(problemIDs, seq(132, 233, 1))
problemIDs <- sort(problemIDs)

for (value in problemIDs) {
  sd3$iid <- ifelse(sd3$iid > value, sd3$iid - 1, sd3$iid)
  sd3$pid <- ifelse(sd3$pid > value, sd3$pid - 1, sd3$pid)
}

#sdrandom <- sd2[sample(nrow(sd2), nrow(sd2)),] #Get a random sample since the data is organized by participan
#sdclean <- na.omit(sd3) #Remove rows with NA values to create a "clean" set


sdReduced <- sd3 #create a dataset so can be used for  indexing appropriately
# sdReduced$iid <- ifelse(sdclean$iid > 117, sdclean$iid - 1, sdclean$iid)
# sdReduced$pid <- ifelse(sdclean$pid > 117, sdclean$pid - 1, sdclean$pid)
# sdReduced$iid <- ifelse(sdclean$iid > 232, sdclean$iid - 114, sdclean$iid)
# sdReduced$pid <- ifelse(sdclean$pid > 232, sdclean$pid - 114, sdclean$pid)


interestTable <- unique(data.frame(sdReduced$iid))
interestTable$iid <- interestTable$sdReduced.iid
interestTable$sdReduced.iid <- NULL

for (x in interestTable$iid) {
  interestTable$go_out[interestTable$iid == x] <- sdReduced$go_out[x == sdReduced$iid]
  interestTable$age[interestTable$iid == x] <- sdReduced$age[x == sdReduced$iid]
  interestTable$field_cd[interestTable$iid == x] <- sdReduced$field_cd[x == sdReduced$iid]
  interestTable$date[interestTable$iid == x] <- sdReduced$date[x == sdReduced$iid]
  interestTable$sports[interestTable$iid == x] <- sdReduced$sports[x == sdReduced$iid]
  interestTable$tvsports[interestTable$iid == x] <- sdReduced$tvsports[x == sdReduced$iid]
  interestTable$exercise[interestTable$iid == x] <- sdReduced$exercise[x == sdReduced$iid]
  interestTable$dining[interestTable$iid == x] <- sdReduced$dining[x == sdReduced$iid]
  interestTable$museums[interestTable$iid == x] <- sdReduced$museums[x == sdReduced$iid]
  interestTable$art[interestTable$iid == x] <- sdReduced$art[x == sdReduced$iid]
  interestTable$hiking[interestTable$iid == x] <- sdReduced$hiking[x == sdReduced$iid]
  interestTable$gaming[interestTable$iid == x] <- sdReduced$gaming[x == sdReduced$iid]
  interestTable$clubbing[interestTable$iid == x] <- sdReduced$clubbing[x == sdReduced$iid]
  interestTable$reading[interestTable$iid == x] <- sdReduced$reading[x == sdReduced$iid]
  interestTable$tv[interestTable$iid == x] <- sdReduced$tv[x == sdReduced$iid]
  interestTable$theater[interestTable$iid == x] <- sdReduced$theater[x == sdReduced$iid]
  interestTable$movies[interestTable$iid == x] <- sdReduced$movies[x == sdReduced$iid]
  interestTable$concerts[interestTable$iid == x] <- sdReduced$concerts[x == sdReduced$iid]
  interestTable$music[interestTable$iid == x] <- sdReduced$music[x == sdReduced$iid]
  interestTable$shopping[interestTable$iid == x] <- sdReduced$shopping[x == sdReduced$iid]
  interestTable$yoga[interestTable$iid == x] <- sdReduced$yoga[x == sdReduced$iid]
}

for (y in sdReduced$pid){
  sdReduced$pGoOut[y == sdReduced$pid] <- interestTable$go_out[interestTable$iid == y]
  sdReduced$pAge[y == sdReduced$pid] <- interestTable$age[interestTable$iid == y]
  sdReduced$pField_cd[y == sdReduced$pid] <- interestTable$field_cd[interestTable$iid == y]
  sdReduced$pDate[y == sdReduced$pid] <- interestTable$date[interestTable$iid == y]
  sdReduced$pSports[y == sdReduced$pid] <- interestTable$sports[interestTable$iid == y]
  sdReduced$pSportsTV[y == sdReduced$pid] <- interestTable$tvsports[interestTable$iid == y]
  sdReduced$pExercise[y == sdReduced$pid] <- interestTable$exercise[interestTable$iid == y]
  sdReduced$pDining[y == sdReduced$pid] <- interestTable$dining[interestTable$iid == y]
  sdReduced$pMuseums[y == sdReduced$pid] <- interestTable$museums[interestTable$iid == y]
  sdReduced$pArt[y == sdReduced$pid] <- interestTable$art[interestTable$iid == y]
  sdReduced$pHiking[y == sdReduced$pid] <- interestTable$hiking[interestTable$iid == y]
  sdReduced$pGaming[y == sdReduced$pid] <- interestTable$gaming[interestTable$iid == y]
  sdReduced$pClubbing[y == sdReduced$pid] <- interestTable$clubbing[interestTable$iid == y]
  sdReduced$pReading[y == sdReduced$pid] <- interestTable$reading[interestTable$iid == y]
  sdReduced$pTV[y == sdReduced$pid] <- interestTable$tv[interestTable$iid == y]
  sdReduced$pTheater[y == sdReduced$pid] <- interestTable$theater[interestTable$iid == y]
  sdReduced$pMovies[y == sdReduced$pid] <- interestTable$movies[interestTable$iid == y]
  sdReduced$pConcerts[y == sdReduced$pid] <- interestTable$concerts[interestTable$iid == y]
  sdReduced$pMusic[y == sdReduced$pid] <- interestTable$music[interestTable$iid == y]
  sdReduced$pShopping[y == sdReduced$pid] <- interestTable$shopping[interestTable$iid == y]
  sdReduced$pYoga[y == sdReduced$pid] <- interestTable$yoga[interestTable$iid == y]
}

sdReduced$pDate <- as.factor(sdReduced$pDate)
levels(sdReduced$pDate) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
sdReduced$pGoOut <- as.factor(sdReduced$pGoOut)
levels(sdReduced$pGoOut) <- c("SVRL/Week","2/Week","1/Week","2/Month", "1/Month", "SVRL/Year", "AlmostNever")
#sdReduced$pField_cd <- as.factor(sdReduced$pField_cd)
#levels(sdReduced$pField_cd) <- c("Law","Math","SocScie/Psych", "MedSci", "Engineering", "English", "History", "Business", "Education", "Bio","SocialWork","Undergrad", "PoliSci", "Film","FineArts","Lang","Architecture","Other")

#not doing one for Field_CD delta
sdReduced$pDeltaGoOut <- as.numeric(sdReduced$go_out) - as.numeric(sdReduced$pGoOut)
sdReduced$pDeltaDate <- as.numeric(sdReduced$date) - as.numeric(sdReduced$pDate)
sdReduced$pDeltaAge <- sdReduced$age - sdReduced$pAge
sdReduced$pDeltaSports <- sdReduced$sports - sdReduced$pSports
sdReduced$pDeltaTVSports <- sdReduced$tvsports - sdReduced$pSportsTV
sdReduced$pDeltaExercise <- sdReduced$exercise - sdReduced$pExercise
sdReduced$pDeltaDining <- sdReduced$dining - sdReduced$pDining
sdReduced$pDeltaMuseums <- sdReduced$museums - sdReduced$pMuseums
sdReduced$pDeltaArt <- sdReduced$art - sdReduced$pArt
sdReduced$pDeltaHiking <- sdReduced$hiking - sdReduced$pHiking
sdReduced$pDeltaGaming <- sdReduced$gaming - sdReduced$pGaming
sdReduced$pDeltaClubbing <- sdReduced$clubbing - sdReduced$pClubbing
sdReduced$pDeltaReading <- sdReduced$reading - sdReduced$pReading
sdReduced$pDeltaTV <- sdReduced$tv - sdReduced$pTV
sdReduced$pDeltaTheater <- sdReduced$theater - sdReduced$pTheater
sdReduced$pDeltaConcerts <- sdReduced$concerts - sdReduced$pConcerts
sdReduced$pDeltaMusic <- sdReduced$music - sdReduced$pMusic
sdReduced$pDeltaMovies <- sdReduced$movies - sdReduced$pMovies
sdReduced$pDeltaYoga <- sdReduced$yoga - sdReduced$pYoga
sdReduced$pDeltaShopping <- sdReduced$shopping - sdReduced$pShopping


