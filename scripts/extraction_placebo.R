library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)
#library(naniar)

raceplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/RACE/data/Placebo",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfracepb <- lapply(raceplacebo , read_csv)
names(listdfracepb) <- tools::file_path_sans_ext(basename(raceplacebo))

#remise en forme alban

listdfracepb$AlbanLegallB <-
  listdfracepb$AlbanLegallB %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)
listdfracepb$AlbanLegallB <- listdfracepb$AlbanLegallB[-c(1:2),-2]
tps <- 1:10319
listdfracepb$AlbanLegallB <- cbind(listdfracepb$AlbanLegallB , tps)
colnames(listdfracepb$AlbanLegallB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$AlbanLegallB <-
  listdfracepb$AlbanLegallB %>% relocate(temps , hr , distance , altitude, speed)

#Mise en forme Antonin

listdfracepb$AntoninBordasA <-
  listdfracepb$AntoninBordasA %>% select(temps, fc , distance, altitude , vitesse)

colnames(listdfracepb$AntoninBordasA) <- c("temps","hr" ,"distance","altitude", "speed")

listdfracepb$AntoninBordasA <-
  listdfracepb$AntoninBordasA %>% relocate(temps, hr, distance, altitude, speed)

listdfracepb$AntoninBordasA$speed <- listdfracepb$AntoninBordasA$speed * 3.6

#Mise en forme Bastien M

listdfracepb$BastienMarsanB <-
  listdfracepb$BastienMarsanB[,c(2,4,7,16)]

listdfracepb$BastienMarsanB <-
  listdfracepb$BastienMarsanB %>% drop_na()

colnames(listdfracepb$BastienMarsanB) <-
  c("distance" , "altitude" , "speed", "hr")

listdfracepb$BastienMarsanB <-
  listdfracepb$BastienMarsanB %>% filter(distance > 1 & distance < 25000)

listdfracepb$BastienMarsanB <-
  listdfracepb$BastienMarsanB %>% relocate(hr, distance, altitude, speed)

listdfracepb$BastienMarsanB$speed <- listdfracepb$BastienMarsanB$speed * 3.6

#mise en forme bastien R

listdfracepb$BastienRiegerA <-
  listdfracepb$BastienRiegerA %>% select(`Start time`,
                                       Sport ,
                                       Date ,
                                       `Average heart rate (bpm)`,
                                       `Max speed (km/h)`)

listdfracepb$BastienRiegerA <- listdfracepb$BastienRiegerA[-c(1:2),-2]
tps <- 1:8187
listdfracepb$BastienRiegerA <- cbind(listdfracepb$BastienRiegerA , tps)
colnames(listdfracepb$BastienRiegerA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$BastienRiegerA <-
  listdfracepb$BastienRiegerA %>% relocate(temps , hr , distance , altitude, speed)

#remise en forme brice

listdfracepb$BriceAlmuniaB <-
  listdfracepb$BriceAlmuniaB %>% select(`Start time`,
                                      Sport ,
                                      Date ,
                                      `Average heart rate (bpm)`,
                                      `Max speed (km/h)`)

listdfracepb$BriceAlmuniaB <- listdfracepb$BriceAlmuniaB[-c(1:2),-2]
tps <- 1:9613
listdfracepb$BriceAlmuniaB <- cbind(listdfracepb$BriceAlmuniaB , tps)
colnames(listdfracepb$BriceAlmuniaB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$BriceAlmuniaB <-
  listdfracepb$BriceAlmuniaB %>% relocate(temps , hr , distance , altitude, speed)

#Remise en forme Gabin

listdfracepb$GabinAgeronB <-
  listdfracepb$GabinAgeronB %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)

listdfracepb$GabinAgeronB <- listdfracepb$GabinAgeronB[-c(1:2),-2]
tps <- 1:8464
listdfracepb$GabinAgeronB <- cbind(listdfracepb$GabinAgeronB , tps)
colnames(listdfracepb$GabinAgeronB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$GabinAgeronB <-
  listdfracepb$GabinAgeronB %>% relocate(temps , hr , distance , altitude, speed)

#Remise en forme Gaetan

# listdfracepb$GaetanSteenbergenA <- listdfracepb$GaetanSteenbergenA[,c(2,5,11)]
#
# colnames(listdfracepb$GaetanSteenbergenA) <-
#   c("distance" , "altitude", "speed")
#
# listdfracepb$GaetanSteenbergenA <-
#   listdfracepb$GaetanSteenbergenA %>% filter(distance < 25000)
# listdfracepb$GaetanSteenbergenA <-
#   listdfracepb$GaetanSteenbergenA %>% filter(altitude > 1800)
#
# listdfracepb$GaetanSteenbergenA <-
#   listdfracepb$GaetanSteenbergenA %>% relocate(distance , altitude, speed)

#Remise en forme Jules


listdfracepb$JulesSavignacA <- listdfracepb$JulesSavignacA[,2:5]

colnames(listdfracepb$JulesSavignacA) <- c("hr","speed","altitude","distance")

listdfracepb$JulesSavignacA <-
  listdfracepb$JulesSavignacA %>% relocate( hr , distance , altitude, speed)

listdfracepb$JulesSavignacA$speed <- listdfracepb$JulesSavignacA$speed * 3.6

# Remise en forme Maxence
#
# listdfracepb$MaxenceFontesA <- listdfracepb$MaxenceFontesA[, 1:23]
#
# speedfontespb <- listdfracepb$MaxenceFontesA[,2]
# speedfontespb <- speedfontespb %>% drop_na()
# colnames(speedfontespb) <- "vitesse"
# speedfontespb <- as.data.frame(as.numeric(unlist(speedfontespb)))
# colnames(speedfontespb) <- "speed"
# speedfontespb <- filter(speedfontespb , speed < 6 & speed > 1.2)
#
# listdfracepb$MaxenceFontesA <- dplyr::select_if(listdfracepb$MaxenceFontesA, is.numeric)
# listdfracepb$MaxenceFontesA <- listdfracepb$MaxenceFontesA %>% drop_na()
# listdfracepb$MaxenceFontesA <- listdfracepb$MaxenceFontesA[, c(2,3,5)]
#
# colnames(listdfracepb$MaxenceFontesA) <- c("distance" , "altitude" , "hr")
#
# listdfracepb$MaxenceFontesA <- listdfracepb$MaxenceFontesA %>% filter(hr < 190 & hr > 55)
#
# listdfracepb$MaxenceFontesA <- listdfracepb$MaxenceFontesA %>% relocate(hr, distance,altitude)

#mise en forme douchet

listdfracepb$MelvinDouchetA <- listdfracepb$MelvinDouchetA[, c(5,14, 17,20,26)]
colnames(listdfracepb$MelvinDouchetA) <- c("tps" , "distance" , "hr" , "altitude" , "speed")

listdfracepb$MelvinDouchetA <-
  listdfracepb$MelvinDouchetA %>% drop_na()

listdfracepb$MelvinDouchetA$tps <- as.numeric(unlist(listdfracepb$MelvinDouchetA$tps))

listdfracepb$MelvinDouchetA <- listdfracepb$MelvinDouchetA %>% mutate(temps = tps - tps[1])

listdfracepb$MelvinDouchetA <- listdfracepb$MelvinDouchetA[,-1]

listdfracepb$MelvinDouchetA <-
  listdfracepb$MelvinDouchetA %>% relocate(temps, hr, distance, altitude, speed)

listdfracepb$MelvinDouchetA$speed <- listdfracepb$MelvinDouchetA$speed * 3.6

# mise en forme PE

listdfracepb$PierreEmanuelleNaulletB <-
  listdfracepb$PierreEmanuelleNaulletB %>% select(temps , fc , distance , vitesse, altitude)

colnames(listdfracepb$PierreEmanuelleNaulletB) <-
  c("temps" , "hr" , "distance" , "speed", "altitude")

listdfracepb$PierreEmanuelleNaulletB <-
  listdfracepb$PierreEmanuelleNaulletB %>% relocate(temps , hr, distance, altitude, speed)

listdfracepb$PierreEmanuelleNaulletB$speed <- listdfracepb$PierreEmanuelleNaulletB$speed * 3.6

#mise en forme remi

listdfracepb$RemiFalconB <-
  listdfracepb$RemiFalconB %>% select(`Start time`,
                                    Sport ,
                                    Date ,
                                    `Average heart rate (bpm)`,
                                    `Max speed (km/h)`)
listdfracepb$RemiFalconB <- listdfracepb$RemiFalconB[-c(1:2),-2]
tps <- 1:9629
listdfracepb$RemiFalconB <- cbind(listdfracepb$RemiFalconB , tps)
colnames(listdfracepb$RemiFalconB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$RemiFalconB <-
  listdfracepb$RemiFalconB %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme romain

# listdfracepb$RomainOrioB <- listdfracepb$RomainOrioB[,c(1,4,10,28)]
#
# listdfracepb$RomainOrioB[1] <- as.numeric(unlist(listdfracepb$RomainOrioB[1]))
# listdfracepb$RomainOrioB[2] <- as.numeric(unlist(listdfracepb$RomainOrioB[2]))
#
# colnames(listdfracepb$RomainOrioB) <- c("altitude","speed","distance","hr")
#
# tempsromainpb <- listdfracepb$RomainOrioB %>% filter(distance > 2000)
# tempsromainpb <- tempsromainpb[,1]
# tempsromainpb <- tempsromainpb %>% mutate(temps = altitude - altitude[1])
# tempsromainpb <- tempsromainpb[,2]
#
# listdfracepb$RomainOrioB$altitude <-
#   ifelse(listdfracepb$RomainOrioB$altitude > 2100,
#           NA,
#           listdfracepb$RomainOrioB$altitude)
#
# listdfracepb$RomainOrioB$speed <-
#   ifelse(listdfracepb$RomainOrioB$speed > 6,
#          NA,
#          listdfracepb$RomainOrioB$speed)
#
# listdfracepb$RomainOrioB <-
#   listdfracepb$RomainOrioB %>% filter(distance > 2 | is.na(distance))
#
# listdfracepb$RomainOrioB <-
#   listdfracepb$RomainOrioB %>% relocate(hr, distance, altitude, speed)
#
# listdfracepb$RomainOrioB$speed <- listdfracepb$RomainOrioB$speed *3.6

#mise en forme Sacha

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA[,1:25]

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA %>% drop_na()

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA[,c(5,8,11,14,20)]

colnames(listdfracepb$SachaPerrierA) <- c("tps" , "distance" ,"hr","altitude" ,"speed")

listdfracepb$SachaPerrierA[1] <- as.numeric(unlist(listdfracepb$SachaPerrierA[1]))

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA %>% mutate(temps = tps - tps[1])

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA[, -1]

listdfracepb$SachaPerrierA <- listdfracepb$SachaPerrierA %>% filter(distance < 30000)

listdfracepb$SachaPerrierA <-
  listdfracepb$SachaPerrierA %>% relocate(temps, hr, distance, altitude, speed)

listdfracepb$SachaPerrierA$speed <- listdfracepb$SachaPerrierA$speed *3.6

#mise en forme simon

listdfracepb$SimonLopezB <-
  listdfracepb$SimonLopezB %>% select(`Start time`,
                                    Sport ,
                                    Date ,
                                    `Average heart rate (bpm)`,
                                    `Max speed (km/h)`)
listdfracepb$SimonLopezB <- listdfracepb$SimonLopezB[-c(1:2),-2]
tps <- 1:8643
listdfracepb$SimonLopezB <- cbind(listdfracepb$SimonLopezB , tps)
colnames(listdfracepb$SimonLopezB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$SimonLopezB <-
  listdfracepb$SimonLopezB %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme tommy

listdfracepb$TommyMaurinA <-
  listdfracepb$TommyMaurinA %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)
listdfracepb$TommyMaurinA <- listdfracepb$TommyMaurinA[-c(1:2),-2]
tps <- 1:4922
listdfracepb$TommyMaurinA <- cbind(listdfracepb$TommyMaurinA , tps)
colnames(listdfracepb$TommyMaurinA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$TommyMaurinA <-
  listdfracepb$TommyMaurinA %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme valerian

listdfracepb$ValerianPelissierB <-
  listdfracepb$ValerianPelissierB %>% select(`Start time`,
                                           Sport ,
                                           Date ,
                                           `Average heart rate (bpm)`,
                                           `Max speed (km/h)`)
listdfracepb$ValerianPelissierB <- listdfracepb$ValerianPelissierB[-c(1:2),-2]
tps <- 1:14720
listdfracepb$ValerianPelissierB <- cbind(listdfracepb$ValerianPelissierB , tps)
colnames(listdfracepb$ValerianPelissierB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfracepb$ValerianPelissierB <-
  listdfracepb$ValerianPelissierB %>% relocate(temps , hr , distance , altitude, speed)
