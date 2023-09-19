library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)
#library(naniar)

racepatch <-
  list.files(
    path = "C:/Users/maxch/Git/RACE/data/Patch",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfrace <- lapply(racepatch , read_csv)
names(listdfrace) <- tools::file_path_sans_ext(basename(racepatch))

#remise en forme alban

listdfrace$AlbanLegallA <-
  listdfrace$AlbanLegallA %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)
listdfrace$AlbanLegallA <- listdfrace$AlbanLegallA[-c(1:2),-2]
tps <- 1:11105
listdfrace$AlbanLegallA <- cbind(listdfrace$AlbanLegallA , tps)
colnames(listdfrace$AlbanLegallA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$AlbanLegallA <-
  listdfrace$AlbanLegallA %>% relocate(temps , hr , distance , altitude, speed)

#Mise en forme Antonin

listdfrace$AntoninBordasB <-
  listdfrace$AntoninBordasB %>% select(temps, hr , distance, altitude , speed)

listdfrace$AntoninBordasB$speed <- listdfrace$AntoninBordasB$speed * 3.6

#Mise en forme Bastien M

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA %>% filter(accumulated_power == "accumulated_power")

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA[,1:6]

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA %>% drop_na()

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA %>% mutate(temps = tps - tps[1])

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA %>% select(temps, hr, speed...3 , altitude...4 , distance)

colnames(listdfrace$BastienMarsanA) <-
  c("temps", "hr" , "speed" , "altitude" , "distance")

listdfrace$BastienMarsanA <-
  listdfrace$BastienMarsanA %>% relocate(temps , hr , distance, altitude, speed)

listdfrace$BastienMarsanA$speed <- listdfrace$BastienMarsanA$speed * 3.6

#mise en forme bastien R

listdfrace$BastienRiegerB <-
  listdfrace$BastienRiegerB %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)

listdfrace$BastienRiegerB <- listdfrace$BastienRiegerB[-c(1:2),-2]
tps <- 1:9146
listdfrace$BastienRiegerB <- cbind(listdfrace$BastienRiegerB , tps)
colnames(listdfrace$BastienRiegerB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$BastienRiegerB <-
  listdfrace$BastienRiegerB %>% relocate(temps , hr , distance , altitude, speed)

#remise en forme brice

listdfrace$BriceAlmuniaA <-
  listdfrace$BriceAlmuniaA %>% select(`Start time`,
                                       Sport ,
                                       Date ,
                                       `Average heart rate (bpm)`,
                                       `Max speed (km/h)`)

listdfrace$BriceAlmuniaA <- listdfrace$BriceAlmuniaA[-c(1:2),-2]
tps <- 1:12792
listdfrace$BriceAlmuniaA <- cbind(listdfrace$BriceAlmuniaA , tps)
colnames(listdfrace$BriceAlmuniaA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$BriceAlmuniaA <-
  listdfrace$BriceAlmuniaA %>% relocate(temps , hr , distance , altitude, speed)

#Remise en forme Gabin

listdfrace$GabinAgeronA <-
  listdfrace$GabinAgeronA %>% select(`Start time`,
                                      Sport ,
                                      Date ,
                                      `Average heart rate (bpm)`,
                                      `Max speed (km/h)`)

listdfrace$GabinAgeronA <- listdfrace$GabinAgeronA[-c(1:2),-2]
tps <- 1:8145
listdfrace$GabinAgeronA <- cbind(listdfrace$GabinAgeronA , tps)
colnames(listdfrace$GabinAgeronA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$GabinAgeronA <-
  listdfrace$GabinAgeronA %>% relocate(temps , hr , distance , altitude, speed)

#Remise en forme Gaetan

# listdfrace$GaetanSteenbergenB <-
#   listdfrace$GaetanSteenbergenB %>% select(`Start time`,
#                                      Sport ,
#                                      Date ,
#                                      `Average heart rate (bpm)`,
#                                      `Max speed (km/h)`)
#
# listdfrace$GaetanSteenbergenB <- listdfrace$GaetanSteenbergenB[-c(1:2),-2]
# tps <- 1:10200
# listdfrace$GaetanSteenbergenB <- cbind(listdfrace$GaetanSteenbergenB , tps)
# colnames(listdfrace$GaetanSteenbergenB) <-
#   c("speed" , "hr" , "altitude" , "distance" , "temps")
#
# listdfrace$GaetanSteenbergenB <-
#   listdfrace$GaetanSteenbergenB %>% relocate(temps , hr , distance , altitude, speed)

#Remise en forme Jules

listdfrace$JulesSavignacB <-
  listdfrace$JulesSavignacB %>% select(`ns1:AltitudeMeters`,
                                       `ns1:DistanceMeters3`,
                                       `ns1:Value4`,
                                       `ns2:Speed` , `ns1:Time`)


listdfrace$JulesSavignacB <-
  mutate(
    listdfrace$JulesSavignacB,
    tps = gsub("[: -]", "" ,  listdfrace$JulesSavignacB$`ns1:Time` , perl = TRUE)
  )
listdfrace$JulesSavignacB$tps <- as.numeric(listdfrace$JulesSavignacB$tps)

listdfrace$JulesSavignacB <-
  listdfrace$JulesSavignacB %>% mutate(temps = tps - tps[1])

listdfrace$JulesSavignacB <-
  listdfrace$JulesSavignacB %>%  select(temps ,
                                        `ns1:AltitudeMeters` ,
                                        `ns1:DistanceMeters3`,
                                        `ns1:Value4` ,
                                        `ns2:Speed`)

colnames(listdfrace$JulesSavignacB) <-
  c("temps" , "altitude" , "distance", "hr" , "speed")

listdfrace$JulesSavignacB <-
  listdfrace$JulesSavignacB %>% relocate(temps , hr , distance , altitude, speed)

listdfrace$JulesSavignacB$speed <- listdfrace$JulesSavignacB$speed * 3.6

# # Remise en forme Maris
#
# listdfrace$MarisAllieB <-
#   listdfrace$MarisAllieB %>% select(`Start time`,
#                                            Sport ,
#                                            Date ,
#                                            `Average heart rate (bpm)`,
#                                            `Max speed (km/h)`)
#
# listdfrace$MarisAllieB <- listdfrace$MarisAllieB[-c(1:2),-2]
# tps <- 1:12603
# listdfrace$MarisAllieB <- cbind(listdfrace$MarisAllieB , tps)
# colnames(listdfrace$MarisAllieB) <-
#   c("speed" , "hr" , "altitude" , "distance" , "temps")
#
# listdfrace$MarisAllieB <-
#   listdfrace$MarisAllieB %>% relocate(temps , hr , distance , altitude, speed)

# Remise en forme Maxence

# listdfrace$MaxenceFontesB <- listdfrace$MaxenceFontesB[, 1:23]
#
# speedfontes <- listdfrace$MaxenceFontesB[,1]
# speedfontes <- speedfontes %>% drop_na()
# colnames(speedfontes) <- "vitesse"
# speedfontes <- as.data.frame(as.numeric(unlist(speedfontes)))
# colnames(speedfontes) <- "speed"
# speedfontes <- filter(speedfontes , speed < 6)
#
# listdfrace$MaxenceFontesB <- dplyr::select_if(listdfrace$MaxenceFontesB, is.numeric)
# listdfrace$MaxenceFontesB <- listdfrace$MaxenceFontesB %>% drop_na()
# listdfrace$MaxenceFontesB <- listdfrace$MaxenceFontesB[, -c(1,4,5)]
#
# colnames(listdfrace$MaxenceFontesB) <- c("distance" , "altitude" , "hr")
#
# listdfrace$MaxenceFontesB <- listdfrace$MaxenceFontesB %>% filter(hr < 190 & hr > 55)

#mise en forme douchet

listdfrace$MelvindouchetB <- listdfrace$MelvindouchetB[, c(1,2,5,8,14)]
colnames(listdfrace$MelvindouchetB) <- c("temps" , "distance" , "hr" , "altitude" , "speed")

listdfrace$MelvindouchetB <-
  listdfrace$MelvindouchetB %>% relocate(temps , hr , distance , altitude , speed)

listdfrace$MelvindouchetB <- listdfrace$MelvindouchetB %>% filter(hr < 200) %>% filter(distance < 18000)

listdfrace$MelvindouchetB$speed <- listdfrace$MelvindouchetB$speed * 3.6


# listdfrace$MelvinDouchetBpart2 <-
#   listdfrace$MelvinDouchetBpart2 %>% select(`Start time`,
#                                      Sport ,
#                                      Date ,
#                                      `Average heart rate (bpm)`,
#                                      `Max speed (km/h)`)
# listdfrace$MelvinDouchetBpart2 <- listdfrace$MelvinDouchetBpart2[-c(1:2),-2]
# tps <- 1:6160
# listdfrace$MelvinDouchetBpart2 <- cbind(listdfrace$MelvinDouchetBpart2 , tps)
# colnames(listdfrace$MelvinDouchetBpart2) <-
#   c("speed" , "hr" , "altitude" , "distance" , "temps")
#
# listdfrace$MelvinDouchetBpart2 <-
#   listdfrace$MelvinDouchetBpart2 %>% relocate(temps , hr , distance , altitude, speed)

#listdfrace$MelvindouchetB <- listdfrace %>%  rbind(MelvindouchetB , MelvinDouchetBpart2)

# mise en forme PE

listdfrace$PierreEmanuelleNaulletA <-
  listdfrace$PierreEmanuelleNaulletA %>% select(temps , fc , distance , vitesse, altitude)

colnames(listdfrace$PierreEmanuelleNaulletA) <-
  c("temps" , "hr" , "distance" , "speed", "altitude")

listdfrace$PierreEmanuelleNaulletA <-
  listdfrace$PierreEmanuelleNaulletA %>% relocate(temps , hr, distance, altitude, speed)

listdfrace$PierreEmanuelleNaulletA$speed <- listdfrace$PierreEmanuelleNaulletA$speed * 3.6

#mise en forme remi

listdfrace$RemiFalconA <-
  listdfrace$RemiFalconA %>% select(`Start time`,
                                            Sport ,
                                            Date ,
                                            `Average heart rate (bpm)`,
                                            `Max speed (km/h)`)
listdfrace$RemiFalconA <- listdfrace$RemiFalconA[-c(1:2),-2]
tps <- 1:10092
listdfrace$RemiFalconA <- cbind(listdfrace$RemiFalconA , tps)
colnames(listdfrace$RemiFalconA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$RemiFalconA <-
  listdfrace$RemiFalconA %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme romain

# listdfrace$RomainOrioA <- listdfrace$RomainOrioA[,c(1,4,10,28)]
#
# listdfrace$RomainOrioA[1] <- as.numeric(unlist(listdfrace$RomainOrioA[1]))
# listdfrace$RomainOrioA[2] <- as.numeric(unlist(listdfrace$RomainOrioA[2]))
#
# colnames(listdfrace$RomainOrioA) <- c("altitude","speed","distance","hr")
#
# tempsromain <- listdfrace$RomainOrioA %>% filter(distance > 2000)
# tempsromain <- tempsromain[,1]
# tempsromain <- tempsromain %>% mutate(temps = altitude - altitude[1])
# tempsromain <- tempsromain[,2]
#
# listdfrace$RomainOrioA$altitude <-
#   ifelse(listdfrace$RomainOrioA$altitude > 2100,
#          NA,
#          listdfrace$RomainOrioA$altitude)
#
# listdfrace$RomainOrioA$speed <-
#   ifelse(listdfrace$RomainOrioA$speed > 6,
#          NA,
#          listdfrace$RomainOrioA$speed)
#
# listdfrace$RomainOrioA <-
#   listdfrace$RomainOrioA %>% filter(distance > 2 | is.na(distance))
#
# listdfrace$RomainOrioA <-
#   listdfrace$RomainOrioA %>% relocate(hr, distance, altitude, speed)
#
# listdfrace$RomainOrioA$speed <- listdfrace$RomainOrioA$speed *3.6

#mise en forme Sacha

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB[,1:20]

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB %>% drop_na()

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB[,c(1,4,7,10,16)]

colnames(listdfrace$SachaPerrierB) <- c("tps" , "distance" ,"hr","altitude" ,"speed")

listdfrace$SachaPerrierB[1] <- as.numeric(unlist(listdfrace$SachaPerrierB[1]))

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB %>% mutate(temps = tps - tps[1])

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB[, -1]

listdfrace$SachaPerrierB <- listdfrace$SachaPerrierB %>% filter(distance < 30000)

listdfrace$SachaPerrierB <-
  listdfrace$SachaPerrierB %>% relocate(temps, hr, distance, altitude, speed)

listdfrace$SachaPerrierB$speed <- listdfrace$SachaPerrierB$speed *3.6

#mise en forme simon

listdfrace$SimonLopezA <-
  listdfrace$SimonLopezA %>% select(`Start time`,
                                    Sport ,
                                    Date ,
                                    `Average heart rate (bpm)`,
                                    `Max speed (km/h)`)
listdfrace$SimonLopezA <- listdfrace$SimonLopezA[-c(1:2),-2]
tps <- 1:8350
listdfrace$SimonLopezA <- cbind(listdfrace$SimonLopezA , tps)
colnames(listdfrace$SimonLopezA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$SimonLopezA <-
  listdfrace$SimonLopezA %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme tommy

listdfrace$TommyMaurinB <-
  listdfrace$TommyMaurinB %>% select(`Start time`,
                                    Sport ,
                                    Date ,
                                    `Average heart rate (bpm)`,
                                    `Max speed (km/h)`)
listdfrace$TommyMaurinB <- listdfrace$TommyMaurinB[-c(1:2),-2]
tps <- 1:5022
listdfrace$TommyMaurinB <- cbind(listdfrace$TommyMaurinB , tps)
colnames(listdfrace$TommyMaurinB) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$TommyMaurinB <-
  listdfrace$TommyMaurinB %>% relocate(temps , hr , distance , altitude, speed)

#mise en forme valerian

listdfrace$ValerianPelissierA <-
  listdfrace$ValerianPelissierA %>% select(`Start time`,
                                     Sport ,
                                     Date ,
                                     `Average heart rate (bpm)`,
                                     `Max speed (km/h)`)
listdfrace$ValerianPelissierA <- listdfrace$ValerianPelissierA[-c(1:2),-2]
tps <- 1:13274
listdfrace$ValerianPelissierA <- cbind(listdfrace$ValerianPelissierA , tps)
colnames(listdfrace$ValerianPelissierA) <-
  c("speed" , "hr" , "altitude" , "distance" , "temps")

listdfrace$ValerianPelissierA <-
  listdfrace$ValerianPelissierA %>% relocate(temps , hr , distance , altitude, speed)
