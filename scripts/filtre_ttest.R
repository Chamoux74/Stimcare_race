library(ggplot2)
library(rstatix)
library(ggpubr)
library(dplyr)
library(pander)
library(kableExtra)
library(CUFF)

#selection exercise activity
#patch

listdfrace <- lapply(listdfrace, function(x) {x["speed"] <- lapply(x["speed"], as.numeric);x})
listdfrace <- lapply(listdfrace, function(x) {x["hr"] <- lapply(x["hr"], as.numeric);x})

act <- function (bob) {filter(bob , speed > 7.5)}

listtest <- lapply(listdfrace, act)

vit <- function(bob) {mean(bob$speed , na.rm = TRUE)}

dfspeed <- as.data.frame(lapply(listtest, vit))

fc <- function (bob) {mean(bob$hr, na.rm = TRUE)}
dfhr <- as.data.frame(lapply(listtest, fc))

d <- function (bob) {max(bob$distance, na.rm = TRUE)}
dfdist <- as.data.frame(lapply(listtest, d))

dfpatch <- rbind(dfdist, dfhr, dfspeed)
dfpatch <- as.data.frame(t(dfpatch))
patch <- "patch"
pourcentageVO2 <- c(76.9, 66.6, 82.2 , 66.6, 67.7, 69.1, 80,44.1, 76.4, 75.4, 82.6 , 70.3,87.3, 78.7)
sujet <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
dfpatch <- cbind(dfpatch, patch, sujet , pourcentageVO2)
colnames(dfpatch) <- c("distance", "hr", "speed", "condition" , "sujet" , "pourcentageVO2")

pourcentageVO2pb <- c(80, 75.3 , 77.4, 68.11, 67.6 , 66.6 ,75.4 ,79.4, 81.8,74.8, 75.4 , 64.8, 90.1,73.7)

#placebo

listdfracepb <- lapply(listdfracepb, function(x) {x["speed"] <- lapply(x["speed"], as.numeric);x})
listdfracepb <- lapply(listdfracepb, function(x) {x["hr"] <- lapply(x["hr"], as.numeric);x})

listtest1 <- lapply(listdfracepb, act)
listtest1$BastienMarsanB <- filter(listtest1$BastienMarsanB, speed < 22)
listtest1$MelvinDouchetA <- filter(listtest1$MelvinDouchetA, speed < 22)

vit <- function(bob) {mean(bob$speed , na.rm = TRUE)}

dfspeedpb <- as.data.frame(lapply(listtest1, vit))

fc <- function (bob) {mean(bob$hr, na.rm = TRUE)}
dfhrpb <- as.data.frame(lapply(listtest1, fc))

d <- function (bob) {max(bob$distance, na.rm = TRUE)}
dfdistpb <- as.data.frame(lapply(listtest1, d))

dfplacebo <- rbind(dfdistpb, dfhrpb, dfspeedpb)
dfplacebo <- as.data.frame(t(dfplacebo))
placebo <- "placebo"
dfplacebo <- cbind(dfplacebo, placebo , sujet, pourcentageVO2pb)
colnames(dfplacebo) <- c("distance", "hr", "speed", "condition" , "sujet" , "pourcentageVO2")

#dataframe final

dffinal <- rbind(dfpatch, dfplacebo)
dffinal <- mutate(dffinal, temps = (distance/1000)/(speed/60))

# ttest

dffinal %>% group_by(condition) %>% shapiro_test(pourcentageVO2)
ggqqplot(dffinal, "pourcentageVO2") +
  facet_grid(rows = vars(condition))

meantsd <- function (x, digits = c(1, 1))
{
  if (length(digits) %in% 1)
    digits <- rep(digits, 1)
  format_str <- sprintf("%%.%df ± %%.%df", digits[1], digits[2])
  sprintf(format_str, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

summary_data <- dffinal %>%
  group_by(condition) %>%
  summarise("Pourcentage VO2max-race" = meantsd(pourcentageVO2, digits = c(1,1)),
    "Race HR (Bpm)" = meantsd(hr, digits = c(1,1)),
    "Race duration (min)" = meantsd(temps, digits = c(1,1)))

summary_data <- t(summary_data)

summary_data <- summary_data[-1, ]

p_values <- tibble(
  pourcentageVO2_p_value = t_test(dffinal, pourcentageVO2 ~ condition, paired = T)$p,
  hr_p_value = t_test(dffinal, hr ~ condition, paired = T)$p,
  temps_p_value = t_test(dffinal, temps ~ condition, paired = T)$p
)

p_values <- tibble(t(p_values))
p_values <- tibble(round(p_values$`t(p_values)`, digits = 2))

cohen_p <-
  tibble(cohens_d(dffinal, pourcentageVO2 ~ condition, paired = T)[, c("effsize", "magnitude")])
cohen_p$effsize <- round(cohen_p$effsize, digits = 2)

cohen_h <-
  tibble(cohens_d(dffinal, hr ~ condition, paired = T)[, c("effsize", "magnitude")])
cohen_h$effsize <- round(cohen_h$effsize, digits = 2)

cohen_t <-
  tibble(cohens_d(dffinal, temps ~ condition, paired = T)[, c("effsize", "magnitude")])
cohen_t$effsize <- round(cohen_t$effsize, digits = 2)

cohen <- rbind(cohen_p, cohen_h, cohen_t)

outcome <- cbind(summary_data,p_values, cohen)

#codelatex

outcome %>%
  kbl(caption="Table 1: Summary race parameters (n=16)",
      format= "latex",
      col.names = c("FIR","PLACEBO","$p$$~$value","Cohen $d$","Magnitude"),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")

#plot

dffinal1 <- filter(dffinal, sujet != 8)

dffinal1 %>%  ggplot(aes(x =condition, y= temps)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(aes(group = sujet) , linetype = "dotted") +
  geom_point(aes(color = factor(sujet))) +
  geom_boxplot(aes(fill = condition), alpha = 0.4) +
  stat_summary(aes(label = round(..y.., 2)),
                   geom = "text",
                   fun.y = mean,
                   size = 4 , )+
  stat_compare_means(method = "t.test" , paired = TRUE, label.x = 1.5)+
  labs(color = "Sujet")


AL <- ggplot(listtest$AlbanLegallA , aes(x= distance)) +
  geom_ribbon(aes(ymin = 1850 , ymax = altitude), fill = "#006cc1", alpha = 0.3) +
  ylim(1850, 1950)

