library(tidyverse)
library(survey)
library(srvyr)
library(gt)
library(haven)
library(labelled)
library(readxl)
library(scales)

bespanel <- read_sav("bespanel/BES2024_W29_Panel_v29.1_file.sav")

## CODE FOR FINDING COLUMN NAMES IN THE PANEL DATASET
## THE PANEL DATASET IS WEIRD AND CONFUSING, BUT AT LEAST THE NAMES ARE ALL ALIGNED
## WITH THE DOCUMENTATION, UNLIKE PRIOR DATASETS

column_names <- colnames(bespanel)
bespanellrscale <- grep("lr_scale", column_names, value = TRUE, ignore.case = TRUE)
print(bespanellrscale)

column_names <- colnames(bespanel)
bespanelalscale <- grep("al_scale", column_names, value = TRUE, ignore.case = TRUE)
print(bespanelalscale)

column_names <- colnames(bespanel)
bespanelwt <- grep("wt_new_W19", column_names, value = TRUE, ignore.case = TRUE)
print(bespanelwt)

column_names <- colnames(bespanel)
bespanelleftRight <- grep("leftRight", column_names, value = TRUE, ignore.case = TRUE)
print(bespanelleftRight)

## CODE FOR FINDING OUT WHAT KIND OF DATA IS IN EACH COLUMN.
## USEFUL FOR DEALING WITH THE FUCKING HAVEN LABELLED BULLSHIT.

column_types <- sapply(besmerged, class)
print(column_types)

## HOW TO CHECK IF THERE ARE NA VALUES IN A COLUMN
na_count <- sum(is.na(besw1$wt_full_W1))
print(na_count)


## EXTRACTING DATA FROM BESPANEL AS WAVES

process_dataframe <- function(df, wave_col, wave_value, wt_col, values_wave, wave_prefix) {
  
  leftRight_col <- paste0("leftRight", wave_prefix)
  lr_scale_col <- paste0("lr_scale", values_wave)
  al_scale_col <- paste0("al_scale", values_wave)
  
  df_processed <- df %>%
    select(c("id", wave_col, wt_col, leftRight_col, lr_scale_col, al_scale_col)) %>%
    filter(!!sym(wave_col) == 1) %>%
    mutate(id = paste0(wave_prefix, ":", id)) %>%
    rename(wt = !!sym(wt_col),
           leftRight = !!sym(leftRight_col),
           lr_scale = !!sym(lr_scale_col),
           al_scale = !!sym(al_scale_col),
           wave = !!sym(wave_col)) %>%
    mutate(wave = recode(wave, `1` = wave_prefix))
  
  return(df_processed)
}

## EXTRACT THIS DATA FROM EVERY WAVE?

besw1 <- process_dataframe(bespanel, 
                           wave_col = "wave1",
                           wt_col = "wt_full_W1",
                           values_wave = "W1_W5",
                           wave_prefix = "W1")

besw2 <- process_dataframe(bespanel, 
                           wave_col = "wave2",
                           wt_col = "wt_full_W2",
                           values_wave = "W1_W5",
                           wave_prefix = "W2")

besw3 <- process_dataframe(bespanel, 
                           wave_col = "wave3",
                           wt_col = "wt_full_W3",
                           values_wave = "W1_W5",
                           wave_prefix = "W3")

besw4 <- process_dataframe(bespanel, 
                           wave_col = "wave4",
                           wt_col = "wt_full_W4",
                           values_wave = "W1_W5",
                           wave_prefix = "W4")

besw5 <- process_dataframe(bespanel, 
                           wave_col = "wave5",
                           wt_col = "wt_full_W5",
                           values_wave = "W1_W5",
                           wave_prefix = "W5")

besw6 <- process_dataframe(bespanel, 
                           wave_col = "wave6",
                           wt_col = "wt_new_W6",
                           values_wave = "W6", 
                           wave_prefix = "W6")

besw7 <- process_dataframe(bespanel, 
                           wave_col = "wave7",
                           wt_col = "wt_new_W7",
                           values_wave = "W7_W9", 
                           wave_prefix = "W7")

besw8 <- process_dataframe(bespanel, 
                           wave_col = "wave8",
                           wt_col = "wt_new_W8",
                           values_wave = "W7_W9",
                           wave_prefix = "W8")

besw9 <- process_dataframe(bespanel, 
                           wave_col = "wave9",
                           wt_col = "wt_new_W9",
                           values_wave = "W7_W9", 
                           wave_prefix = "W9")

besw10 <- process_dataframe(bespanel, 
                           wave_col = "wave10",
                           wt_col = "wt_new_W10",
                           values_wave = "W10_W12", 
                           wave_prefix = "W10")

besw11 <- process_dataframe(bespanel, 
                            wave_col = "wave11",
                            wt_col = "wt_new_W11",
                            values_wave = "W10_W12", 
                            wave_prefix = "W11")

besw12 <- process_dataframe(bespanel, 
                            wave_col = "wave12",
                            wt_col = "wt_new_W12",
                            values_wave = "W10_W12", 
                            wave_prefix = "W12")

besw13 <- process_dataframe(bespanel, 
                            wave_col = "wave13",
                            wt_col = "wt_new_W13_result", 
                            values_wave = "W13",
                            wave_prefix = "W13")

besw14 <- process_dataframe(bespanel, 
                            wave_col = "wave14",
                            wt_col = "wt_new_W14",
                            values_wave = "W14W15",
                            wave_prefix = "W14")

besw15 <- process_dataframe(bespanel, 
                            wave_col = "wave15",
                            wt_col = "wt_new_W15",
                            values_wave = "W14W15",
                            wave_prefix = "W15")

besw16 <- process_dataframe(bespanel, 
                            wave_col = "wave16",
                            wt_col = "wt_new_W16", 
                            values_wave = "W16",
                            wave_prefix = "W16")

besw17 <- process_dataframe(bespanel, 
                            wave_col = "wave17",
                            wt_col = "wt_new_W17",
                            values_wave = "W17_W19",
                            wave_prefix = "W17")

besw18 <- process_dataframe(bespanel, 
                            wave_col = "wave18",
                            wt_col = "wt_new_W18",
                            values_wave = "W17_W19",
                            wave_prefix = "W18")

besw19 <- process_dataframe(bespanel, 
                            wave_col = "wave19",
                            wt_col = "wt_new_W19_result",
                            values_wave = "W17_W19", 
                            wave_prefix = "W19")

besw20 <- process_dataframe(bespanel, 
                            wave_col = "wave20",
                            wt_col = "wt_new_W20", 
                            values_wave = "W20", 
                            wave_prefix = "W20")

besw21 <- process_dataframe(bespanel, 
                            wave_col = "wave21",
                            wt_col = "wt_new_W21", 
                            values_wave = "W21", 
                            wave_prefix = "W21")

besw22 <- process_dataframe(bespanel, 
                            wave_col = "wave22",
                            wt_col = "wt_new_W22",
                            values_wave = "W22", 
                            wave_prefix = "W22")

besw23 <- process_dataframe(bespanel, 
                            wave_col = "wave23",
                            wt_col = "wt_new_W23",
                            values_wave = "W23", 
                            wave_prefix = "W23")

besw25 <- process_dataframe(bespanel, 
                            wave_col = "wave25",
                            wt_col = "wt_new_W25", 
                            values_wave = "W25W26", 
                            wave_prefix = "W25")

besw26 <- process_dataframe(bespanel, 
                            wave_col = "wave26",
                            wt_col = "wt_new_W26", 
                            values_wave = "W25W26", 
                            wave_prefix = "W26")

besw27 <- process_dataframe(bespanel, 
                            wave_col = "wave27",
                            wt_col = "wt_new_W27", 
                            values_wave = "W27W29", 
                            wave_prefix = "W27")

besw29 <- process_dataframe(bespanel, 
                            wave_col = "wave29",
                            wt_col = "wt_new_W29_result", 
                            values_wave = "W27W29", 
                            wave_prefix = "W29")

## COMBINE ALL DATAFRAMES

besmerged <- bind_rows(besw1, besw2, besw3, besw4, besw5, besw6, besw7, besw8, besw9, besw10,
                       besw11, besw12, besw13, besw14, besw15, besw16, besw17, besw18,
                       besw19, besw20, besw21, besw22, besw23, besw25, besw26, 
                       besw27, besw29)

## CREATE A MONTH VARIABLE

beswavedates <- read_excel(path = "bespanel/beswavedates.xlsx")

besmerged <- besmerged %>%
  left_join(beswavedates, by = c("wave" = "beswave"))

besmerged$startdate <- as_date(besmerged$startdate)
besmerged$enddate <- as_date(besmerged$enddate)

## GET RID OF THE BLASTED HAVEN LABEL FORMAT

besmerged$leftRight <- unlabelled(besmerged$leftRight)
besmerged$lr_scale <- unlabelled(besmerged$lr_scale)
besmerged$al_scale <- unlabelled(besmerged$al_scale)

## REMOVE NA WEIGHTS

besmerged_clean <- besmerged %>%
  filter(!is.na(wt))

## CREATE WEIGHTED SURVEY DATA

besmerged_des <- besmerged_clean %>%
  as_survey_design(
    ids = id,
    weight = wt
  )

## COLLAPSE THE LEFT-RIGHT FACTOR INTO LEFT, CENTRE, RIGHT

besmerged_des <- besmerged_des %>%
  mutate(lr_sid = fct_collapse(
    leftRight,
    Left = c("Left", "1", "2", "3"),
    Centre = c("4", "5", "6"),
    Right = c("7", "8", "9", "Right"),
    DK = c("Don't know")
  )) 

## COLLAPSE THE LEFT RIGHT VALUES INTO LEFT, CENTRE, RIGHT
## COLLAPSE THE LIB AUTH VALUES INTO LIBERAL, MODERATE, AUTHORITARIAN
## CLASSIFICATION MIRRORS THAT USED BY PAULA SURRIDGE IN https://onlinelibrary.wiley.com/doi/10.1111/1467-923X.13494

besmerged_des <- besmerged_des%>%
  mutate(lr_value = case_when(
    lr_scale %in% 0:2.5 ~ "Left",
    lr_scale %in% 3:5 ~ "Centre",
    lr_scale %in% 5.5 ~ "Right",
    TRUE ~ NA_character_)) %>%
  mutate(al_value = case_when(
    al_scale %in% 0:4.5 ~ "Liberal",
    al_scale %in% 5:7 ~ "Moderate",
    al_scale %in% 7.5:10 ~ "Authoritarian",
    TRUE ~ NA_character_))

besmerged_des %>%
  survey_count(lr_sid, lr_value)


## CREATE SID OVERTIME

besmerged_sid_overtime <- besmerged_des %>%
  group_by(startdate) %>%
  summarize(
    Left = survey_mean(lr_sid == "Left", na.rm = TRUE) * 100,
    Centre = survey_mean(lr_sid == "Centre", na.rm = TRUE) * 100,
    Right = survey_mean(lr_sid == "Right", na.rm = TRUE) * 100,
    DK = survey_mean(lr_sid == "DK", na.rm = TRUE) * 100
  )

# wave_order <- c("W1", "W6", "W7", "W10", "W13", "W14", "W16", "W17", "W20", "W21", "W22",
#                 "W23", "W25", "W27")

#besmerged_sid_overtime$wave <- factor(besmerged_sid_overtime$wave, levels = wave_order)

besmerged_sid_overtime_long <- besmerged_sid_overtime %>%
  pivot_longer(cols = c(Left, Centre, Right, DK), names_to = "ideology", values_to = "p")


## GGPLOT

datebreaks <- seq(as.Date("2014-02-20"), as.Date("2024-07-05"), by = "1 year")

theme_set(theme_bw())

ppi <- 300

png("bespanel/srid_bes.png", width = 10*ppi, height = 5*ppi, res = ppi)

p1 <- ggplot(besmerged_sid_overtime_long, aes(x = startdate, y = p,
                       color = ideology, group = ideology)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Wave", 
       y = "Share of BESIP respondents in each wave (%)",
       title = "Self-Reported Ideology in Great Britain over time",
       caption = "Data: British Election Study Internet Panel W1-W29",
       color = "Self-reported\nideology") +
  scale_x_date(breaks = datebreaks, labels = date_format("%Y")) +
  scale_color_manual(values = c("#4daf4a",
                                "#969696",
                                "#e41a1c",
                                "#377eb8")) +
  coord_cartesian(ylim = c(15, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 12), size = 12),
        axis.title.y = element_text(margin = margin(r = 12), size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(margin = margin(12,0,12,0),
                                  size = 14,
                                  face = "bold"))

p1

dev.off()

## DEFUNCT CODE

# besw1 <- bespanel %>%
#   select(c("id", "wave1", "wt_full_W1","leftRightW1","lr_scaleW1_W5","al_scaleW1_W5")) %>%
#   filter(wave1 == 1) %>%
#   mutate(id = paste0("W1:", id)) %>%
#   rename(wt = wt_full_W1,
#          leftRight = leftRightW1,
#          lr_scale = lr_scaleW1_W5,
#          al_scale = al_scaleW1_W5)