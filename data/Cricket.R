library(tidyverse)
library(readxl)

DD <- read_excel("CricketDissectionData.xls")
MM <- read_csv("Cricket_output_10minute.csv")
MM <- MM[MM$id != "42.8",]

MD <- left_join(MM, DD, join_by(id == Number))

MD <- MD[!is.na(MD$`Body Wt`),]


mod <- lm(CO2.mean ~ `Body Wt`, data = MD)

MD$CO2_resid <- mod$residuals

MD |> group_by(`Wing Morph`) |>
  summarise(mean = mean(CO2_resid))

MD.out <- MD[,c(1,10,12, 19)]

colnames(MD.out)[2] <- "Wing_Morph"
colnames(MD.out)[3] <- "DLMstate"

write_csv(MD.out, file = "Cricket_Metabolic_MassCor.csv")
