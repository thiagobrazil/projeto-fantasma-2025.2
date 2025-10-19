source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

infos_clientes<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_clientes")

infos_clientes<- infos_clientes|>
  mutate(Weight_kg = 0.45 * Weight_lbs)|>
  mutate(Height_cm = 10 * Height_dm)|>
  select(Name, Sex, Weight_kg, Height_cm)

Gráfico_2<- ggplot(infos_clientes) +
  aes(x = Height_cm, y = Weight_kg) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
  labs(
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")


Gráfico_por_sexo<- ggplot(infos_clientes) +
  aes(x = Height_cm, y = Weight_kg) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
  labs(
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_estat() +
  facet_wrap(Sex ~ .)
ggsave("disp_wrap.pdf", width = 158, height = 93, units = "mm")


infos_por_sexo<- infos_clientes|>
  group_by(Sex)|>
  summarise(WeightMean = mean(Weight_kg), HeightMean = mean(Height_cm),
            DPWeight = sd(Weight_kg), DPHeight = sd(Height_cm),
            MinWeight = min(Weight_kg), MaxWeight = max(Weight_kg), 
            MinHeight = min(Height_cm), MaxHeight = max(Height_cm),
            Freq = table(Sex))

infos_medias<- infos_clientes|>
  summarise(WeightMean = mean(Weight_kg), HeightMean = mean(Height_cm),
            DPWeight = sd(Weight_kg), DPHeight = sd(Height_cm),
            MinWeight = min(Weight_kg), MaxWeight = max(Weight_kg), 
            MinHeight = min(Height_cm), MaxHeight = max(Height_cm))

