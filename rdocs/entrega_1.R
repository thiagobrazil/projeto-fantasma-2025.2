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

relatorio_vendas<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "relatorio_vendas")

info_vendas<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_vendas")|>
  rename(SaleID = Sal3ID)

info_produtos<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_produtos")|>
  rename(ItemID = Ite3ID)

info_lojas<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_lojas")|>
  rename(StoreID = Stor3ID)

vendas_produtos<- inner_join(info_vendas, info_produtos, by = "ItemID")

vendas<- inner_join(relatorio_vendas, vendas_produtos, by = "SaleID")

old_town_road<- inner_join(vendas, info_lojas, by = "StoreID") 

old_town_road<- old_town_road|>
  mutate(Date = year(Date))|>
  rename(Year = Date)|>
  mutate(TotalPrice = 5.31  * UnityPrice* Quantity)|>
  select(SaleID, Year, NameStore, TotalPrice)|>
  arrange(Year)

media_por_ano<- old_town_road|>
  group_by(Year)|>
  summarise(StoreMean = sum(TotalPrice)/ 18)

Gráfico<- ggplot(media_por_ano) +
  aes(x=Year, y=StoreMean, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita total média") +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")

Gráfico<- Gráfico +
  scale_x_continuous(breaks = c(1880, 1881, 1882, 1883, 1884,1885, 1886, 1887, 1888, 1889))

