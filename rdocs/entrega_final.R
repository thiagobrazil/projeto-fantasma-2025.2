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

renda_tot<- old_town_road|>
  mutate(Date = year(Date))|>
  rename(Year = Date)|>
  mutate(TotalPrice = 5.31  * UnityPrice* Quantity)|>
  select(SaleID, Year, NameStore, TotalPrice)|>
  arrange(Year)

media_por_ano<- renda_tot|>
  group_by(Year)|>
  summarise(StoreMean = sum(TotalPrice)/ 18)

Gráfico<- ggplot(media_por_ano) +
  aes(x=Year, y=StoreMean, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita total média (R$)") +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")

Gráfico<- Gráfico +
  scale_x_continuous(breaks = c(1880, 1881, 1882, 1883, 1884,1885, 1886, 1887, 1888, 1889))

cv_renda<- 100* (sd(media_por_ano$StoreMean)/mean(media_por_ano$StoreMean))

infos_clientes<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_clientes")

infos_clientes<- infos_clientes|>
  distinct(Cli3ntID, .keep_all = TRUE)|>
  mutate(Weight_kg = 0.45 * Weight_lbs)|>
  mutate(Height_cm = 10 * Height_dm)

pearson<- cor(infos_clientes$Weight_kg, infos_clientes$Height_cm)

Gráfico_2<- ggplot(infos_clientes) +
  aes(x = Height_cm, y = Weight_kg) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
  labs(
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")


print_quadro_resumo <- function(data, Weight_kg, title="Medidas resumo
da idade dos clientes de Âmbar Seco", label="quad:quadro_resumo3")
{
  var_name <- ensym(Weight_kg)
  data <- data %>%
    summarize(Média = round(mean(!!sym(Weight_kg)),2),
              `Desvio Padrão` = round(sd(!!sym(Weight_kg)),2),
              `Variância` = round(var(!!sym(Weight_kg)),2),
              `Mínimo` = round(min(!!sym(Weight_kg)),2),
              `1o Quartil` = round(quantile(!!sym(Weight_kg), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(Weight_kg), probs = .5)
                                ,2),
              `3o Quartil` = round(quantile(!!sym(Weight_kg), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(Weight_kg)),2)) %>%
    
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- suppressWarnings({
      floor(log10(abs(as.numeric(data[i, -1])))) + 1
    })
    numCount[!is.finite(numCount)] <- 1 %>% max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
  }
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
infos_clientes %>%
  print_quadro_resumo("Weight_kg")

print_quadro_resumo2 <- function(data, Height_cm, title="Medidas resumo
da idade dos clientes de Âmbar Seco", label="quad:quadro_resumo3")
{
  var_name <- ensym(Height_cm)
  data <- data %>%
    summarize(Média = round(mean(!!sym(Height_cm)),2),
              `Desvio Padrão` = round(sd(!!sym(Height_cm)),2),
              `Variância` = round(var(!!sym(Height_cm)),2),
              `Mínimo` = round(min(!!sym(Height_cm)),2),
              `1o Quartil` = round(quantile(!!sym(Height_cm), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(Height_cm), probs = .5)
                                ,2),
              `3o Quartil` = round(quantile(!!sym(Height_cm), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(Height_cm)),2)) %>%
    
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- suppressWarnings({
      floor(log10(abs(as.numeric(data[i, -1])))) + 1
    })
    numCount[!is.finite(numCount)] <- 1 %>% max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
  }
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
infos_clientes %>%
  print_quadro_resumo("Height_cm")


relatorio_vendas<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "relatorio_vendas")

infos_clientes<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_clientes")

info_clientes<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_clientes")|>
  rename(ClientID = Cli3ntID)

info_lojas<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_lojas")|>
  rename(StoreID = Stor3ID)

info_cidades<- read_xlsx("relatorio_old_town_road.xlsx", sheet= "infos_cidades")|>
  rename(CityID = C1tyID)

lojas_cidades<- inner_join(info_lojas, info_cidades, by = "CityID")

relatorio_clientes<- inner_join(relatorio_vendas, info_clientes, by = "ClientID")

relatorio_ambar_seco<- inner_join(relatorio_clientes, lojas_cidades, by = "StoreID")

relatorio_ambar_seco<- relatorio_ambar_seco|>
  distinct(Name, .keep_all = T)|>
  filter(NameCity == "Âmbar Seco")|>
  arrange(NameStore)|>
  select(NameStore, Name, Age)

media_por_loja<- relatorio_ambar_seco|>
  group_by(NameStore)|>
  summarise(Age = mean(Age))|>
  select(NameStore, Age)

box_plot_lojas<- ggplot(relatorio_ambar_seco) +
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Lojas", y = "Idade (anos)") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

box_plot_lojas

print_quadro_resumo <- function(data, Age, title="Medidas resumo
da idade dos cliente de Âmbar Seco", label="quad:quadro_resumo1")
{
  var_name <- ensym(Age)
  data <- data %>%
    summarize(Média = round(mean(!!sym(Age)),2),
              `Desvio Padrão` = round(sd(!!sym(Age)),2),
              `Variância` = round(var(!!sym(Age)),2),
              `Mínimo` = round(min(!!sym(Age)),2),
              `1o Quartil` = round(quantile(!!sym(Age), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(Age), probs = .5)
                                ,2),
              `3o Quartil` = round(quantile(!!sym(Age), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(Age)),2)) %>%
    
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- suppressWarnings({
      floor(log10(abs(as.numeric(data[i, -1])))) + 1
    })
    numCount[!is.finite(numCount)] <- 1 %>% max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
  }
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
relatorio_ambar_seco %>%
  group_by(NameStore) %>%
  print_quadro_resumo("Age")


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

top_vendas<- old_town_road|>
  mutate(Date = year(Date))|>
  rename(Year = Date)|>
  filter(Year == 1889)|>
  mutate(TotalPrice = 5.31  * UnityPrice* Quantity)|>
  select(SaleID, NameProduct, Quantity, NameStore, TotalPrice)

top_lojas<- top_vendas|>
  group_by(NameStore)|>
  summarise(TotalIncome = sum(TotalPrice))|>
  arrange(desc(TotalIncome))|>
  select(NameStore, TotalIncome)|>
  head(3)
    
top_produtos<- top_vendas|>
  filter(NameStore %in% c("Loja TendTudo", "Loja Ouro Fino", "Ferraria Apache"))|>
  group_by(NameStore, NameProduct)|>
  summarise(Quantity = sum(Quantity))|>
  arrange(NameStore ,desc(Quantity))|>
  slice_head(n = 3)|>
  select(NameStore, NameProduct, Quantity)|>
  distinct()|>
  ungroup()

top_produtos<- top_produtos|>
  add_row(NameStore = "Ferraria Apache", NameProduct = "Whisky", Quantity = 41)


grafico_produtos<- ggplot(top_produtos) +
  aes(
    x = fct_reorder(NameStore, Quantity, .desc = T), y = Quantity,
    fill = NameProduct, label = Quantity
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Loja", y = "Quantidade") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

grafico_produtos