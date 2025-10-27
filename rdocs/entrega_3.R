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
  labs(x = "Lojas", y = "Idade") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

box_plot_lojas

hist_lojas<- ggplot(relatorio_ambar_seco) +
  aes(x = Age) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  facet_grid(. ~ NameStore) +
  labs(x = "Idade em anos", y = "Frequência") +
  theme_estat(
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white")
  )
ggsave("hist_grid.pdf", width = 200, height = 93, units = "mm"
)

hist_lojas

print_quadro_resumo <- function(data, Age, title="Medidas resumo
da(o) [nome da variável]", label="quad:quadro_resumo1")
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