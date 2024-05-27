pacman::p_load(tidyverse,lubridate)

load("crohn.RData")
pop = read_excel("C:/Users/MARCOS.ANTUNES/Downloads/br_uf_pop.xlsx", sheet = 2)
#Tratamento de dados

df$data = ymd(df$AP_DTAUT)
df$ano = year(df$data)

df1 = df %>% filter(ano=="2019"|ano=="2020"|ano=="2021"|ano=="2022")

df1$AM_PESO = as.numeric(df1$AM_PESO)
df1$AM_ALTURA = as.numeric(df1$AM_ALTURA)
test = df1 %>% filter(AM_PESO<250)
test = test %>% filter(AM_PESO<210)
test$altura_m = test$AM_ALTURA/100
test$imc = test$AM_PESO/(test$altura_m*test$altura_m)
test = test %>% filter(imc<45)
df1$AP_NUIDADE = as.numeric(df1$AP_NUIDADE)

# Supondo que df1 seja o dataframe original
summary_data <- summary(factor(df1$AP_CIDPRI))

# Criar um dataframe a partir do sumário
df1_summary <- data.frame(
  AP_CIDPRI = names(summary_data),
  Frequencia = as.numeric(summary_data)
)



ggplot(df1_summary, aes(x = AP_CIDPRI, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Frequência de dispensação por CID-10", x = "", y = "Frequência") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black") # Define o título do gráfico como preto
  )


ggplot(df1, aes(x = factor(ano), fill = AP_CIDPRI)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Frequência de dispensação por CID-10 e ano", x = "Ano", y = "Frequência") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black") # Define o título do gráfico como preto
  )

ggplot(df1, aes(x = factor(ano))) +
  geom_bar(position = "dodge", color = "black", fill = "skyblue", width = 0.5) + # Adicionei uma cor de preenchimento padrão
  labs(title = "Frequência de dispensação por ano, 2014 - 2022", x = "Ano", y = "Frequência") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + # Remove o espaço entre a linha do eixo x e as barras
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    axis.line = element_line(color="black") # Adiciona linha nos eixos
  )


# Criando o boxplot
ggplot(test, aes(x = AP_SEXO, y = imc, fill = AP_SEXO)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot da Mediana de IMC por Sexo", x = "Sexo", y = "IMC") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = c("M" = "skyblue", "F" = "orange"), # Ajuste as cores conforme necessário
    labels = c("M" = "Masculino", "F" = "Feminino"),
    name = "Sexo"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    legend.title = element_text(face = "bold") # Define o título da legenda como negrito
  )


# Identificar e remover outliers
remove_outliers <- function(data, variable) {
  outlier_indices <- boxplot.stats(data[[variable]])$out
  data_filtered <- data[!data[[variable]] %in% outlier_indices, ]
  num_outliers <- nrow(data) - nrow(data_filtered)
  list(data_filtered = data_filtered, num_outliers = num_outliers)
}

# Aplicar a função para remover outliers de IMC
result <- remove_outliers(test, "imc")
test_filtered <- result$data_filtered
num_outliers <- result$num_outliers

# Informar quantos outliers foram removidos
cat("Número de outliers removidos: ", num_outliers, "\n")

# Criando o boxplot sem outliers
ggplot(test_filtered, aes(x = AP_SEXO, y = imc, fill = AP_SEXO)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot da Mediana de IMC por Sexo (sem outliers)", x = "Sexo", y = "IMC") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = c("M" = "skyblue", "F" = "orange"), # Ajuste as cores conforme necessário
    labels = c("M" = "Masculino", "F" = "Feminino"),
    name = "Sexo"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    legend.title = element_text(face = "bold") # Define o título da legenda como negrito
  )

ggplot(df1, aes(x = factor(ano), fill = AP_TPAPAC)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Frequência de dispensação por CID-10 e ano", x = "Ano", y = "Frequência") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = c("1" = "skyblue", "2" = "orange"), # Ajuste as cores conforme necessário
    labels = c("1" = "Inicial", "2" = "Continuidade"),
    name = "Tipo de APAC"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    panel.grid.minor = element_blank(), # Remove as linhas de grade secundárias
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    legend.title = element_text(face = "bold") # Define o título da legenda como negrito
  )



# Supondo que as categorias de AP_PRIPAL sejam de 1 a 17
# Verifique os níveis da variável AP_PRIPAL

# Supondo que df1 já está carregado com suas variáveis

# Vetor de códigos e seus respectivos títulos
titulos <- c(
  "0604010036" = "Mesalazina – 800 mg",
  "0604010028" = "Mesalazina – 500 mg",
  "0604530013" = "Azatioprina – 50 mg",
  "0604010010" = "Mesalazina – 400 mg",
  "0604380062" = "Adalimumabe – 40 mg",
  "0604380054" = "Infliximabe – 10 mg/ml injetável",
  "0604010095" = "Sulfassalazina – 500 mg",
  "0604010060" = "Mesalazina – 1000 mg",
  "0604010044" = "Mesalazina – 250 mg",
  "0604010052" = "Mesalazina – 500 mg",
  "0604380119" = "Infliximabe – 10 mg/ml injetável",
  "0604380070" = "Certolizumabe pegol – 200 mg/ml injetável",
  "0604320167" = "Vedolizumabe – 300 mg",
  "0604010079" = "Mesalazina – 1g + diluente 100 ml",
  "0604530030" = "Mesalazina – 3 g + diluente 100 ml",
  "0604010087" = "Adalimumabe – 40 mg injetável",
  "0604380127" = "Adalimumabe – 40 mg injetável",
  "0604380135" = "Metotrexato – 25 mg/ml injetável",
  "0604320159" = "Tofacitinibe – 5 mg",
  "0604340028" = "Ciclosporina – 25 mg",
  "0604340044" = "Ciclosporina – 100 mg",
  "0604340036" = "Ciclosporina – 50 mg",
  "0604280106" = "Metilprednisolona – 500 mg injetável",
  "0604380097" = "Adalimumabe - 40 mg",
  "0604340052" = "Ciclosporina – 50 mg/ml solução oral"
)

df2 = df1 %>% filter(AP_TPAPAC==1)
df3 = df1 %>% filter(AP_TPAPAC==2)


df2$AP_PRIPAL <- titulos[df2$AP_PRIPAL]

# Verifique os níveis da variável AP_PRIPAL em df2
unique_levels <- unique(df2$AP_PRIPAL)

# Defina as cores e labels apenas para os níveis presentes nos dados
cores <- c("#1f77b4", "#ff7f0e", "green", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
           "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94",
           "#f7b6d2", "#c1c2c9", "#dbdb8d", "#9edae5", "#d62766", "#2ca05b", "#9467bd", "#8c564b",
           "#bcbd22", "#99becf", "#1f01b4")

# Ajuste os níveis para corresponder aos dados
valid_cores <- cores[1:length(unique_levels)]
valid_labels <- unique_levels

ggplot(df2, aes(x = factor(AP_PRIPAL), fill = factor(AP_PRIPAL))) +
  geom_bar(position = "dodge", color = "black") +geom_density()+
  labs(title = "Frequência por CID-10 - APAC incial", x = "", y = "Frequência") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = setNames(valid_cores, unique_levels),
    labels = setNames(valid_labels, unique_levels),
    name = "APAC - Procedimento"
  ) +
  facet_wrap(~ AP_CIDPRI) +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    axis.text.x = element_blank(), # Remove o texto dos ticks do eixo x
    axis.ticks.x = element_blank(), # Remove os ticks do eixo x
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    legend.title = element_text(face = "bold"), # Define o título da legenda como negrito
    legend.position = "bottom", # Posiciona a legenda abaixo do gráfico
    legend.direction = "horizontal", # Define a direção da legenda como horizontal]
  )
####################


df3 = df1 %>% filter(AP_TPAPAC==2)

df3$AP_PRIPAL <- titulos[df3$AP_PRIPAL]

# Verifique os níveis da variável AP_PRIPAL em df2
unique_levels2 <- unique(df3$AP_PRIPAL)

# Defina as cores e labels apenas para os níveis presentes nos dados
cores <- c("#1f77b4", "#ff7f0e", "green", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
           "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94",
           "#f7b6d2", "#c1c2c9", "#dbdb8d", "#9edae5", "#d62766", "#2ca05b", "#9467bd", "#8c564b",
           "#bcbd22", "#99becf", "#1f01b4")

# Ajuste os níveis para corresponder aos dados
valid_cores2 <- cores[1:length(unique_levels2)]
valid_labels2 <- unique_levels2

ggplot(df3, aes(x = factor(AP_PRIPAL), fill = factor(AP_PRIPAL))) +
  geom_bar(position = "dodge", color = "black") +geom_density()+
  labs(title = "Frequência por CID-10 - APAC contínua", x = "", y = "Frequência") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = setNames(valid_cores, unique_levels),
    labels = setNames(valid_labels, unique_levels),
    name = "APAC - Procedimento"
  ) +
  facet_wrap(~ AP_CIDPRI) +
  theme(
    panel.grid.major = element_blank(), # Remove as linhas de grade principais
    axis.text.x = element_blank(), # Remove o texto dos ticks do eixo x
    axis.ticks.x = element_blank(), # Remove os ticks do eixo x
    text = element_text(color = "black"), # Define todas as letras como pretas
    axis.text = element_text(color = "black"), # Define o texto dos eixos como preto
    axis.title = element_text(color = "black"), # Define o título dos eixos como preto
    plot.title = element_text(color = "black"), # Define o título do gráfico como preto
    legend.title = element_text(face = "bold"), # Define o título da legenda como negrito
    legend.position = "bottom", # Posiciona a legenda abaixo do gráfico
    legend.direction = "horizontal", # Define a direção da legenda como horizontal]
  )





install.packages("gtsummary")
library(gtsummary)
library(flextable)

df_selected <- df1 %>% select(AP_PRIPAL, AP_TPAPAC)

# Criar a tabela de resumo com gtsummary
tbl <- df_selected %>%
  tbl_summary(by = AP_TPAPAC, label = list(AP_PRIPAL ~ "APAC's"),
              sort = all_categorical() ~ "frequency")


tbl = as_flex_table(tbl)

save_as_docx(tbl,path="tabela1.docx")


# Supondo que df1 é o seu dataframe e data é a variável que contém a data em formato de dia, mês e ano
df_summ <- df1 %>%
  mutate(data = as.Date(data, format = "%Y-%m-%d")) %>%  # Certificar que a variável data está no formato Date
  group_by(month = floor_date(data, "month"), AP_CIDPRI) %>%        # Agrupar por mês
  summarize(total_cases = n())                           # Contar o número de casos por mês

df_summ$pop = 203080756
df_summ$inc = df_summ$total_cases/df_summ$pop*100000

# Criar o gráfico de múltiplas linhas com uma paleta que suporta mais de dez cores
p <- ggplot(df_summ, aes(x = month, y = inc, color = AP_CIDPRI, group = AP_CIDPRI)) +
  geom_line(size = 1) +  # Adicionar linhas
  geom_point(size = 2) +  # Adicionar pontos para destaque
  scale_color_viridis_d() +  # Usar a paleta viridis, opção 'plasma'
  labs(title = "Incidência por Mês", x = "Mês", y = "Incidência", color = "Causa Principal") +
  theme_minimal(base_size = 15) +  # Usar um tema minimalista e aumentar o tamanho da fonte
  theme(legend.position = "right")  # Colocar a legenda à direita

# Exibir o gráfico
print(p)

#################

# Supondo que df1 é o seu dataframe e data é a variável que contém a data em formato de dia, mês e ano
df_summ1 <- df1 %>%
  mutate(data = as.Date(data, format = "%Y-%m-%d")) %>%  # Certificar que a variável data está no formato Date
  group_by(month = floor_date(data, "month"), AP_CIDPRI) %>%        # Agrupar por mês
  summarize(total_cases = n())                           # Contar o número de casos por mês

df_summ$pop = 203080756
df_summ$inc = df_summ$total_cases/df_summ$pop*100000

# Criar o gráfico de múltiplas linhas com uma paleta que suporta mais de dez cores
p <- ggplot(df_summ, aes(x = month, y = inc, color = AP_CIDPRI, group = AP_CIDPRI)) +
  geom_line(size = 1) +  # Adicionar linhas
  geom_point(size = 2) +  # Adicionar pontos para destaque
  scale_color_viridis_d() +  # Usar a paleta viridis, opção 'plasma'
  labs(title = "Incidência por Mês", x = "Mês", y = "Incidência", color = "Causa Principal") +
  theme_minimal(base_size = 15) +  # Usar um tema minimalista e aumentar o tamanho da fonte
  theme(legend.position = "right")  # Colocar a legenda à direita

# Exibir o gráfico
print(p)

df1 <- df1 %>%
  mutate(uf = substr(AP_MUNPCN, 1, 2))

df_summ2 <- df1 %>% filter(AP_TPAPAC==1) %>%  # Certificar que a variável data está no formato Date
  group_by(AP_CIDPRI, uf) %>%        # Agrupar por mês
  summarize(total_cases = n())     

df_summ2$uf = as.double(df_summ2$uf)

library(geobr)

df_summ2 = df_summ2 %>% left_join(pop, by = c("uf"="code_ibge"))
df_summ2$inc = df_summ2$total_cases/df_summ2$pop*100000
df_summ2$inc = round(df_summ2$inc,2)
df_summ2$uf.y = NULL

states <- read_state(code_state = "all", year = 2020)
map_data <- states %>%
  left_join(df_summ2, by = c("code_state" = "uf"))

p <- ggplot(data = map_data) +
  geom_sf(aes(fill = inc), color = "white") +  # Mapa de calor
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "") +  # Paleta de cores
  labs(title = "Incidência de casos por estado", subtitle = "Diferenciado por CID-10") +
  theme_void(base_size = 15) +  # Tema vazio para remover plano cartesiano
  facet_wrap(~ AP_CIDPRI) +  # Criar vários mapas de calor por AP_CIDPRI
  theme(
    legend.position = "right",
    legend.title.align = 0.5,
    panel.background = element_rect(fill = "white", color = NA),  # Fundo branco
    plot.background = element_rect(fill = "white", color = NA)    # Fundo branco
  )  
