# ============================================================
# GRÁFICOS RIDGELINE - PM2,5 E INTERNAÇÕES RESPIRATÓRIAS
# Ano: 2024
# ============================================================

# Instalar pacotes, caso necessário
install.packages("ggridges")

# Carregar pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(ggridges)
library(lubridate)
library(stringr)
library(viridis)

# ============================================================
# 1. DEFINIR DIRETÓRIO
# ============================================================

dir_dados <- "C:/Users/re_co/OneDrive/curso_CD2_Rejane/dados"

arquivo_pm25 <- file.path(dir_dados, "pm25_max_mean_MT.xlsx")
arquivo_sih  <- file.path(dir_dados, "SIH_2011-2026.xlsx")

# ============================================================
# 2. IMPORTAR BASE DE PM2,5
# ============================================================

pm25 <- read_excel(arquivo_pm25)

# Ver nomes das variáveis
names(pm25)

# Ajuste os nomes abaixo caso estejam diferentes na sua base
pm25_2024 <- pm25 %>%
  mutate(
    data = as.Date(data),
    ano = year(data),
    mes = month(data, label = TRUE, abbr = FALSE)
  ) %>%
  filter(ano == 2024)

# ============================================================
# 3. GRÁFICO RIDGELINE - PM2,5 MÁXIMA
# ============================================================

grafico_pm25 <- ggplot(
  pm25_2024,
  aes(
    x = pm25_max,
    y = mes,
    fill = after_stat(x)
  )
) +
  geom_density_ridges_gradient(
    scale = 2.5,
    rel_min_height = 0.01,
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_viridis_c(option = "C", name = "PM2,5") +
  labs(
    title = "Distribuição mensal da concentração máxima de PM2,5 em Mato Grosso, 2024",
    x = "PM2,5 máxima",
    y = "Mês"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
  )

grafico_pm25

# Salvar gráfico
ggsave(
  filename = file.path(dir_dados, "ridgeline_pm25_max_2024.png"),
  plot = grafico_pm25,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 4. IMPORTAR BASE SIH
# ============================================================

sih <- read_excel(arquivo_sih)

# Ver nomes das variáveis
names(sih)

# Filtrar internações por doenças respiratórias em 2024
sih_2024_resp <- sih %>%
  filter(
    AnoInternacao == 2024,
    DiagnosticoPrincipalCid10Capitulo == "X - Doenças do aparelho respiratório"
  ) %>%
  mutate(
    mes = month(MesInternacao, label = TRUE, abbr = FALSE)
  )

# Caso MesInternacao já esteja como número de 1 a 12, use:
sih_2024_resp <- sih %>%
  filter(
    AnoInternacao == 2024,
    DiagnosticoPrincipalCid10Capitulo == "X - Doenças do aparelho respiratório"
  ) %>%
  mutate(
    mes = factor(
      MesInternacao,
      levels = 1:12,
      labels = c(
        "janeiro", "fevereiro", "março", "abril", "maio", "junho",
        "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
      )
    )
  )

# ============================================================
# 5. GRÁFICO RIDGELINE - INTERNAÇÕES RESPIRATÓRIAS
# ============================================================

grafico_sih <- ggplot(
  sih_2024_resp,
  aes(
    x = NumeroInternacoes,
    y = mes,
    fill = after_stat(x)
  )
) +
  geom_density_ridges_gradient(
    scale = 2.5,
    rel_min_height = 0.01,
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_viridis_c(option = "C", name = "Internações") +
  labs(
    title = "Distribuição mensal das internações por doenças respiratórias em Mato Grosso, 2024",
    x = "Número de internações",
    y = "Mês"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
  )

grafico_sih

# Salvar gráfico
ggsave(
  filename = file.path(dir_dados, "ridgeline_internacoes_respiratorias_2024.png"),
  plot = grafico_sih,
  width = 10,
  height = 7,
  dpi = 300
)