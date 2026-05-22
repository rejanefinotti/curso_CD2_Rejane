# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(ggridges)
library(viridis)
library(patchwork)


# Abrindo bases -----------------------------------------------------------

pm25 <- read_csv('dados/pm25_max_mean_MT.csv')

sih <- read_excel('dados/SIH_2011-2026.xlsx')


# Meses -------------------------------------------------------------------

mes_map <- c(
  Jan = "janeiro",
  Fev = "fevereiro",
  Mar = "março",
  Abr = "abril",
  Mai = "maio",
  Jun = "junho",
  Jul = "julho",
  Ago = "agosto",
  Set = "setembro",
  Out = "outubro",
  Nov = "novembro",
  Dez = "dezembro"
)

ordem_meses <- c(
  "janeiro", "fevereiro", "março", "abril", "maio", "junho",
  "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
)


# Arrumando PM2,5 ---------------------------------------------------------

pm25_2024 <- pm25 |>
  mutate(
    ano = year(date),
    mes = month(date, label = TRUE, abbr = FALSE),
    mes = as.character(mes),
    mes = factor(mes, levels = ordem_meses)
  ) |>
  filter(ano == 2024) |>
  group_by(date, mes) |>
  summarise(
    pm25_max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(pm25_max <= quantile(pm25_max, 0.99, na.rm = TRUE))


# Arrumando SIH -----------------------------------------------------------

sih_2024_resp <- sih |>
  mutate(
    AnoInternacao = as.numeric(AnoInternacao),
    DiaInternacao = as.numeric(DiaInternacao),
    NumeroInternacoes = as.numeric(NumeroInternacoes),
    mes_abrev = str_sub(MesInternacao, 4, 6),
    mes = mes_map[mes_abrev],
    mes = factor(mes, levels = ordem_meses)
  ) |>
  filter(AnoInternacao == 2024)


# Agregando SIH por dia ---------------------------------------------------

sih_diario_2024 <- sih_2024_resp |>
  group_by(mes, DiaInternacao) |>
  summarise(
    internacoes = sum(NumeroInternacoes, na.rm = TRUE),
    .groups = "drop"
  )


# Gráfico PM2,5 -----------------------------------------------------------

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
    linewidth = 0.3
  ) +
  scale_y_discrete(limits = rev(ordem_meses)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "PM2,5"
  ) +
  labs(
    title = "A) PM2,5 máxima diária",
    x = "PM2,5 máxima diária",
    y = "Mês"
  ) +
  theme_minimal()


# Gráfico internações -----------------------------------------------------

grafico_sih <- ggplot(
  sih_diario_2024,
  aes(
    x = internacoes,
    y = mes,
    fill = after_stat(x)
  )
) +
  geom_density_ridges_gradient(
    scale = 2.5,
    rel_min_height = 0.01,
    color = "black",
    linewidth = 0.3
  ) +
  scale_y_discrete(limits = rev(ordem_meses)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Internações"
  ) +
  labs(
    title = "B) Internações respiratórias diárias",
    x = "Internações diárias",
    y = "Mês"
  ) +
  theme_minimal()


# Painel com os dois gráficos --------------------------------------------

painel_graficos <- grafico_pm25 / grafico_sih +
  plot_annotation(
    title = "Distribuição mensal de PM2,5 e internações respiratórias em Mato Grosso, 2024"
  )

painel_graficos


# Salvando painel ---------------------------------------------------------

ggsave(
  filename = "dados/painel_ridgeline_pm25_internacoes_2024.png",
  plot = painel_graficos,
  width = 12,
  height = 12,
  dpi = 300
)

# Pacotes adicionais ------------------------------------------------------

library(gganimate)
library(gifski)
library(transformr)


# Base mensal PM2,5 -------------------------------------------------------

pm25_mes_2024 <- pm25 |>
  mutate(
    ano = year(date),
    mes = month(date),
    code_muni = str_sub(as.character(code_muni), 1, 6)
  ) |>
  filter(ano == 2024) |>
  group_by(code_muni, mes) |>
  summarise(
    pm25_media = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mes_nome = factor(
      mes,
      levels = 1:12,
      labels = ordem_meses
    )
  )


# Mapa com PM2,5 mensal ---------------------------------------------------

mapa_pm25_mes <- mt_mapa |>
  st_transform(4326) |>
  mutate(
    code_muni = str_sub(as.character(CD_MUN), 1, 6)
  ) |>
  left_join(pm25_mes_2024, by = "code_muni")


# Escala fixa -------------------------------------------------------------

limites_pm25 <- range(mapa_pm25_mes$pm25_media, na.rm = TRUE)


# Animação ----------------------------------------------------------------

g_pm25_anim <- ggplot(mapa_pm25_mes) +
  geom_sf(aes(fill = pm25_media), color = "white", linewidth = 0.08) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = limites_pm25,
    na.value = "grey90",
    name = "PM2,5"
  ) +
  labs(
    title = "Distribuicao mensal do PM2,5 em Mato Grosso",
    subtitle = "Ano de 2024 - {closest_state}",
    caption = "Media mensal por municipio",
    fill = "PM2,5"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
  transition_states(
    mes_nome,
    transition_length = 2,
    state_length = 3,
    wrap = TRUE
  ) +
  ease_aes("linear")

g_pm25_anim
