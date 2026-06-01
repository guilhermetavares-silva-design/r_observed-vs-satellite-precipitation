# =============================================================================
# ANALISE DE ACURACIA: PRECIPITACAO OBSERVADA vs. SATELITE
# Dados: CEAPLA (observados) x Satelite (CSV)
# Rio Claro - SP | 1996-2025
# =============================================================================

# --- 1. PACOTES --------------------------------------------------------------
pacotes <- c(
  "readxl", "dplyr", "ggplot2", "hydroGOF", "tidyr",
  "scales", "patchwork", "lubridate", "viridis"
)

novos <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos)) install.packages(novos, dependencies = TRUE)

invisible(lapply(pacotes, library, character.only = TRUE))

# =============================================================================
# --- 2. CONFIGURACAO DOS ARQUIVOS DE ENTRADA --------------------------------
# =============================================================================

# Para reutilizar este script em outra analise, altere apenas os nomes abaixo.
# Os arquivos devem estar na mesma pasta deste script, a menos que voce informe
# o caminho completo.
arquivo_satelite <- "la.csv"
arquivo_observado <- "Dados_1996_2025_CEAPLA.xlsx"

# =============================================================================
# --- 3. LEITURA E PREPARACAO DOS DADOS --------------------------------------
# =============================================================================

# ---- 3a. Dados de satelite (CSV) -------------------------------------------
df_sat <- read.csv(arquivo_satelite, stringsAsFactors = FALSE) %>%
  rename(pr_sat = pr) %>%
  mutate(data = as.character(data))

# ---- 3b. Dados observados (XLSX com aba por ano) ---------------------------
meses_br <- c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
              "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
mes_num <- setNames(1:12, meses_br)

ler_xlsx_ceapla <- function(arquivo) {
  abas <- excel_sheets(arquivo)
  abas_anos <- abas[grepl("^[0-9]{4}$", abas)]

  registros <- lapply(abas_anos, function(aba) {
    raw <- read_excel(arquivo, sheet = aba, col_names = FALSE)

    header_idx <- which(apply(raw, 1, function(r) any(r == "DIA", na.rm = TRUE)))
    if (length(header_idx) == 0) return(NULL)

    h <- as.character(raw[header_idx, ])
    dados <- raw[(header_idx + 1):nrow(raw), ]
    colnames(dados) <- h

    dados <- dados %>%
      filter(!is.na(DIA), suppressWarnings(!is.na(as.numeric(DIA)))) %>%
      mutate(dia = as.integer(DIA))

    dados %>%
      select(dia, any_of(meses_br)) %>%
      pivot_longer(cols = -dia, names_to = "mes_abr", values_to = "pr_obs") %>%
      filter(!is.na(pr_obs)) %>%
      mutate(
        pr_obs = suppressWarnings(as.numeric(pr_obs)),
        mes = mes_num[mes_abr],
        ano = as.integer(aba)
      ) %>%
      filter(!is.na(pr_obs), !is.na(mes))
  })

  bind_rows(Filter(Negate(is.null), registros))
}

df_obs_diario <- ler_xlsx_ceapla(arquivo_observado)

df_obs <- df_obs_diario %>%
  group_by(ano, mes) %>%
  summarise(pr_obs = sum(pr_obs, na.rm = TRUE), .groups = "drop") %>%
  mutate(data = sprintf("%04d-%02d", ano, mes))

# ---- 3c. Merge --------------------------------------------------------------
df <- inner_join(df_obs, df_sat %>% select(data, pr_sat), by = "data") %>%
  filter(!is.na(pr_obs), !is.na(pr_sat)) %>%
  arrange(ano, mes) %>%
  mutate(
    data_dt = ym(data),
    estacao = case_when(
      mes %in% c(12, 1, 2) ~ "Verao (DJF)",
      mes %in% c(3, 4, 5) ~ "Outono (MAM)",
      mes %in% c(6, 7, 8) ~ "Inverno (JJA)",
      mes %in% c(9, 10, 11) ~ "Primavera (SON)"
    ),
    residuo = pr_sat - pr_obs,
    mes_lab = factor(month.abb[mes], levels = month.abb)
  )

cat("\n=== Periodo analisado:", min(df$data_dt), "a", max(df$data_dt), "===\n")
cat("Total de meses:", nrow(df), "\n\n")

# =============================================================================
# --- 4. METRICAS DE ACURACIA ------------------------------------------------
# =============================================================================

obs <- df$pr_obs
sim <- df$pr_sat

calcular_metricas <- function(obs, sim, label = "Geral") {
  n <- length(obs)
  bias <- mean(sim - obs)
  pbias_val <- 100 * sum(sim - obs) / sum(obs)
  mae_val <- mean(abs(sim - obs))
  rmse_val <- sqrt(mean((sim - obs)^2))
  nrmse <- rmse_val / mean(obs) * 100
  r2_val <- cor(obs, sim)^2
  nse_val <- NSE(sim, obs)
  kge_val <- KGE(sim, obs)
  rsr_val <- rmse_val / sd(obs)

  tibble(
    Conjunto = label,
    N = n,
    Bias_mm = round(bias, 2),
    PBIAS = round(pbias_val, 2),
    MAE_mm = round(mae_val, 2),
    RMSE_mm = round(rmse_val, 2),
    NRMSE_pct = round(nrmse, 2),
    RSR = round(rsr_val, 3),
    R2 = round(r2_val, 4),
    NSE = round(nse_val, 4),
    KGE = round(kge_val, 4)
  )
}

metricas_geral <- calcular_metricas(obs, sim, "Geral")

metricas_estacao <- df %>%
  group_by(estacao) %>%
  summarise(
    res = list(calcular_metricas(pr_obs, pr_sat, estacao[1])),
    .groups = "drop"
  ) %>%
  pull(res) %>%
  bind_rows()

metricas_ano <- df %>%
  group_by(ano) %>%
  filter(n() >= 6) %>%
  summarise(
    res = list(calcular_metricas(pr_obs, pr_sat, as.character(ano[1]))),
    .groups = "drop"
  ) %>%
  pull(res) %>%
  bind_rows()

cat("========== METRICAS GERAIS ==========\n")
print(as.data.frame(metricas_geral))

cat("\n========== METRICAS POR ESTACAO ==========\n")
print(as.data.frame(metricas_estacao))

cat("\n========== METRICAS ANUAIS (resumo) ==========\n")
print(as.data.frame(metricas_ano %>%
                      select(Conjunto, PBIAS, R2, NSE, KGE, RMSE_mm)))

write.csv(bind_rows(metricas_geral, metricas_estacao), "metricas_acuracia.csv",
          row.names = FALSE)
write.csv(metricas_ano, "metricas_acuracia_anuais.csv", row.names = FALSE)

# =============================================================================
# --- 5. GRAFICOS ------------------------------------------------------------
# =============================================================================

tema_base <- theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    panel.grid.minor = element_blank()
  )

cores_estacao <- c(
  "Verao (DJF)" = "#E63946",
  "Outono (MAM)" = "#F4A261",
  "Inverno (JJA)" = "#457B9D",
  "Primavera (SON)" = "#2A9D8F"
)

# ---- 5.1 Serie temporal -----------------------------------------------------
p1 <- ggplot(df, aes(x = data_dt)) +
  geom_line(aes(y = pr_obs, color = "Observado (CEAPLA)"), linewidth = 0.6) +
  geom_line(aes(y = pr_sat, color = "Satelite"), linewidth = 0.6, linetype = "dashed") +
  scale_color_manual(values = c("Observado (CEAPLA)" = "#1d3557", "Satelite" = "#e63946")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "Serie Temporal de Precipitacao Mensal",
    subtitle = "Rio Claro - SP | 1996-2025",
    x = NULL, y = "Precipitacao (mm)", color = NULL
  ) +
  tema_base

# ---- 5.2 Dispersao geral com metricas --------------------------------------
lim <- range(c(obs, sim)) * c(0.95, 1.05)

ann_txt <- sprintf(
  "R2 = %.3f\nNSE = %.3f\nKGE = %.3f\nPBIAS = %.1f%%\nRMSE = %.1f mm",
  metricas_geral$R2, metricas_geral$NSE, metricas_geral$KGE,
  metricas_geral$PBIAS, metricas_geral$RMSE_mm
)

p2 <- ggplot(df, aes(x = pr_obs, y = pr_sat, color = estacao)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8, aes(group = 1)) +
  scale_color_manual(values = cores_estacao) +
  coord_equal(xlim = lim, ylim = lim) +
  annotate(
    "text",
    x = lim[1] + 0.02 * diff(lim),
    y = lim[2] - 0.02 * diff(lim),
    label = ann_txt,
    hjust = 0, vjust = 1, size = 3.5,
    family = "mono", color = "black"
  ) +
  labs(
    title = "Dispersao: Observado vs. Satelite",
    subtitle = "Linha tracejada = 1:1 | Linha solida = regressao",
    x = "Precipitacao Observada (mm)",
    y = "Precipitacao Satelite (mm)",
    color = "Estacao"
  ) +
  tema_base

# ---- 5.3 Ciclo anual medio --------------------------------------------------
ciclo <- df %>%
  group_by(mes) %>%
  summarise(
    obs_mean = mean(pr_obs), obs_sd = sd(pr_obs),
    sat_mean = mean(pr_sat), sat_sd = sd(pr_sat),
    .groups = "drop"
  ) %>%
  mutate(mes_lab = factor(month.abb[mes], levels = month.abb))

ciclo_long <- ciclo %>%
  select(mes_lab, obs_mean, sat_mean) %>%
  pivot_longer(-mes_lab, names_to = "fonte", values_to = "media") %>%
  mutate(
    fonte = recode(fonte, obs_mean = "Observado", sat_mean = "Satelite"),
    sd = ifelse(
      fonte == "Observado",
      ciclo$obs_sd[match(mes_lab, ciclo$mes_lab)],
      ciclo$sat_sd[match(mes_lab, ciclo$mes_lab)]
    )
  )

p3 <- ggplot(ciclo_long, aes(x = mes_lab, y = media, fill = fonte, group = fonte)) +
  geom_ribbon(aes(ymin = pmax(0, media - sd), ymax = media + sd), alpha = 0.15) +
  geom_line(aes(color = fonte), linewidth = 1) +
  geom_point(aes(color = fonte), size = 2.5) +
  scale_fill_manual(values = c("Observado" = "#1d3557", "Satelite" = "#e63946")) +
  scale_color_manual(values = c("Observado" = "#1d3557", "Satelite" = "#e63946")) +
  labs(
    title = "Ciclo Anual Medio",
    subtitle = "Media +/- 1 desvio-padrao por mes",
    x = NULL, y = "Precipitacao (mm)", color = NULL, fill = NULL
  ) +
  tema_base

# ---- 5.4 PBIAS por ano ------------------------------------------------------
p4 <- ggplot(metricas_ano, aes(
  x = as.integer(Conjunto),
  y = PBIAS,
  fill = ifelse(PBIAS >= 0, "Superestima", "Subestima")
)) +
  geom_col(color = "white", width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_hline(yintercept = c(-25, 25), linetype = "dashed", color = "orange", linewidth = 0.7) +
  scale_fill_manual(values = c("Superestima" = "#e63946", "Subestima" = "#457b9d")) +
  labs(
    title = "PBIAS Anual do Satelite",
    subtitle = "Linhas tracejadas = limite +/-25% (criterio satisfatorio)",
    x = "Ano", y = "PBIAS (%)", fill = NULL
  ) +
  tema_base

# ---- 5.5 Residuos ao longo do tempo ----------------------------------------
p5 <- ggplot(df, aes(x = data_dt, y = residuo, color = residuo > 0)) +
  geom_hline(yintercept = 0, linewidth = 0.8, color = "black") +
  geom_segment(aes(xend = data_dt, yend = 0), alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.8) +
  scale_color_manual(
    values = c(`TRUE` = "#e63946", `FALSE` = "#457b9d"),
    labels = c(`TRUE` = "Superestima", `FALSE` = "Subestima")
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "Residuos Mensais (Satelite - Observado)",
    subtitle = "Vermelho = satelite superestima | Azul = subestima",
    x = NULL, y = "Residuo (mm)", color = NULL
  ) +
  tema_base

# ---- 5.6 Boxplot de residuos por mes do ano --------------------------------
p6 <- ggplot(df, aes(x = mes_lab, y = residuo, fill = mes_lab)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  labs(
    title = "Distribuicao dos Residuos por Mes",
    subtitle = "Mediana, IQR e outliers",
    x = NULL, y = "Residuo (mm)"
  ) +
  tema_base

# =============================================================================
# --- 6. EXPORTAR GRAFICOS ---------------------------------------------------
# =============================================================================

salvar <- function(plot, nome, w = 10, h = 6) {
  ggsave(nome, plot = plot, width = w, height = h, dpi = 150)
  message("Salvo: ", nome)
}

salvar(p1, "01_serie_temporal.png", 14, 5)
salvar(p2, "02_dispersao.png", 7, 7)
salvar(p3, "03_ciclo_anual.png", 9, 5)
salvar(p4, "04_pbias_anual.png", 10, 5)
salvar(p5, "05_residuos_temporais.png", 14, 5)
salvar(p6, "06_boxplot_residuos_mes.png", 9, 5)

painel <- (p1 / p3) | (p2 / p4)
ggsave("00_painel_principal.png", plot = painel, width = 16, height = 11, dpi = 150)
message("Painel salvo: 00_painel_principal.png")

cat("\n=== CONCLUSAO ===\n")
cat(sprintf(
  "PBIAS = %.1f%% -> Satelite %s a precipitacao em %.1f%%\n",
  metricas_geral$PBIAS,
  ifelse(metricas_geral$PBIAS > 0, "SUPERestima", "SUBestima"),
  abs(metricas_geral$PBIAS)
))
cat(sprintf(
  "NSE   = %.3f -> %s\n",
  metricas_geral$NSE,
  ifelse(metricas_geral$NSE > 0.65, "Bom",
         ifelse(metricas_geral$NSE > 0.5, "Satisfatorio", "Insatisfatorio"))
))
cat(sprintf(
  "KGE   = %.3f -> %s\n",
  metricas_geral$KGE,
  ifelse(metricas_geral$KGE > 0.75, "Bom",
         ifelse(metricas_geral$KGE > 0.5, "Satisfatorio", "Insatisfatorio"))
))
cat(sprintf("R2    = %.3f\n", metricas_geral$R2))
cat(sprintf("RMSE  = %.1f mm\n", metricas_geral$RMSE_mm))
cat(sprintf("MAE   = %.1f mm\n", metricas_geral$MAE_mm))
