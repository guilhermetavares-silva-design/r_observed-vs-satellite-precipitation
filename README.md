# Analise de acuracia de precipitacao

Script em R para comparar precipitacao mensal observada com estimativas de satelite. O fluxo foi pensado para estudos climaticos recorrentes, especialmente quando os dados observados estao em uma planilha Excel com uma aba por ano.

## Objetivo

O script calcula metricas de desempenho entre duas series de precipitacao:

- observado, agregado a partir de dados diarios do CEAPLA;
- satelite, lido de um arquivo CSV mensal.

Tambem gera graficos para avaliar concordancia, vies, ciclo anual e residuos.

## Arquivos de entrada

Antes de rodar, coloque os arquivos na mesma pasta do script ou informe o caminho completo no bloco de configuracao:

```r
arquivo_satelite <- "la.csv"
arquivo_observado <- "Dados_1996_2025_CEAPLA.xlsx"
```

### CSV do satelite

O CSV deve conter pelo menos estas colunas:

- `data`: periodo mensal no formato `YYYY-MM`;
- `pr`: precipitacao mensal estimada pelo satelite.

### XLSX observado

A planilha Excel deve ter:

- uma aba por ano, com nomes como `1996`, `1997`, `1998`;
- uma linha de cabecalho contendo a coluna `DIA`;
- colunas mensais abreviadas em portugues: `JAN`, `FEV`, `MAR`, `ABR`, `MAI`, `JUN`, `JUL`, `AGO`, `SET`, `OUT`, `NOV`, `DEZ`.

O script soma os valores diarios para obter a precipitacao mensal observada.

## Metricas calculadas

- Bias
- PBIAS
- MAE
- RMSE
- NRMSE
- RSR
- R2
- NSE
- KGE

As metricas sao calculadas para o conjunto geral, por estacao do ano e por ano.

## Saidas geradas

### Tabelas

- `metricas_acuracia.csv`
- `metricas_acuracia_anuais.csv`

### Graficos

- `00_painel_principal.png`
- `01_serie_temporal.png`
- `02_dispersao.png`
- `03_ciclo_anual.png`
- `04_pbias_anual.png`
- `05_residuos_temporais.png`
- `06_boxplot_residuos_mes.png`

## Como executar

No R ou RStudio, defina a pasta de trabalho para a pasta do projeto e execute:

```r
source("analise_acuracia_precipitacao.R")
```

O script instala automaticamente pacotes ausentes e salva as saidas na pasta atual.

## Pacotes R utilizados

- `readxl`
- `dplyr`
- `ggplot2`
- `hydroGOF`
- `tidyr`
- `scales`
- `patchwork`
- `lubridate`
- `viridis`

## Observacoes

Este repositorio nao precisa incluir os arquivos de dados brutos caso eles sejam privados ou muito grandes. Nesse caso, mantenha apenas o script e documente a estrutura esperada dos arquivos de entrada.
