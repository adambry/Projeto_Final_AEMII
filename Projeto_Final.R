library (readr)
library (dplyr)
library (tidyr)
library (tidyverse)
library (skimr)
library (ggplot2)

urlfile <- "https://raw.githubusercontent.com/nailson/ifood-data-business-analyst-test/master/ifood_df.csv"
dados <-read_csv(url(urlfile))

metadata <- dados  %>%
  lapply(type_sum) %>%
  as_tibble() %>%
  pivot_longer(cols = 1:ncol(dados),
               names_to = "Coluna",
               values_to = "Tipo") %>%
  inner_join(
    dados %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(cols = 1:ncol(dados),
                   names_to = "Coluna",
                   values_to = "Total NA")
  )


# Skim dos dados
skim(dados)


# Limpeza dos dados -------------------------------------------------------


# É visto que na 38o coluna há valores negativos, no que se refere ao valor compras realizadas na plataforma, 
# então números negativos não são aplicáveis

sum(dados[38] < 0)

# Como há apenas 3 valores negativos, eles serão removidos da base

dados <- dados %>%
            filter (dados$MntRegularProds > 0)


# Remoção das colunas "Z_CostContact" e "Z_Revenue"

col_rem <- c("Z_CostContact", "Z_Revenue")

dados <- dados %>% select(-col_rem)

# Verificação de outliers de colunas numéricas


cols <- c('Age', 'Income', 'Recency', 'NumWebVisitsMonth', 'Customer_Days',
          'MntTotal', 'MntRegularProds', 'MntWines', 'MntFruits', 'MntMeatProducts', 
          'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 'NumDealsPurchases', 
          'NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases')

# Cálculo de outliers

for (column in cols) {
  q1 <- quantile(dados[[column]], 0.25)
  q3 <- quantile(dados[[column]], 0.75)
  q95 <- quantile(dados[[column]], 0.95)
  iqr <- q3 - q1
  median <- median(dados[[column]])
  outlier_limit <- median + 1.5 * iqr
  # Count the number of values above the limit
  outlier_count <- sum(dados[[column]] > outlier_limit)
  cat(sprintf('%20s | median+1.5xiqr: %8.2f | 95quantile: %8.2f | outliers: %d\n', 
              column, outlier_limit, q95, outlier_count))
}

# Configuração de layout para exibir 4x5 boxplots
par(mfrow = c(4, 5), mar = c(2, 2, 2, 2))

#Plot dos boxplots
for (col in cols) {  
  boxplot(dados[[col]], main = col, col = "lightblue", border = "black", notch = TRUE)
}

