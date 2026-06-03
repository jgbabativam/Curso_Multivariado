
library(pacman)
options(scipen = 999)
p_load(tidyverse, janitor, haven, FactoMineR, factoextra, ggrepel)

url <- "https://github.com/jgbabativam/Curso_Multivariado/raw/main/Datos/"
microdato <- read_sav(paste0(url, "ejemploACS.sav")) |> 
  mutate(across(c("opinion", "grupo"), ~as_factor(.)))

tbl <- table(microdato$grupo, microdato$opinion)


microdato |> 
  count(grupo, opinion) |> 
  pivot_wider(names_from = opinion, values_from = n) |> 
  adorn_totals(where = c("row", "col"))


chi_test <- chisq.test(tbl)
chi_test

n     <- sum(tbl)
F_mat <- as.matrix(tbl) / n       # frecuencias relativas fij
fi_   <- rowSums(F_mat)           # marginal fila fi.
f_j   <- colSums(F_mat)           # marginal columna f.j

perf_fila <- sweep(F_mat, 1, fi_, "/")   # perfil fila: fij / fi.
perf_col <- sweep(F_mat, 2, f_j, "/")   # perfil columna: fij / f.j

d2_col <- function(j1, j2)
  sum((perf_col[, j1] - perf_col[, j2])^2 / fi_)

cat("d²(TC, Af)  =", round(d2_col("Totalmente en Contra", "A favor"), 5))

H <- F_mat - outer(fi_, f_j)   # H = F - fi.f.j
round(H, 5)


W <- sqrt(outer(fi_, f_j))   # denominador: sqrt(fi. * f.j)
Z <- H / W                   # Z matriz estandarizada

Z
