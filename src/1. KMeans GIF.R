# =========================================================
# GIF: K-means inestable (convergencia en iteración 8)
# Genera: "kmeans_inestable.gif"
# =========================================================

# install.packages(c("ggplot2","gganimate","gifski"))
library(ggplot2)
library(gganimate)
library(gifski)

set.seed(2026)

# ---------------------------
# 1) Datos más dispersos
# ---------------------------
n <- 150
k <- 3

centros_true <- matrix(c(-2,0,  0,2.5,  2,-2), ncol=2, byrow=TRUE)

X <- do.call(rbind, lapply(1:k, function(j){
  cbind(rnorm(n/k, centros_true[j,1], 0.9),
        rnorm(n/k, centros_true[j,2], 0.9))
}))

df <- data.frame(x = X[,1], y = X[,2])

# ---------------------------
# 2) Inicialización MUY mala
# ---------------------------
cent <- matrix(c(4,4,  -4,4,  4,-4), ncol=2, byrow=TRUE)

max_iter <- 12
iter_convergencia <- 8

asignar <- function(P, C){
  d2 <- sapply(1:nrow(C), function(j)
    (P[,1]-C[j,1])^2 + (P[,2]-C[j,2])^2)
  max.col(-d2)
}

frames_pts  <- list()
frames_cent <- list()

for (it in 0:max_iter) {
  
  cl <- asignar(as.matrix(df[,c("x","y")]), cent)
  df$cluster <- factor(cl)
  
  cent_df <- data.frame(
    x = cent[,1],
    y = cent[,2],
    cluster = factor(1:k)
  )
  
  df$iter <- it
  cent_df$iter <- it
  
  frames_pts[[it+1]]  <- df
  frames_cent[[it+1]] <- cent_df
  
  # --- Actualizar hasta iteración 8 ---
  if (it < iter_convergencia) {
    
    cent_new <- do.call(rbind, lapply(1:k, function(j){
      colMeans(df[df$cluster == j, c("x","y"), drop=FALSE])
    }))
    
    # Pequeña perturbación para hacerlo más "inestable"
    ruido <- matrix(rnorm(2*k, 0, 0.15), ncol=2)
    cent <- cent_new + ruido
    
  } else if (it == iter_convergencia) {
    # Convergencia limpia
    cent <- do.call(rbind, lapply(1:k, function(j){
      colMeans(df[df$cluster == j, c("x","y"), drop=FALSE])
    }))
  }
  
  # Después de 8, ya no se mueve (se estabiliza)
}

anim_pts  <- do.call(rbind, frames_pts)
anim_cent <- do.call(rbind, frames_cent)

# ---------------------------
# 3) Animación elegante
# ---------------------------
p <- ggplot(anim_pts, aes(x, y)) +
  geom_point(aes(color = cluster), size = 2.5, alpha = 0.85) +
  geom_point(
    data = anim_cent,
    aes(x, y, color = cluster),
    shape = 4, size = 8, stroke = 1.7, inherit.aes = FALSE
  ) +
  coord_equal() +
  labs(
    title = "Iteración {closest_state}",
    x = "Eje 1",
    y = "Eje 2",
    color = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  transition_states(iter, state_length = 1.2, transition_length = 2) +
  ease_aes("cubic-in-out")

anim_save(
  "images/kmeans_demo.gif",
  animate(
    p,
    fps = 15,
    width = 1800,
    height = 1000,
    res = 180,
    renderer = gifski_renderer(loop = TRUE)
  )
)

