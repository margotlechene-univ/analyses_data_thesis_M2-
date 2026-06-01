library(ggplot2)
library(dplyr)
library(tidyr)

# Proportions annuelles observees a La Roche
# (a lire dans ton Appendix E ou tes donnees brutes)
sigma_0_obs <- c(0.34, 0.14, 0.11, 0.09, 0.05, 0.10, 0.28, 0.08, 0.19, 0.29, 0.10)
sigma_1_obs <- c(0.49, 0.48, 0.45, 0.43, 0.37, 0.34, 0.65, 0.27, 0.24, 0.83, 1.00)

# Regression lineaire forcee par l'origine (sans intercept)
# car si sigma_0 = 0 alors sigma_1 = 0 logiquement
mod <- lm(sigma_1_obs ~ 0 + sigma_0_obs)
ratio_s1_estimated <- coef(mod)["sigma_0_obs"]
cat("ratio_s1 estimé =", round(ratio_s1_estimated, 3), "\n")
cat("R² =", round(summary(mod)$r.squared, 3), "\n")

# Visualisation
plot(sigma_0_obs, sigma_1_obs,
     xlab="sigma_0 (proportion matures 0+)",
     ylab="sigma_1 (proportion matures 1+)",
     main="Relation sigma_0 ~ sigma_1 — La Roche 2015-2025")
abline(mod, col="blue")
# =============================================================
# FONCTIONS DE BASE
# =============================================================

get_lambda <- function(M) max(Re(eigen(M)$values))

build_leslie <- function(s0,
                         s_01=0.53,      # Bal et al. 2011; Marchand et al. 2017
                         s_12=0.50,      # Buoro et al. 2010; Marchand et al. 2017
                         s_sm=0.07,      # Rouault et al. 2003; Servanty & Prevost 2016
                         s_ad=0.05,      # Fleming 1996; Klemetsen et al. 2003
                         F_adult=62.5,   # Rouault et al. 2003
                         p_smolt=0.80,   # Myers 1984; Marchand et al. 2017
                         ratio_s1=2.519 ,  # estimé par régression sur données 2015-2025
                         sneak_repro=TRUE) {  # SCENARIO B par defaut
  s0  <- as.numeric(s0)
  s1  <- min(s0 * ratio_s1, 0.99)
  # Scenario B : sneakers contribuent a la fecondite
  # ratio reproductif sneaker/anadrome = 0.082 (Tentelier et al. 2016)
  # = 2.24 descendants/sneaker vs 27.17/anadrome
  Fef <- F_adult * (1 + s0 * 0.082)
  M   <- matrix(0.0, 4, 4)
  M[1,4] <- Fef        # adultes -> parr 0+
  M[2,1] <- s_01*(1-s0)          # parr 0+ -> parr 1+
  M[3,2] <- s_12*(1-s1)*p_smolt  # parr 1+ -> smolt
  M[4,3] <- s_sm                  # smolt -> adulte
  M[4,4] <- s_ad                  # survie adulte
  M
}

# Test rapide
M_test <- build_leslie(0.118)
cat("=== TEST ===\n")
cat("lambda La Roche (sigma=11.8%, Sc.B):", round(get_lambda(M_test), 4), "\n")
cat("Matrice:\n"); print(round(M_test, 4))

# Sequence sigma_0
sigma_seq <- seq(0.0, 0.95, by=0.005)

# =============================================================
# FIG 1 — LAMBDA vs SIGMA_0 (Scenario B uniquement)
# =============================================================

lB <- vapply(sigma_seq,
             function(s) get_lambda(build_leslie(s)),
             numeric(1))

# Seuil critique sigma_0* (lambda = 1)
cr <- which(diff(sign(lB - 1.0)) != 0)
ssB <- if(length(cr)==0) NA else sigma_seq[cr[1]]
cat("\nsigma_0* Scenario B =",
    if(is.na(ssB)) "none (lambda < 1 for all sigma)" else paste0(ssB*100, "%"), "\n")

lB_obs <- get_lambda(build_leslie(0.118))
cat("lambda La Roche (11.8%) =", round(lB_obs, 4), "\n")

df_B <- data.frame(
  sigma  = sigma_seq * 100,
  lambda = lB
)

p1 <- ggplot(df_B, aes(x=sigma, y=lambda)) +
  geom_line(color="#56B4E9", linewidth=1.3) +
  geom_hline(yintercept=1.0, linetype="dashed",
             color="red", linewidth=0.9) +
  # zone stable si sigma_0* existe
  {if(!is.na(ssB))
    annotate("rect", xmin=0, xmax=ssB*100,
             ymin=-Inf, ymax=Inf, alpha=0.08, fill="green")} +
  # ligne verticale La Roche
  geom_vline(xintercept=11.8, linetype="dotted",
             color="darkgreen", linewidth=0.8) +
  # point La Roche
  annotate("point", x=11.8, y=lB_obs,
           color="darkgreen", size=3.5) +
  annotate("text", x=14, y=lB_obs+0.03,
           label=paste0("La Roche (11.8%)\nlambda = ",
                        round(lB_obs, 3)),
           color="darkgreen", size=3.5, hjust=0) +
  # annotation seuil si existant
  {if(!is.na(ssB))
    annotate("text", x=ssB*100+0.5, y=0.60,
             label=paste0("sigma_0* ~ ", ssB*100, "%"),
             color="darkred", size=3.5, hjust=0)} +
  scale_x_continuous(labels=function(x) paste0(x,"%"),
                     limits=c(0, 95)) +
  coord_cartesian(ylim=c(0.70, 1.15)) +
  labs(
    title="Population growth rate (lambda) as a function of sneaker proportion",
    subtitle=paste0(
      "Scenario B — sneakers contribute to fecundity ",
      "(ratio = 0.082, Tentelier et al. 2016)\n",
      "Leslie matrix — 4 stages: parr 0+, parr 1+, smolt, anadromous adult"
    ),
    x="Proportion of precocious 0+ males (sigma_0)",
    y="Annual population growth rate (lambda)",
    caption=paste0(
      "Parameters: F=62.5 parr/adult | s_01=53% | s_12=50% | ",
      "s_marine=7% | s_adult=5% | p_smolt=80%\n",
      "Sneaker reprod. ratio = 0.082 (Tentelier et al. 2016: ",
      "2.24 vs 27.17 offspring/male)\n",
      "Bal et al. 2011; Buoro et al. 2010; Marchand et al. 2017; ",
      "Servanty & Prevost 2016"
    )
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(size=7.5, color="grey50"))
p1
ggsave("/mnt/user-data/outputs/figB_lambda.png",
       p1, width=10, height=6.5, dpi=150)
cat("Fig1 (lambda vs sigma) saved\n")

# =============================================================
# FIG 2 — DYNAMIQUE TEMPORELLE (Scenario B)
# =============================================================

sim_pop <- function(s0, n0=c(5000,2000,500,200), yrs=60) {
  M <- build_leslie(as.numeric(s0))
  N <- matrix(0.0, 4, yrs+1)
  N[,1] <- n0
  for(t in seq_len(yrs)) N[,t+1] <- pmax(M %*% N[,t], 0)
  data.frame(year=0:yrs, adults=N[4,], sigma_0=s0)
}

svals <- c(0.06, 0.118, 0.25, 0.40, 0.60)
slabs <- c("sigma_0=6% (visual detection)",
           "sigma_0=11.8% (La Roche observed)",
           "sigma_0=25%",
           "sigma_0=40%",
           "sigma_0=60%")
scols <- c("#009E73","#0072B2","#E69F00","#D55E00","#8B0000")
slty  <- c("dashed","solid","dashed","dashed","dashed")

dyn <- bind_rows(lapply(svals, sim_pop)) %>%
  mutate(label=factor(sigma_0, levels=svals, labels=slabs))

# Effectifs a 60 ans pour annotation
ad60 <- dyn %>%
  filter(year==60) %>%
  select(label, adults) %>%
  mutate(adults=round(adults))
cat("\nEffectifs adultes a 60 ans:\n")
print(ad60)

p2 <- ggplot(dyn, aes(x=year, y=adults,
                       color=label, linetype=label)) +
  geom_line(linewidth=1.1) +
  geom_hline(yintercept=200, linetype="dotted", color="grey50") +
  annotate("text", x=52, y=207,
           label="N0 = 200 adults", color="grey50", size=3) +
  scale_color_manual(values=scols) +
  scale_linetype_manual(values=slty) +
  labs(
    title="Population dynamics of anadromous adults by sneaker proportion",
    subtitle=paste0(
      "Scenario B — sneakers contribute to fecundity ",
      "(Tentelier et al. 2016)\n",
      "Leslie matrix — 4 stages"
    ),
    x="Year",
    y="Number of anadromous adults",
    color="Sneaker proportion (sigma_0)",
    linetype="Sneaker proportion (sigma_0)",
    caption=paste0(
      "Solid line = La Roche observed scenario\n",
      "Dashed lines = hypothetical scenarios\n",
      "Parameters: F=62.5 | s_01=53% | s_12=50% | s_marine=7%"
    )
  ) +
  theme_minimal(base_size=12) +
  theme(legend.position="right",
        plot.title=element_text(face="bold"),
        plot.caption=element_text(size=7.5, color="grey50"))
p2
ggsave("/mnt/user-data/outputs/figB_dynamics.png",
       p2, width=10, height=6, dpi=150)
cat("Fig2 (dynamics) saved\n")

# =============================================================
# FIG 3 — SENSIBILITE DE SIGMA_0* A LA SURVIE MARINE (Sc. B)
# =============================================================

sm_seq <- seq(0.01, 0.25, by=0.005)

ss_sm <- vapply(sm_seq, function(sm) {
  lv <- vapply(sigma_seq,
               function(s) get_lambda(build_leslie(s, s_sm=sm)),
               numeric(1))
  cr <- which(diff(sign(lv - 1.0)) != 0)
  if(length(cr)==0) return(NA_real_)
  sigma_seq[cr[1]]
}, numeric(1))

df_s <- data.frame(
  s_marine   = sm_seq * 100,
  sigma_star = ss_sm * 100
)

# Valeur de sigma_0* a s_marine=7%
ss_at7 <- df_s$sigma_star[which.min(abs(df_s$s_marine - 7))]
cat("\nsigma_0* a s_marine=7% (Sc.B) =",
    round(ss_at7, 1), "%\n")

p3 <- ggplot(df_s, aes(x=s_marine, y=sigma_star)) +
  geom_line(color="#0072B2", linewidth=1.3, na.rm=TRUE) +
  geom_point(color="#0072B2", size=1.5, na.rm=TRUE) +
  geom_hline(yintercept=11.8, linetype="dotted",
             color="#009E73", linewidth=0.9) +
  geom_vline(xintercept=7.0, linetype="dashed",
             color="grey50", linewidth=0.8) +
  annotate("rect", xmin=-Inf, xmax=Inf,
           ymin=0, ymax=11.8,
           alpha=0.07, fill="red") +
  annotate("text", x=21, y=12.8,
           label="sigma_0 obs. La Roche (11.8%)",
           color="#009E73", size=3.5) +
  annotate("text", x=7.4, y=4.0,
           label="s_marine\nretained (7%)",
           color="grey40", size=3) +
  annotate("text", x=19, y=3.0,
           label="Decline zone",
           color="red", size=3.2) +
  # point a s_marine=7%
  {if(!is.na(ss_at7))
    annotate("point", x=7, y=ss_at7,
             color="darkred", size=3.5)} +
  {if(!is.na(ss_at7))
    annotate("text", x=8.5, y=ss_at7+1.5,
             label=paste0("sigma_0* = ", round(ss_at7,1), "%\nat s_marine=7%"),
             color="darkred", size=3, hjust=0)} +
  scale_x_continuous(labels=function(x) paste0(x,"%")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(
    title="Sensitivity of critical threshold sigma_0* to marine survival",
    subtitle=paste0(
      "Scenario B — sneakers contribute to fecundity\n",
      "Higher marine survival -> population tolerates higher sneaker frequency"
    ),
    x="Marine survival (s_marine)",
    y="Critical sneaker threshold sigma_0* (lambda = 1)",
    caption=paste0(
      "Scenario B | Sneaker reprod. ratio = 0.082 (Tentelier et al. 2016)\n",
      "All other parameters fixed at baseline values\n",
      "Red zone: La Roche observed frequency (11.8%) exceeds the critical threshold"
    )
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(size=7.5, color="grey50"))
p3
ggsave("/mnt/user-data/outputs/figB_sensitivity_smarine.png",
       p3, width=9, height=6, dpi=150)
cat("Fig3 (sensitivity) saved\n")

# =============================================================
# RECAP FINAL
# =============================================================
cat("\n=== RECAP SCENARIO B ===\n")
cat("lambda(sigma=0)    =", round(get_lambda(build_leslie(0)), 4), "\n")
cat("lambda(sigma=11.8%)=", round(lB_obs, 4), "\n")
cat("sigma_0*           =",
    if(is.na(ssB)) "none" else paste0(ssB*100, "%"), "\n")
cat("sigma_0* a s_mar=7%=", round(ss_at7, 1), "%\n")
