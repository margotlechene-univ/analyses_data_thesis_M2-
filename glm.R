library(ggplot2)
library(tidyverse)

# Est-ce que la relation taille-âge varie selon l'année et/ou le secteur ?

# 1. DISTRIBUTION DES TAILLES PAR ÂGE ET ANNÉE


# Graphique : Fréquence vs Taille, coloré par âge, facettes par année
p1 <- df_TRI %>%
  filter(!is.na(age_num)) %>%
  ggplot(aes(x = cap_lf, fill = factor(age_num))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~annee, ncol = 2) +
  labs(
    title = "Distribution des tailles par âge et année",
    x = "Taille (cm)",
    y = "Fréquence",
    fill = "Âge"
  ) +
  theme_minimal()

print(p1)

# INTERPRÉTATION :
# - Si les distributions se décalent selon l'année → Effet année significatif
# - Si les pics restent aux mêmes tailles → Pas d'effet année



# DISTRIBUTION DES TAILLES PAR ÂGE ET SECTEUR


p2 <- df_TRI %>%
  filter(!is.na(age_num)) %>%
  ggplot(aes(x = cap_lf, fill = factor(age_num))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~sec_code, ncol = 3) +
  labs(
    title = "Distribution des tailles par âge et secteur",
    x = "Taille (cm)",
    y = "Fréquence",
    fill = "Âge"
  ) +
  theme_minimal()

print(p2)

# INTERPRÉTATION :
# - Si les distributions varient beaucoup entre secteurs → Effet secteur significatif
# - Si similaires → Pas besoin de stratifier par secteur



#  BOXPLOTS : Taille moyenne par âge selon année


p3 <- df_TRI %>%
  filter(!is.na(age_num)) %>%
  ggplot(aes(x = factor(age_num), y = cap_lf, fill = factor(annee))) +
  geom_boxplot() +
  labs(
    title = "Taille par âge et année",
    x = "Âge",
    y = "Taille (cm)",
    fill = "Année"
  ) +
  theme_minimal()

print(p3)

# INTERPRÉTATION :
# - Si les boxplots d'un même âge varient selon l'année → Effet année
# - Exemple : Âge 1 en 2018 = 15 cm, en 2020 = 17 cm → Effet année !



# REGROUPER LES SECTEURS (si trop nombreux)

# Combien de secteurs ?
cat("Nombre de secteurs :", n_distinct(df_TRI$sec_code), "\n")
print(table(df_TRI$sec_code))

# Si > 10 secteurs, regrouper arbitrairement en amont/aval


df_TRI <- df_TRI %>%
  mutate(
    zone = case_when(
      sec_code %in% c("RR01", "RR02", "RR03", "RR04", "RR05", "RR06" ) ~ "Aval",
      sec_code %in% c("RR07", "RR08", "RR09", "RR10", "RR11", "RR12") ~ "Milieu",
      TRUE ~ "Amont"
    )
  )

# Graphique avec zones regroupées
p4 <- df_TRI %>%
  filter(!is.na(age_num)) %>%
  ggplot(aes(x = cap_lf, fill = factor(age_num))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~zone) +
  labs(
    title = "Distribution des tailles par âge et zone",
    x = "Taille (cm)",
    y = "Fréquence",
    fill = "Âge"
  ) +
  theme_minimal()

print(p4)


# GRAPHIQUE : Tous les saumons (avec et sans âge)


# Préparer les données
df_visu <- df_TRI %>%
  mutate(
    statut = if_else(is.na(age_num), "Âge inconnu", "Âge connu"),
    age_display = if_else(is.na(age_num), "?", as.character(age_num))
  )

# Graphique de fréquence
p_manuel <- ggplot(df_visu, aes(x = cap_lf)) +
  # Histogramme des âges connus (coloré par âge)
  geom_histogram(
    data = df_visu %>% filter(statut == "Âge connu"),
    aes(fill = factor(age_num)),
    alpha = 0.6,
    bins = 50,
    position = "identity"
  ) +
  # Points pour les âges inconnus (en noir au-dessus)
  geom_point(
    data = df_visu %>% filter(statut == "Âge inconnu"),
    aes(y = 0),
    color = "black",
    size = 2,
    alpha = 0.5,
    position = position_jitter(height = 5)
  ) +
  labs(
    title = "Distribution des tailles : âges connus (couleur) vs inconnus (points noirs)",
    x = "Taille (cm)",
    y = "Fréquence",
    fill = "Âge",
    caption = "Points noirs = saumons sans âge à assigner"
  ) +
  theme_minimal()

print(p_manuel)

# INTERPRÉTATION :
# - Si les points noirs tombent clairement dans une zone d'âge → Assignation évidente
# - Si dans les zones de chevauchement → Besoin d'un modèle statistique
```

# **Exemple :
#   ```
# Fréquence
# |     ╭─╮              ╭──╮           ╭─╮
# |    ╱   ╲            ╱    ╲         ╱   ╲
# |   ╱ Âge0╲   • •    ╱ Âge1 ╲    •  ╱Âge2 ╲
# |__╱_______╲________╱________╲_____╱_______╲___
# 12       15  •  17        20    22       25  Taille (cm)
# ↑
# Saumon ambigu (entre âge 0 et 1)

# ═══════════════════════════════════════════════════════════════════════════════
# GLM : Tester l'effet année et secteur
library(MASS)  # Pour polr (ordered logistic regression)

# Préparer les données (seulement âges connus)
df_glm <- df_TRI %>%
  filter(!is.na(age_num)) %>%
  mutate(
    age_ordered = ordered(age_num),  # Âge ordonné (0 < 1 < 2 < 3)
    annee_factor = factor(annee),
    zone_factor = factor(zone)
  )


# MODÈLE 1 : Global (sans stratification)
# ═══════════════════════════════════════════════════════════════════════════════

glm1 <- polr(age_ordered ~ cap_lf, data = df_glm, Hess = TRUE)

cat("\n=== MODÈLE 1 : GLOBAL (taille seule) ===\n")
summary(glm1)
AIC_1 <- AIC(glm1)
cat("AIC :", AIC_1, "\n")

#AIC : 793.376 


# MODÈLE 2 : Avec effet année
# ═══════════════════════════════════════════════════════════════════════════════

glm2 <- polr(age_ordered ~ cap_lf + annee_factor, data = df_glm, Hess = TRUE)

cat("\n=== MODÈLE 2 : TAILLE + ANNÉE ===\n")
summary(glm2)
AIC_2 <- AIC(glm2)
cat("AIC :", AIC_2, "\n")

#AIC : 392.3747
# Test du rapport de vraisemblance (année significative ?)
anova(glm1, glm2)



# MODÈLE 3 : Avec effet zone (secteurs regroupés)
# ═══════════════════════════════════════════════════════════════════════════════

glm3 <- polr(age_ordered ~ cap_lf + zone_factor, data = df_glm, Hess = TRUE)

cat("\n=== MODÈLE 3 : TAILLE + ZONE ===\n")
summary(glm3)
AIC_3 <- AIC(glm3)
cat("AIC :", AIC_3, "\n")

#AIC : 792.2281 

# Test du rapport de vraisemblance (zone significative ?)
anova(glm1, glm3)



# MODÈLE 4 : Avec année ET zone
# ═══════════════════════════════════════════════════════════════════════════════

glm4 <- polr(age_ordered ~ cap_lf + annee_factor + zone_factor, 
             data = df_glm, Hess = TRUE)

cat("\n=== MODÈLE 4 : TAILLE + ANNÉE + ZONE ===\n")
summary(glm4)
AIC_4 <- AIC(glm4)
cat("AIC :", AIC_4, "\n")

#AIC : 392.142 

# Test complet
anova(glm1, glm2, glm3, glm4)



# MODÈLE 5 : Avec interaction année × zone
# ═══════════════════════════════════════════════════════════════════════════════

glm5 <- polr(age_ordered ~ cap_lf + annee_factor * zone_factor, 
             data = df_glm, Hess = TRUE)
#age:zone
cat("\n=== MODÈLE 5 : TAILLE + ANNÉE × ZONE (interaction) ===\n")
summary(glm5)
AIC_5 <- AIC(glm5)
cat("AIC :", AIC_5, "\n")
#AIC : 393.1753 


# COMPARAISON FINALE DES MODÈLES
# ═══════════════════════════════════════════════════════════════════════════════

comparaison <- tibble(
  Modèle = c("Global", "Taille + Année", "Taille + Zone", 
             "Taille + Année + Zone", "Taille + Année × Zone"),
  AIC = c(AIC_1, AIC_2, AIC_3, AIC_4, AIC_5),
  Delta_AIC = AIC - min(AIC)
)

print(comparaison %>% arrange(AIC))

# cat("\nINTERPRÉTATION :\n")
# cat("- Delta AIC < 2 : Modèles équivalents\n")
# cat("- Delta AIC 2-7 : Support modéré pour le meilleur modèle\n")
# cat("- Delta AIC > 10 : Fort support pour le meilleur modèle\n\n")

meilleur_modele <- comparaison %>% 
  filter(AIC == min(AIC)) %>% 
  pull(Modèle)

cat("→ MEILLEUR MODÈLE :", meilleur_modele, "\n")

if (meilleur_modele == "Global") {
  cat("→ CONCLUSION : Pas besoin de stratifier (modèle global suffit)\n")
} else if (meilleur_modele == "Taille + Année") {
  cat("→ CONCLUSION : Stratifier par ANNÉE seulement\n")
} else if (meilleur_modele == "Taille + Zone") {
  cat("→ CONCLUSION : Stratifier par ZONE seulement\n")
} else {
  cat("→ CONCLUSION : Stratifier par ANNÉE et ZONE\n")
}

# Modèle                  AIC Delta_AIC
# <chr>                 <dbl>     <dbl>
#   1 Taille + Année + Zone  392.     0    
# 2 Taille + Année         392.     0.233
# 3 Taille + Année × Zone  393.     1.03 
# 4 Taille + Zone          792.   400.   
# 5 Global                 793.   401

# Test formel : Zone apporte-t-elle quelque chose au-delà de Année ?

glm_annee <- polr(age_ordered ~ cap_lf + annee_factor, data = df_glm, Hess = TRUE)
glm_annee_zone <- polr(age_ordered ~ cap_lf + annee_factor + zone_factor, 
                       data = df_glm, Hess = TRUE)

# Test du rapport de vraisemblance
test_lr <- anova(glm_annee, glm_annee_zone)
print(test_lr)

# Si p > 0.05 → Zone n'apporte PAS d'amélioration significative = 0.1204718


# DIAGNOSTIC : Effectifs par strate
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════════════\n")
cat("VÉRIFICATION DES EFFECTIFS PAR STRATE\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Option 1 : Année + Zone
effectifs_annee_zone <- df_TRI %>%
  group_by(annee, zone) %>%
  summarise(
    n = n(),
    nb_ages = n_distinct(age_num),
    modele_possible = (n >= 10 & nb_ages >= 2),
    .groups = "drop"
  ) %>%
  arrange(n)

cat("─── STRATIFICATION ANNÉE × ZONE ───\n")
print(effectifs_annee_zone)

prop_ok_annee_zone <- mean(effectifs_annee_zone$modele_possible) * 100
cat("\nStrates avec données suffisantes :", 
    round(prop_ok_annee_zone, 1), "%\n\n")

# Option 2 : Année seulement
effectifs_annee <- df_TRI %>%
  group_by(annee) %>%
  summarise(
    n = n(),
    nb_ages = n_distinct(age_num),
    modele_possible = (n >= 10 & nb_ages >= 2),
    .groups = "drop"
  ) %>%
  arrange(n)

cat("─── STRATIFICATION ANNÉE SEULEMENT ───\n")
print(effectifs_annee)

prop_ok_annee <- mean(effectifs_annee$modele_possible) * 100
cat("\nStrates avec données suffisantes :", 
    round(prop_ok_annee, 1), "%\n\n")

# Recommandation
cat("═══════════════════════════════════════════════════════\n")
cat("RECOMMANDATION :\n")

if (prop_ok_annee_zone < 80 & prop_ok_annee >= 80) {
  cat("→ Utiliser ANNÉE SEULEMENT\n")
  cat("  Raison : Trop de strates année×zone ont < 10 observations\n")
} else if (prop_ok_annee_zone >= 80) {
  cat("→ Vous POUVEZ utiliser ANNÉE × ZONE\n")
  cat("  Mais ANNÉE seule est plus parcimonieux (Delta AIC = 0.2)\n")
} else {
  cat("⚠️  ATTENTION : Même ANNÉE seule pose problème\n")
  cat("  → Envisager modèle global avec fallback\n")
}
cat("═══════════════════════════════════════════════════════\n")

