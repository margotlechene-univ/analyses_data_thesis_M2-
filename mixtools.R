library(dplyr)
library(purrr)
library(mixtools)
library(pROC)

library(ggplot2)
#cap_lf suit une distribution normale dans chaque groupe ?
df_TRI %>%
  filter(!is.na(age_groupe), !is.na(cap_lf)) %>%
  ggplot(aes(sample = cap_lf)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(annee ~ age_groupe) +
  theme_bw()

library(dplyr)
library(purrr)
library(mixtools)
library(pROC)
# MIXTURE MODEL (2 groupes) PAR ANNÉE
# cap_lf ~ mélange de 2 normales

res_mix <- map_dfr(sort(unique(df_TRI$annee)), function(a) {
  
  # 1) Données de l'année a
  df_a <- df_TRI %>%
    filter(annee == a) %>%
    filter(!is.na(cap_lf))
  
  # Sécurité : si trop peu de données, on ignore
  if(nrow(df_a) < 50) return(NULL)
  
  # 2) Fit du mélange de 2 normales sur cap_lf
  # -> EM estime : mu1, mu2, sigma1, sigma2, proportions
  mix <- normalmixEM(df_a$cap_lf, k = 2, maxit = 2000)
  
  # 3) Probabilité d'être dans le groupe 2
  df_a <- df_a %>%
    mutate(
      proba_g2 = mix$posterior[,2],
      groupe_mix = ifelse(proba_g2 >= 0.5, 2, 1)
    )
  
  # 4) Pour éviter que les groupes s'inversent selon l'année :
  # on force groupe 2 = celui qui a la plus grande moyenne de cap_lf
  if(mix$mu[1] > mix$mu[2]) {
    df_a <- df_a %>%
      mutate(
        proba_g2 = mix$posterior[,1],
        groupe_mix = ifelse(proba_g2 >= 0.5, 2, 1)
      )
    mu1 <- mix$mu[2]
    mu2 <- mix$mu[1]
  } else {
    mu1 <- mix$mu[1]
    mu2 <- mix$mu[2]
  }
  
  # 5) Comparaison avec age_groupe (uniquement là où il est connu)
  df_known <- df_a %>%
    filter(!is.na(age_groupe)) %>%
    mutate(age_groupe = trimws(as.character(age_groupe))) %>%
    mutate(age_num = ifelse(age_groupe == "1", 1, 0))
  
  # Si on n'a pas les 2 classes, on ne calcule pas accuracy/AUC
  if(nrow(df_known) == 0 || n_distinct(df_known$age_num) < 2) {
    return(tibble(
      annee = a,
      n_total = nrow(df_a),
      n_known = nrow(df_known),
      mu1 = mu1,
      mu2 = mu2,
      separation = mu2 - mu1,
      accuracy = NA_real_,
      auc = NA_real_
    ))
  }
  
  # On suppose : groupe_mix 2 correspond à age_groupe = 1
  df_known <- df_known %>%
    mutate(pred_age1 = ifelse(groupe_mix == 2, 1, 0))
  
  # Accuracy (taux de bonnes classifications)
  acc <- mean(df_known$pred_age1 == df_known$age_num)
  
  # AUC (qualité de séparation)
  roc_obj <- roc(df_known$age_num, df_known$proba_g2)
  auc_val <- as.numeric(auc(roc_obj))
  
  # 6) Résumé pour cette année
  tibble(
    annee = a,
    n_total = nrow(df_a),
    n_known = nrow(df_known),
    mu1 = mu1,
    mu2 = mu2,
    separation = mu2 - mu1,
    accuracy = acc,
    auc = auc_val
  )
})

# Affichage du tableau final
res_mix
#sachant que : 
  #mu1 et mu2 sont moyennes estimées des 2 groupes : si mu2 - mu1 est grand (2015 : 16.6) (vs sigma) alors oui 2 groupes anturels
  #l'AUC est proche de 1 : le non supervisé retrouve la séparation liée à l'âge --> bonne qualité de séparation 
  #accurary : si je prends le cluster mixture, et je l’associe à age_groupe connu, quel % tombe dans le bon groupe ?
  #Ici souvent souvent > 0.97 --> le clustering non supervisé retrouve presque exactement tes labels d’âge.

# A tibble: 11 × 8
# annee n_total n_known   mu1   mu2 separation accuracy   auc
# <chr>   <int>   <int> <dbl> <dbl>      <dbl>    <dbl> <dbl>
#   1 2015      196     191  72.2  111.       39.1    0.984 1    
# 2 2016      483     479  93.0  113.       19.8    0.827 0.989
# 3 2017      211     209  83.6  122.       38.8    0.976 0.997
# 4 2018     1168    1088  73.8  130.       55.9    1     1    
# 5 2019      339     319  71.8  113.       41.0    0.969 0.998
# 6 2020      796     737  72.8  124.       51.5    0.999 1    
# 7 2021      468     423  84.6  124.       39.8    0.981 0.999
# 8 2022      773     714  69.4  123.       53.9    0.999 1    
# 9 2023      199     197  81.6  127.       45.2    0.990 1    
# 10 2024       61      57 121.   134.       12.8    0.614 0.968
# 11 2025      223      32  84.2  142.       57.6    0.969 1 
