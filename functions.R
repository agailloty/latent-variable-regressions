statistiques_desc <- function(data){
  data <- data[sapply(data, is.numeric)]
  colonnes <- colnames(data)
  
  vecteur_des_moyennes <- vector(mode = "numeric")
  for (col in colonnes){
    vecteur_des_moyennes[col] <- mean(data[[col]], na.rm = TRUE)
  }
  
  vecteur_des_ecart_types <- vector(mode = "numeric")
  for (col in colonnes){
    vecteur_des_ecart_types[col] <- sd(data[[col]], na.rm = TRUE)
  }
  
  vecteur_des_min <- vector(mode = "numeric")
  for (col in colonnes){
    vecteur_des_min[col] <- min(data[[col]], na.rm = TRUE)
  }
  
  vecteur_des_max <- vector(mode = "numeric")
  data <- data[sapply(data, is.numeric)]
  for (col in colonnes){
    vecteur_des_max[col] <- max(data[[col]], na.rm = TRUE)
    
  }
  
  vecteur_des_medianes <- vector(mode = "numeric")
  for (col in colonnes){
    vecteur_des_medianes[col] <- median(data[[col]], na.rm =TRUE)
  }
  
  #missings <- vector(mode = "numeric")
  #for (col in colonnes){
  #missings[col] <- sum(is.na(data[[col]]))
  #}
  
  
  data.frame(Min = vecteur_des_min, Moyenne = vecteur_des_moyennes, 
             Ecart_type = vecteur_des_ecart_types, Max = vecteur_des_max,
             Médiane = vecteur_des_medianes)
}


add_stars <- function(p, cutoff = c(0.1, 0.05, 0.01, 0.001)){
  ifelse(p > cutoff[1], "", 
         ifelse(p > cutoff[2] & p< cutoff[1], ".",
         ifelse(p < cutoff[2], "*",
         ifelse(p > cutoff[3] & p <= cutoff[2], "**", 
         ifelse(p < cutoff[4], "***"))))
         )
  
  
}