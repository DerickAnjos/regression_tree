# Instalando os pacotes necessários

pacotes <- c('titanic',    # carrega a base original titanic_treino 
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'viridis',    # Escalas 'viridis' para o ggplot2
             'AMR',
             'randomForest',
             'fastDummies',
             #'rattle', -> esse pacote foi excluído do CRAN, portanto agora, 
             # para utilizá-lo, é necessário instalar manualmente.
             'xgboost', 
             'ggpubr'
             )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
