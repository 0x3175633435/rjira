library('dplyr')
library('stringr')

source("~/ProjectManagement/bevicred-erp/functions/getFixedSprint.R")

#'
#' getSprints
#' 
#' Retorna sprints pelo número de pontos concluídos
#' 
#' @param path Arquivo a ser carregado
#' @examples 
#'    getSprints('mydata.csv')
#'
getSprints <- function(path) {
  
  # Carrega os dados a serem analizados
  issues <- read.csv(path)
  
  # Separa apenas as colunas que serão utilizadas
  issues <- select(issues, points, sprint, sprint.1, sprint.2, sprint.3)
  
  # Cria um novo dataset com a informação corrigida
  entries <- getFixedSprint(issues)
  
  # Apaga os issues
  issues <- NULL
  
  # Agrupa os dados no banco
  result <- aggregate(entries$p, by=list(s = entries$s), FUN=sum)
  
  # Retorna o resultado
  return(result)
  
}