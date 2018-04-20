# Carrega dependências
library('dplyr')
library('stringr')

#'
#' sprintPerformanceByUser
#' 
#' Exibe o histórico de pontuação de cada usuário por sprint
#' utilizando-se de um gráfico de linha.
#' 
#' @example sprintPerformanceByUserLineChart()
#'
sprintPerformanceByUserLineChart <- function(p) {
  
  # Carrega informações do CSV
  issues <- read.csv(p)
  
  # Seleciona colunas a serem utilizadas
  issues <- select(issues, situacao, responsavel, sprint, points, version)
  
  # Corrige os dados 
  entries <- NULL
  by(issues, 1:nrow(issues), function(row){
    if(row$responsavel != '' && row$sprint != ''){
       row$sprint <- strtoi(str_replace(row$sprint, 'Sprint ', ''))
       
       if(is.na(row$points)) {
         row$points <- 0
       }
       
       entries <<- rbind(entries, row)
    }
  });
  issues <- entries
  
  # Reordena
  issues <- issues[order(issues$sprint),]
    
  # Agrupa os dados
  result <- aggregate(
    x = issues$points, 
    by=list(
      responsavel = issues$responsavel, 
      sprint = issues$sprint
    ), 
    FUN=sum, na.rm = TRUE
  )
  
  # Transforma resultado
  result <- reshape(result, idvar = "sprint", timevar = "responsavel", direction = "wide")
  
  # colum names
  names <- colnames(result)
  
  for(i in 2:ncol(result)){
    performance <- result[, i]
    worker <- str_replace(names[i], 'x.', '')
    plot(performance, type = 'b', main = worker, xlab= 'Sprint', ylab = 'Story Points', asp = -5)
  }
}
  
sprintPerformanceByUserLineChart("~/ProjectManagement/bevicred-erp/datasets/issues-19042018.csv")
  