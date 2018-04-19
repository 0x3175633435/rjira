# Carrega dependências
library('dplyr')

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
  result <- reshape(result, idvar = "responsavel", timevar = "sprint", direction = "wide")
  
  # Gráfico
  matplot(rownames(result), result, type = 'l', xlab = 'Sprint', ylab = 'Points', col  = 1:11, lwd=2, lty=1)
  
  # Legenda
  legend('bottomright', inset=.05, legend=result$responsavel, pch=1, horiz=TRUE, col=1:11)
  
}
  
sprintPerformanceByUserLineChart("~/ProjectManagement/bevicred-erp/datasets/issues-19042018.csv")
  