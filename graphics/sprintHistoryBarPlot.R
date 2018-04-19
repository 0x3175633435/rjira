# Dependências
source("~/ProjectManagement/bevicred-erp/functions/getSprints.R")

#'
#' sprintHistoryBarPlot
#' 
#' Gera um gráfico de barras com histórico de sprints.
#' 
#' @seealso https://i.imgur.com/gHXYzQW.png
#'
sprintHistoryBarPlot <- function(){
  # Busca as sprints
  sprints = getSprints("~/ProjectManagement/bevicred-erp/datasets/issues-19042018.csv")
  
  # Gera o gráfico com as sprints
  sprintHistoryBarPlot = barplot(sprints$x, 
          names.arg = sprints$s, 
          col       = rainbow(nrow(sprints)),
          main      = "Histórico de Sprints",
          ylab      = "Pontuação",
          xlab      = "Sprints")
  
  # Adiciona número de pontos entregues ao gráfico
  text(sprintHistoryBarPlot, sprints$x-10, sprints$x) 
}

sprintHistoryBarPlot()