# Carrega dependências
library('dplyr')
library('stringr')

#'
#' Retorna pontos executados por cada usuário
#'
#' @param sprint  Nome da sprint
#' @param version Número da versão trabalhada
#' @examples
#'    getUserStoryPoints(p = 'mydata.csv', s = 'Sprint 2', v = 'v1.2.0')
#'    getUserStoryPoints(p = 'mydata.csv')
#'    
getUserStoryPoints <- function(p, s, v, u) {
  
  # Carrega informações do CSV
  issues <- read.csv(p)
  
  # Seleciona colunas a serem utilizadas
  issues <- select(issues, situacao, responsavel, sprint, points, version)
  
  # Filta elementos por sprint
  if(!missing(s)){
    
    # Agrupamento de elementos
    entries <- NULL
    
    # Seleciona elementos da sprint selecionada
    by(issues, 1:nrow(issues), function(row){
      if(row$Sprint == s) {
        entries <<- rbind(entries, data.frame(
          sprint      = row$Sprint, 
          points      = row$points,
          responsavel = row$ResponsÃ.vel,
          situacao    = ro2$SituaÃ.Ã.o)
        )
      }
    });
    
    # Define os issues
    issues <- entries
    
  }
  
  # Filtra elementos por versão
  
  return (issues)
}
