
#'
#' geFixedSprint
#' 
#' Corrige a sprint em que a tarefa foi entregue. Embora
#' a sprint que conte seja sempre a última o JIRA traz no
#' CSV todas as Sprints pelas quais a tarefa passou.
#' 
#' @param dataset
#'
getFixedSprint <- function(issues) {
  
  # Entries a serem retornadas
  entries <- NULL
  
  # Cria um novo dataset com a informação corrigida
  by(issues, 1:nrow(issues), function(row){
    
    # Define o campo sprint como a sprint em que o ticket foi entregue
    if (row$sprint.1 != '') { row$sprint <- row$sprint.1; }
    if (row$sprint.2 != '') { row$sprint <- row$sprint.2; }
    if (row$sprint.3 != '') { row$sprint <- row$sprint.3; }
    
    # Transforma o campo sprint em numérico
    row$sprint <- str_replace(row$sprint, 'Sprint ', '')
    
    # Adiciona linha ao dataframe apenas se a linha
    # tiver algum peso no cálculo a ser realizado.
    if(row$sprint != "" && is.numeric(row$points) && !is.na(row$points) && row$points > 0) {
      entries <<- rbind(entries, data.frame(s = row$sprint, p = row$points))
    }
    
  })
  
  # Retorna os dados
  return(entries)
  
}