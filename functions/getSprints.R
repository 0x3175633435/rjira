library('dplyr')
library('stringr')

getSprints <- function(path) {
  
  # Carrega os dados a serem analizados
  issues <- read.csv(path)
  
  # Separa apenas as colunas que serão utilizadas
  issues <- select(issues, points, Sprint, Sprint.1, Sprint.2, Sprint.3)
  
  # Cria um dataframe vazio para atualizar as informações
  entries = NULL
  
  # Cria um novo dataset com a informação corrigida
  by(issues, 1:nrow(issues), function(row){
    
    # Define o campo sprint como a sprint em que o ticket foi entregue
    if (row$Sprint.1 != '') { row$Sprint <- row$Sprint.1; }
    if (row$Sprint.2 != '') { row$Sprint <- row$Sprint.2; }
    if (row$Sprint.3 != '') { row$Sprint <- row$Sprint.3; }
    
    # Transforma o campo sprint em numérico
    row$Sprint <- str_replace(row$Sprint, 'Sprint ', '')
    
    # Adiciona linha ao dataframe apenas se a linha
    # tiver algum peso no cálculo a ser realizado.
    if(row$Sprint != "" && is.numeric(row$points) && !is.na(row$points) && row$points > 0) {
      entries <<- rbind(entries, data.frame(s = row$Sprint, p = row$points))
    }
    
  })
  
  # Apaga os issues
  issues <- NULL
  
  # Agrupa os dados no banco
  result <- aggregate(entries$p, by=list(s = entries$s), FUN=sum)
  
  # Retorna o resultado
  return(result)
  
}