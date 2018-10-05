source("Qc8p.R")

Qc8p <- function(desc=NULL, pai=NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Qc8p", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre Qc8ps
Ops.Qc8p = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica print
print.Qc8p <- function(obj){
  cat(obj$desc, "\n")
  cat(obj$g, "\n")
  cat(obj$h, "\n")
}

## Criação do método genérico "heuristica"
# matrix(c(1, 2, 3, 8, null, 4, 7, 6, 5), nrow=3, ncol=3)
heuristica <- function(atual, ...) {
	mat <- matrix(atual$desc, nrow=3, ncol=3)
	d_manhattan <- 0
	for (i in nrow(mat)) {
    	for(j in ncol(mat)){
			if(mat[i][j] == 1)
				d_manhattan <- d_manhattan + i + j
			else if (mat[i][j] == 2 || mat[i][j] == 8) 
				d_manhattan <- d_manhattan + i + j - 1
			else if (mat[i][j] == 3 || mat[i][j] == 7)
				d_manhattan <- d_manhattan + i + j - 2 
			else if (mat[i][j] == 4 || mat[i][j] == 6)
				d_manhattan <- d_manhattan + i + j - 3
			else if (mat[i][j] == 5)
				d_manhattan <- d_manhattan + i + j - 4
    	}
  	}
	return(d_manhattan)
}

## Criação do método genérico "geraFilhos"
geraFilhos <- function(obj) {
  	UseMethod("geraFilhos")
}

geraFilhos.default <- function(obj) {
  print("Funcao Generica. Defina a geração de filhos para o seu problema!\n")
  return(NULL)
}