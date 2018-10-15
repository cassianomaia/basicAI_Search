source("Estado.R")

Qc8p <- function(desc=NULL, pai=NULL, passado=list()){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("passado", passado, envir = e)
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
  matrix_print <- t(matrix(obj$desc,nrow=3,ncol=3))
  print(matrix_print)
  cat("G(n):",obj$g, "\n")
  cat("H(n):",obj$h, "\n")
  cat("F(n):",obj$f, "\n")
}

## Criação do método genérico "heuristica"
# matrix(c(1, 2, 3, 8, nll, 4, 7, 6, 5), nrow=3, ncol=3)
heuristica <- function(atual, ...) {
	if(is.null(atual$desc))
    	return(Inf)

	mat <- t(matrix(atual$desc, nrow=3, ncol=3))
	d_manhattan <- 0
	for (i in 1:nrow(mat)) {
    	for(j in 1:ncol(mat)){
			if(mat[i,j] == 1)
				d_manhattan <- d_manhattan + abs((i - 1) + (j - 1))
			else if(mat[i,j] == 2)
				d_manhattan <- d_manhattan + abs((i - 1) + (j - 2))
			else if(mat[i,j] == 3)
				d_manhattan <- d_manhattan + abs((i - 1) + (j - 3))
			else if(mat[i,j] == 4)
				d_manhattan <- d_manhattan + abs((i - 2) + (j - 3))
			else if(mat[i,j] == 5)
				d_manhattan <- d_manhattan + abs((i - 3) + (j - 3))
			else if(mat[i,j] == 6)
				d_manhattan <- d_manhattan + abs((i - 3) + (j - 2))
			else if(mat[i,j] == 7)
				d_manhattan <- d_manhattan + abs((i - 3) + (j - 1))
			else if(mat[i,j] == 8)
				d_manhattan <- d_manhattan + abs((i - 2) + (j - 1))
    	}
  	}
	# cat("d_manhattan: ", d_manhattan, "\n")
	return(d_manhattan)
}

aplicaOperador <-  function(filhosDesc, obj_mat, operador, i, j) {
	aux_mat <- obj_mat
	i.novo <- i+operador[1]
	j.novo <- j+operador[2]
	aux <- aux_mat[i.novo,j.novo]
	aux_mat[i.novo,j.novo] <- aux_mat[i,j]
	aux_mat[i,j] <- aux
	filhosDesc <- c(filhosDesc, unlist(c(t(aux_mat))))
	return(filhosDesc)
}


## Criação do método genérico "geraFilhos"
## obj= matrix(c(1, 2, 3, 8, null, 4, 7, 6, 5)
geraFilhos <- function(obj) {
	desc <- obj$desc
	obj_mat <- t(matrix(desc, nrow=3, ncol=3))
	filhosDesc <- list()
	filhos <- list()

	count_row <- 0

	# posicao da celula vazia
	i_null <- which(obj_mat == 0, arr.ind=T)[1]
	j_null <- which(obj_mat == 0, arr.ind=T)[2]

	operadores <- list(
		c(1,0),		# Operador mover para a esquerada
		c(-1,0),	# Operador mover para a direita
		c(0,1),		# Operador mover para cima
		c(0,-1)		# Operador mover para baixo
	)
	
	if(i_null == 1 || i_null == 2){
		count_row <- count_row + 1
		filhosDesc <- aplicaOperador(filhosDesc, obj_mat, operadores[[1]], i_null, j_null)
	}
	
	if(i_null == 3 || i_null == 2){
		count_row <- count_row + 1
		filhosDesc <- aplicaOperador(filhosDesc, obj_mat, operadores[[2]], i_null, j_null)
	}
	
	if(j_null == 1 || j_null == 2){
		count_row <- count_row + 1
		filhosDesc <- aplicaOperador(filhosDesc, obj_mat, operadores[[3]], i_null, j_null)
	}
	
	if(j_null == 3 || j_null == 2){
		count_row <- count_row + 1
		filhosDesc <- aplicaOperador(filhosDesc, obj_mat, operadores[[4]], i_null, j_null)
	}
	
	filhosDesc <- matrix(c(filhosDesc), ncol=count_row, nrow=length(desc))
	print(obj_mat)
	
	for(j in 1:ncol(filhosDesc)){
		if(!list(unlist(filhosDesc[,j])) %in% obj$passado){
			filhoDesc <- unlist(filhosDesc[,j])
			
			filho <- Qc8p(desc=filhoDesc, pai=obj, passado=c(obj$passado, list(obj$desc)))
			filho$h <- heuristica(filho)
			filho$g <- 0
			filho$f <- filho$h
			filhos <- c(filhos, list(filho))
		}
	}

	return(filhos)
}
