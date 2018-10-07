source("Estado.R")

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

	#print(obj_mat)
	if(i_null == 1 || i_null == 2){
		aux_mat <- obj_mat

		aux <- aux_mat[i_null+1,j_null]
		aux_mat[i_null+1,j_null] <- aux_mat[i_null,j_null]
		aux_mat[i_null,j_null] <- aux
		filhosDesc <- c(filhosDesc, unlist(c(t(aux_mat))))
		count_row <- count_row + 1
	}
	
	if(i_null == 3 || i_null == 2){
		aux_mat <- obj_mat

		aux <- aux_mat[i_null-1,j_null]
		aux_mat[i_null-1,j_null] <- aux_mat[i_null,j_null]
		aux_mat[i_null,j_null] <- aux
		filhosDesc <- c(filhosDesc, unlist(c(t(aux_mat))))
		count_row <- count_row + 1
	}
	
	if(j_null == 1 || j_null == 2){
		aux_mat <- obj_mat
		
		aux <- aux_mat[i_null,1+j_null]
		aux_mat[i_null,1+j_null] <- aux_mat[i_null,j_null]
		aux_mat[i_null,j_null] <- aux
		filhosDesc <- c(filhosDesc, unlist(c(t(aux_mat))))
		count_row <- count_row + 1
	}
	
	if(j_null == 3 || j_null == 2){
		aux_mat <- obj_mat
		
		aux <- aux_mat[i_null,j_null-1]
		aux_mat[i_null,j_null-1] <- aux_mat[i_null,j_null]
		aux_mat[i_null,j_null] <- aux
		filhosDesc <- c(filhosDesc, unlist(c(t(aux_mat))))
		count_row <- count_row + 1
	}

	filhosDesc = matrix(c(filhosDesc), ncol=count_row, nrow=length(desc))
	#print("Filhos Desc:")
	#print(filhosDesc)
	for(j in 1:ncol(filhosDesc)){
		filhoDesc <- unlist(filhosDesc[,j])
		# print(filhoDesc)
		filho <- Qc8p(desc=filhoDesc, pai=obj)
		filho$h <- heuristica(filho)
		filho$g <- 0
		filho$f <- filho$h
		filhos <- c(filhos, list(filho))
	}

	return(filhos)
}
