source("Estado.R")

Trajeto <- function(desc=NULL, pai=NULL, cidades=NULL, fun_heuristica=NULL){
  
	e <- environment()
	
	assign("desc", desc, envir = e)
	assign("pai", pai, envir = e)
	assign("cidades", cidades, envir = e)
	assign("fun_h", fun_heuristica, envir = e)
	assign("g", 0, envir = e)
	assign("h", Inf, envir = e)
	assign("f", Inf, envir = e)
	
	class(e) <- c("Trajeto", "Estado")
	
	return(e)
}

## Sobrecarregando o operador "==" para comparação entre Trajetos
Ops.Trajeto = function(obj1,obj2){
 	if(.Generic == "=="){
  		return(all(obj1$desc == obj2$desc))
 	}
}

## Sobrecarga da função genérica print
print.Trajeto <- function(obj){
	cat("CIDADE: ", obj$desc, "\n")
	cat("G(N): ", obj$g, "\n")
	cat("H(N): ", obj$h, "\n")
	cat("F(N): ", obj$f, "\n")
}

## Criação do método genérico "heuristica"
heuristica <- function(atual, ...) {
    return(atual$fun_h()[1,atual$desc])
}

aplicarOperadorIr <- function(filhosDesc, vizinhos) {
	return(c(
		filhosDesc,
		colnames(vizinhos)[apply(vizinhos,1,function(vizinhos){
			# Descobrir quais são os nós vizinhos
			# (posições cujo valor é diferente de 0 na matriz de transição de estados)
			which(vizinhos != 0, arr.ind=T) 
		})]
	))
}

## Criação do método genérico "geraFilhos"
geraFilhos <- function(obj) {
    filhosDesc <- list()
	filhos <- list()

	cidades <- obj$cidades

	vizinhos <- t(cidades[obj$desc,])
	colnames(vizinhos, do.NULL=FALSE)
	colnames(vizinhos) <- colnames(cidades)

	# Aplicar o único operador: "ir" para cidade vizinha
	filhosDesc <- aplicarOperadorIr(filhosDesc, vizinhos)

	for(filhoDesc in filhosDesc){
		filho <- Trajeto(desc=filhoDesc, pai=obj, fun_h=obj$fun_heuristica, cidades=obj$cidades)
		filho$h <- heuristica(filho)
		filho$g <- obj$g + vizinhos[1,filhoDesc]
		filho$f <- filho$h + filho$g
		filhos <- c(filhos, list(filho))
	}
	return(filhos)
}
