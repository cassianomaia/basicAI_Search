CITY_NAMES <- c(
    "A", "B", "C", "D",
    "F", "G", "L", "M",
    "O", "P", "R", "S",
    "T", "U", "Z"
)

euclideandistance.get <- function(){
    distances <- t(c(366,0,160,242,178,77,244,241,380,98,193,253,329,80,374))
    colnames(distances, do.NULL=FALSE)
    colnames(distances) = CITY_NAMES
    return(distances)
}

transitionmatrix.get <- function() {
    mat <- matrix(c(
        0,0,0,0,0,0,0,0,0,0,0,140,118,0,75
        ,0,0,0,0,211,90,0,0,0,101,0,0,0,85,0
        ,0,0,0,120,0,0,0,0,0,138,146,0,0,0,0
        ,0,0,120,0,0,0,0,75,0,0,0,0,0,0,0
        ,0,211,0,0,0,0,0,0,0,0,0,99,0,0,0
        ,0,90,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,70,0,0,0,0,111,0,0
        ,0,0,0,75,0,0,70,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,151,0,0,71
        ,0,101,138,0,0,0,0,0,0,0,97,0,0,0,0
        ,0,0,146,0,0,0,0,0,0,97,0,80,0,0,0
        ,140,0,0,0,99,0,0,0,151,0,80,0,0,0,0
        ,118,0,0,0,0,0,111,0,0,0,0,0,0,0,0
        ,0,0,85,0,0,0,0,0,0,0,0,0,0,0,0
        ,75,0,0,0,0,0,0,0,71,0,0,0,0,0,0
    ), nrow=15, ncol=15)
    rownames(mat, do.NULL = FALSE)
    colnames(mat, do.NULL = FALSE)
    rownames(mat) <- CITY_NAMES
    colnames(mat) <- CITY_NAMES
    return(mat)
}

transitionmatrix <- transitionmatrix.get()
ed <- euclideandistance.get

source("TrajetoDuasCidades.R")
source("buscaDesinformada.R")
source("buscaInformada.R")

inicial <- Trajeto(desc = "A", cidades = transitionmatrix, fun_heuristica = euclideandistance.get)
objetivo <- Trajeto(cidades = transitionmatrix, fun_heuristica = euclideandistance.get)
objetivo$desc <- "B"

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
#print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))
