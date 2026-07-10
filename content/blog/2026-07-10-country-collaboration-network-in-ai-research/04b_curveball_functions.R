library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

get_b_ac <- function(table){
  
  articles <- unique(table$eid) # 504
  countries <- unique(table$iso3c) # 29
  
  ac_matrix <- matrix(0, length(articles), length(countries))
  
  rownames(ac_matrix) <- articles
  colnames(ac_matrix) <- countries
  
  for(i in 1:nrow(iclr_iso3c)){
    row <- which(iclr_iso3c$eid[i] == articles)
    column <- which(iclr_iso3c$iso3c[i] == countries)
    ac_matrix[row, column] <- 1
  }
  
  return(ac_matrix)
  
}

get_country_graph <- function(b_ac){
  
  country_matrix <- t(b_ac) %*% b_ac
  diag(country_matrix) <- 0 
  
  country_graph <- graph_from_adjacency_matrix(
    country_matrix,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  
  return(list(graph = country_graph, matrix = country_matrix))
}

null_country_graph <- function(b_ac){
  
  # crating a null b_ac matrix
  null_b_ac <- curve_ball(t(b_ac)) |> t()
  rownames(null_b_ac) <- rownames(b_ac) 
  colnames(null_b_ac) <- colnames(b_ac)
  
  # creating the null country matrix
  ncm <- get_country_graph(null_b_ac)
  
  # returning
  return(list(graph = ncm$graph, matrix = ncm$matrix))
  
}

curve_ball<-function(m){
  RC=dim(m)
  R=RC[1]
  C=RC[2]
  hp=list()
  for (row in 1:dim(m)[1]) {hp[[row]]=(which(m[row,]==1))}
  l_hp=length(hp)
  for (rep in 1:(5*l_hp)){
    AB=sample(1:l_hp,2)
    a=hp[[AB[1]]]
    b=hp[[AB[2]]]
    ab=intersect(a,b)
    l_ab=length(ab)
    l_a=length(a)
    l_b=length(b)
    if ((l_ab %in% c(l_a,l_b))==F){
      tot=setdiff(c(a,b),ab)
      l_tot=length(tot)
      tot=sample(tot, l_tot, replace = FALSE, prob = NULL)
      L=l_a-l_ab
      hp[[AB[1]]] = c(ab,tot[1:L])
      hp[[AB[2]]] = c(ab,tot[(L+1):l_tot])}
    
  }
  rm=matrix(0,R,C)
  for (row in 1:R){rm[row,hp[[row]]]=1}
  rm
}

