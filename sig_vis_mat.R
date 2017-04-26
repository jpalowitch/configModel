sig_vis_mat <- function (G, membership) {
  
  degs <- degree(G)
  K <- max(membership)
  comms <- lapply(1:K, function (i) which(membership == i))
  sigmat <- matrix(0, ncol = K, nrow = K)
  Gadj <- as.matrix(get.adjacency(G))
  dT <- sum(degs)
  
  for (i in 1:K) {
    
    duB <- colSums(Gadj[comms[[i]], ])
    means <- degs * sum(degs[comms[[i]]]) / dT
    stats <- (duB - means) / length(comms[[i]])
    sigmat[i, ] <- tapply(stats, membership, mean)
    
  }
  
  return(sigmat)
  
}