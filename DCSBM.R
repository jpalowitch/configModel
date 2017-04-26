library(igraph)

DCSBM <- function (degrees, memberships, P) {
  
  # Checking degrees
  if (max(degrees) > length(degrees))
    stop('degrees cannot be longer than number of degrees\n')
  if (max(degrees)^2 / sum(degrees) > 1)
    stop('probabilities may be greater than 1\n')
  
  # Checking P
  if (!is.matrix(P))
    stop('P must be a matrix\n')
  K <- max(membership)
  if (nrow(P) != K)
    stop('P must have K rows\n')
  if (!isSymmetric(P))
    stop('P must be symmetric\n')
  
  n <- length(degrees)
  pair_mat <- combn(n, 2)
  memb_mat <- apply(pair_mat, 2, function (c) membership[c])
  P_mat <- apply(memb_mat, 2, function (c) P[c[1], c[2]])
  degs_mat <- apply(pair_mat, 2, function (c) degrees[c])
  probs <- P_mat * degs_mat[1, ] * degs_mat[2, ] / sum(degrees)
  edges <- rbinom(length(probs), 1, probs)
  edge_list <- t(pair_mat[ , edges == 1])
  
  G <- graph.edgelist(edge_list, directed = FALSE)
  return(G)
  
}


P <- matrix(c(0.06, 0.04, 0.00,
              0.04, 0.12, 0.04,
              0.00, 0.04, 0.66), ncol = 3)
membership <- c(rep(1, 100), rep(2, 100), rep(3, 100))
degrees <- rep(50, 300)
G <- DCSBM(degrees, membership, P)
