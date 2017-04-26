library(igraph)

configModel <- function (degrees = rep(n, floor(n / 2)), 
                         membership = rep(1, n), P = NULL) {
  
  # Checking P
  if (!is.null(P)) {
    if (!is.matrix(P))
      stop('P must be a matrix\n')
    K <- max(membership)
    if (nrow(P) != K)
      stop('P must have K rows\n')
    if (var(rowSums(P)) != 0)
      stop('row sums of P must all be equal\n')
  }
  
  # Checking degrees / membership
  n <- length(degrees)
  dT <- sum(degrees)
  if (dT %% 2 != 0) stop('sum of degrees must be even\n')
  if (length(membership) != length(degrees)) 
    stop('Number of degrees must equal number of memberships\n')
  
  # Initializing objects
  edge_list <- matrix(0, dT / 2, 2)
  set.seed(12345)
  edge_set <- unlist(lapply(1:n, function (u) rep(u, degrees[u])))
  edge_set <- sample(edge_set)
  remaining <- rep(TRUE, length(edge_set))
  mship_set <- membership[edge_set]
  
  if (is.matrix(P)) {
    Wmat <- t(apply(P, 1, function (r) r[mship_set]))
  } else {
    Wmat <- NULL
  }
  
  counter <- 1
  while (sum(remaining) > 0) {
    cat(counter, '/', dT / 2, '\n')
    indx <- min(which(remaining))
    remaining[indx] <- FALSE
    indx_mship <- mship_set[indx]
    now_rem <- which(remaining)
    if (length(now_rem) > 1) {
      indx2 <- sample(now_rem, 1, prob = Wmat[indx_mship, now_rem])
    } else {
      indx2 <- now_rem
    }
    remaining[indx2] <- FALSE
    edge_list[counter, ] <- edge_set[c(indx, indx2)]
    counter <- counter + 1
  }
  
  G <- graph.edgelist(edge_list, directed = FALSE)
  return(G)
}

if (FALSE) {
  P <- matrix(c(0.06, 0.06, 0.00,
                0.04, 0.12, 0.04,
                0.00, 0.04, 0.66), ncol = 3)
  P <- t(apply(P, 1, function (r) r / sum(r)))
  membership <- c(rep(1, 100), rep(2, 100), rep(3, 100))
  degrees <- rep(50, 300)
  G <- configModel(degrees, membership, P)
}