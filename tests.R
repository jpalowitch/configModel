source("DCSBM.R")
source("configModel.R")
source("sig_vis_mat.R")

# Testing the Bickel & Chen example

# If you want to draw degrees from a power law
degrees <- sample(10:100, 300, prob = 1 / 10:100, replace = TRUE)

# SBM
P <- matrix(c(0.06, 0.04, 0.00,
              0.04, 0.12, 0.04,
              0.00, 0.04, 0.66), ncol = 3)
membership <- c(rep(1, 100), rep(2, 100), rep(3, 100))
degrees <- rep(50, 300)
G1 <- DCSBM(degrees, membership, P)


# config
G2 <- configModel(degrees, membership, P)


sig_vis_mat(G1, membership)
sig_vis_mat(G2, membership)


# Testing example with null comm

P <- matrix(c(1.5, 0.5, 1.0,
              0.5, 1.5, 1.0,
              1.0, 1.0, 1.0), ncol = 3)
G2 <- configModel(degrees, membership, P)
sig_vis_mat(G2, membership)

