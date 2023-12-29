# varn <- function
lvec = erdat$Effort
  nvec = erdat$transect_n_observations
  type = unique(erdat$er_est)

  ntot <- sum(nvec)
  L <- sum(lvec)
  k <- length(lvec)

  if (!(type %in% c("R2", "R3", "R4", "S1",
                    "S2", "O1", "O2", "O3", "P2",
                    "P3")))
    stop(paste("Encounter rate variance type '", type,
               "' is not recognized.", sep = ""))
  if (type == "R2") {
    var.R2 <- (k * sum(lvec^2 * (nvec/lvec - ntot/L)^2))/(L^2 *
                                                            (k - 1))
    return(var.R2)
  }
  if (type == "P2") {
    var.P2 <- (1/(k * (k - 1))) * sum((nvec/lvec - 1/k *
                                         sum(nvec/lvec))^2)
    return(var.P2)
  }
  if (type == "R3" | type == "P3") {
    var.R3 <- 1/(L * (k - 1)) * sum(lvec * (nvec/lvec - ntot/L)^2)
    return(var.R3)
  }
  if (type == "R4") {
    if (all(lvec == mean(lvec))) {
      phi <- (2 - 2/k)/(1 - 2/k)
    }
    else {
      S <- sum(lvec^2)
      C <- sum(lvec^3)
      logvec <- log(lvec)
      D1 <- sum(lvec * logvec)
      D2 <- sum(lvec^2 * logvec)
      D3 <- sum(lvec^3 * logvec)
      eps.top <- 2 * (S^2 - L * C)
      eps.bottom <- L * S * D1 - 2 * S * D2 + 2 * L * D3 -
        L^2 * D2
      eps <- eps.top/eps.bottom
      phi <- 2 + eps
    }
    alpha <- 1/sum(lvec^phi * (L/lvec - 1))
    var.R4 <- alpha * sum(lvec^phi * (nvec/lvec - ntot/L)^2)
    return(var.R4)
  }

  H <- floor(k/2)
  k.h <- rep(2, H)
  if (k%%2 > 0) {
    k.h[H] <- 3
  }
  end.strat <- cumsum(k.h)
  begin.strat <- cumsum(k.h) - k.h + 1
  if (type == "S1" | type == "S2") {
    sum.S1 <- 0
    sum.S2 <- 0
    for (h in 1:H) {
      nvec.strat <- nvec[begin.strat[h]:end.strat[h]]
      lvec.strat <- lvec[begin.strat[h]:end.strat[h]]
      nbar.strat <- mean(nvec.strat)
      lbar.strat <- mean(lvec.strat)
      inner.strat.S1 <- sum((nvec.strat - nbar.strat -
                               (ntot/L) * (lvec.strat - lbar.strat))^2)
      sum.S1 <- sum.S1 + k.h[h]/(k.h[h] - 1) * inner.strat.S1
      L.strat <- sum(lvec.strat)
      var.strat.S2 <- k.h[h]/(L.strat^2 * (k.h[h] - 1)) *
        sum(lvec.strat^2 * (nvec.strat/lvec.strat - nbar.strat/lbar.strat)^2)
      sum.S2 <- sum.S2 + L.strat^2 * var.strat.S2
    }
    if (type == "S1") {
      var.S1 <- sum.S1/L^2
      return(var.S1)
    }
    else {
      var.S2 <- sum.S2/L^2
      return(var.S2)
    }
  }
  lvec.1 <- lvec[-k]
  lvec.2 <- lvec[-1]
  nvec.1 <- nvec[-k]
  nvec.2 <- nvec[-1]
  ervec.1 <- nvec.1/lvec.1
  ervec.2 <- nvec.2/lvec.2
  if (type == "O1") {
    overlap.varterm <- (nvec.1 - nvec.2 - ntot/L * (lvec.1 -
                                                      lvec.2))^2
    var.O1 <- k/(2 * L^2 * (k - 1)) * sum(overlap.varterm)
    return(var.O1)
  }
  if (type == "O2") {
    V.overlap.R2 <- ((lvec.1 * lvec.2)/(lvec.1 + lvec.2))^2 *
      (ervec.1 - ervec.2)^2
    var.O2 <- (2 * k)/(L^2 * (k - 1)) * sum(V.overlap.R2)
    return(var.O2)
  }
  if (type == "O3") {
    V.overlap.R3 <- ((lvec.1 * lvec.2)/(lvec.1 + lvec.2)) *
      (ervec.1 - ervec.2)^2
    var.O3 <- 1/(L * (k - 1)) * sum(V.overlap.R3)
    return(var.O3)
  }
}



#########################
my_x <- 1:5
my_x
mean(my_x)
match.call(mean, call("mean", my_x)) # Apply match.call to mean function
match.call(mean, my_x) # Apply match.call to mean function
