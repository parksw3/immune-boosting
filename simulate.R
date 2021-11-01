simulate_model_generalized <- function(R0=2.5,
                                       q=1,
                                       rho=0.02/7,
                                       p=0.05,
                                       theta=1,
                                       gamma_u=1/5,
                                       gamma_v=1/5,
                                       S0=1-1e-6,
                                       I0=1e-6,
                                       mu=0,
                                       base=0.01,
                                       tvec=seq(0, 150, by=1)) {
  beta_u <- R0 * gamma_u
  beta_v <- beta_u
  
  par <- c(beta_u=beta_u, beta_v=beta_v, rho=rho, 
           p = p, gamma_u = gamma_u, gamma_v = gamma_v, q=q, theta=theta, mu=mu, base=base)
  yini <- c(S=S0, Iu=I0, Ru=0, V=0, Iv=0, Rv=0)
  
  out <- as.data.frame(ode(yini, tvec, model_generalized, par))
  
  out
}

model_generalized <- function(t, y, par) {
  with(as.list(c(y, par)), {
    
    lambda <- beta_u * Iu + beta_v * Iv
    
    dS <- mu - lambda * S - rho * S - mu * S
    dIu <- lambda * S - gamma_u * Iu - mu * Iu
    dRu <- gamma_u * Iu - mu * Ru
    
    dV <- - p/theta * lambda * V - q * (1-p/theta) * lambda * V + theta * rho * S - mu * V
    dIv <- p/theta * lambda * V - gamma_v * Iv - mu * Iv
    dRv <- gamma_v * Iv + q * (1-p/theta) * lambda * V + (1-theta) * rho * S - mu * Rv
    
    list(c(dS, dIu, dRu, dV, dIv, dRv),
         ev_u=potentialfun(1) * Iu,
         ev_v=potentialfun(p/theta) * Iv,
         inc_s = lambda * S, inc_v = p * lambda * V/theta)
  })
}

potentialfun <- function(x, base=0.01) {
  1-(1-base)/0.25* (x-0.5)^2
}

## c(2.91, 3.51, 3.31, 3.54, 3.51, 3.26, 3.57, 3.39, 3.2, 3.13, 3.21, 3.72, 3.3, 4.06, 3.36, 3.45, 3.54)
## mean: 3.41
simulate_model_viral <- function(beta=10^-7.23,
                                 delta=3.14,
                                 k=0.08,
                                 m=3.41,
                                 phi=100,
                                 r=10,
                                 pii=10^2.6,
                                 gamma=15,
                                 omega=10^-4.56,
                                 q=2.4e-5,
                                 delta_E=1,
                                 tvec=seq(0, 100, by=0.01),
                                 S0=1e7,
                                 I0=1,
                                 M10=1,
                                 M20=0,
                                 E0=0) {
  V0 <- pii/gamma * I0
  yini <- c(S=S0, I=I0, V=pii/gamma, M1=M10, M2=M20, E=E0)
  parms <- c(beta=beta, delta=delta, k=k, m=m, phi=phi, r=r, pii=pii, gamma=gamma, omega=omega, q=q, delta_E=delta_E)
  
  out <- as.data.frame(deSolve::ode(yini, tvec, model_viral, parms))
  out$EIP <- out$V*cumsum(out$acquired)*0.01
  
  out
}

model_viral <- function(t, y, par) {
  with(as.list(c(y, par)), {
    
    dS <- - beta * V * S
    dI <- beta * V * S - delta * I^k * I - m * E^r/(E^r + phi^r) * I
    dV <- pii * I - gamma * V
    dM1 <- omega * I * M1 - q * M1
    dM2 <- q * M1 - q * M2
    dE <- q * M2 - delta_E * E
    
    list(c(dS, dI, dV, dM1, dM2, dE),
         innate=delta * I^k,
         acquired=m * E^r/(E^r + phi^r))
  })
}
