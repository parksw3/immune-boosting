simulate_model_generalized <- function(R0=2.5,
                                       VE_L=0,
                                       VE_P=0,
                                       q=1,
                                       rho=0,
                                       gamma_u=1/5,
                                       gamma_v=1/5,
                                       S0=1-1e-6,
                                       V0=0,
                                       Rv0=0,
                                       I0=1e-6,
                                       mu=0,
                                       base=0.01,
                                       tvec=seq(0, 220, by=1)) {
  beta_u <- R0 * gamma_u
  beta_v <- beta_u
  
  par <- c(beta_u=beta_u, beta_v=beta_v, rho=rho, 
           VE_L=VE_L, VE_P=VE_P, gamma_u = gamma_u, gamma_v = gamma_v, q=q, mu=mu, base=base)
  yini <- c(S=S0, Iu=I0, Ru=0, V=V0, Iv=0, Rv=Rv0, Cu=0, Cv=0)
  
  out <- as.data.frame(ode(yini, tvec, model_generalized, par))
  
  out
}

model_generalized <- function(t, y, par) {
  with(as.list(c(y, par)), {
    
    lambda <- beta_u * Iu + beta_v * Iv
    
    dS <- mu - lambda * S - rho * S - mu * S
    dIu <- lambda * S - gamma_u * Iu - mu * Iu
    dRu <- gamma_u * Iu - mu * Ru
    
    dV <- - (1 - (1-q) * VE_L) * lambda * V + (1-VE_P) * rho * S - mu * V
    dIv <- (1-VE_L) * lambda * V - gamma_v * Iv - mu * Iv
    dRv <- gamma_v * Iv + q * VE_L * lambda * V + VE_P * rho * S - mu * Rv
    
    dCu <- lambda * S
    dCv <- (1-VE_L) * lambda * V
    
    list(c(dS, dIu, dRu, dV, dIv, dRv, dCu, dCv), inc_s = lambda * S, inc_v = (1-VE_L) * lambda * V)
  })
}
