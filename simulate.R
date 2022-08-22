simulate_model_generalized <- function(R0=2.5,
                                       q=1,
                                       rho=0,
                                       p=0.05,
                                       theta=1,
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
           p = p, gamma_u = gamma_u, gamma_v = gamma_v, q=q, theta=theta, mu=mu, base=base)
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
    
    dV <- - p/theta * lambda * V - q * (1-p/theta) * lambda * V + theta * rho * S - mu * V
    dIv <- p/theta * lambda * V - gamma_v * Iv - mu * Iv
    dRv <- gamma_v * Iv + q * (1-p/theta) * lambda * V + (1-theta) * rho * S - mu * Rv
    
    dCu <- lambda * S
    dCv <- p/theta * lambda * V
    
    list(c(dS, dIu, dRu, dV, dIv, dRv, dCu, dCv), inc_s = lambda * S, inc_v = p * lambda * V/theta)
  })
}

simulate_model_distribution <- function(R0=2.5,
                                        q=1,
                                        rho=0,
                                        p=0.05,
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
           p = p, gamma_u = gamma_u, gamma_v = gamma_v, q=q, theta=theta, mu=mu, base=base)
  yini <- c(S=S0, Iu=I0, Ru=0, V=V0, Iv=0, Rv=Rv0, Cu=0, Cv=0)
  
  out <- as.data.frame(ode(yini, tvec, model_generalized, par))
  
  out
}

model_distribution <- function(t, y, par) {
  with(as.list(c(par)), {
    S <- y[1]
    Iu <- y[2]
    Ru <- y[3]
    
    
    
    lambda <- beta_u * Iu + beta_v * Iv
    
    dS <- mu - lambda * S - rho * S - mu * S
    dIu <- lambda * S - gamma_u * Iu - mu * Iu
    dRu <- gamma_u * Iu - mu * Ru
    
    dV <- - lambda * V + rho * S - mu * V
    dIv <- p * lambda * V - gamma_v * Iv - mu * Iv
    dRv <- gamma_v * Iv + (1-p) * lambda * V - mu * Rv
    
    list(c(dS, dIu, dRu, dV, dIv, dRv, dCu, dCv))
  })
}
