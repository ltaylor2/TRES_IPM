# ------------------
# Life table response experiments (LTRE)
# overall [#1] and by yearly-interval [2]
# Following Koons et al. 2016, 2017
#
# Uses full IPM posterior from output of Run_TRES.R
#
# OUTPUT
# cont_summary -- 
#                 Key: Parameter code
#                 Mean: % contribution to overall variance in lambda
#                 Low: 2.5% BCI posterior sample
#                 High: 97.5% BCI posterior sample
#
# yearly_cont_summary --
#                 Key: Parameter code
#                 Year: Starting year (of 2-yr interval)
#                 Mean: contribution to lambda[t+1]-lambda[t]
#                 Low: 2.5% BCI posterior sample
#                 High: 97.5% BCI posterior sample
#
# LTREcors_max  -- vital rate correl matrix (Maximum density r)
# LTREcors_low  -- vital rate correl matrix (2.5% posterior quantile)
# LTREcors_high -- vital rate correl matrix (97.5% posterior quantile)
# LTREcors_P    -- vital rate correl matrix (P(r>0)) 

# LT 2/20/18
# ------------------

library("matrixStats")

# LTRE for contributions to overall variance in lambda-------------------------
# #1

# pull IPM posteriors
ipm_sim <- ipm$sims.list    
numIters <- nrow(ipm_sim[[1]])

m.cs <- ipm_sim[["cs"]][,1:(nyears-1)]
m.hs <- ipm_sim[["hs"]][,1:(nyears-1)]
m.fs <- ipm_sim[["fs"]][,1:(nyears-1)]
m.phi.J.F <- ipm_sim[["phi.J.F"]][,1:(nyears-1)]
m.phi.J.M <- ipm_sim[["phi.J.M"]][,1:(nyears-1)]
m.phi.A.F <- ipm_sim[["phi.A.F"]][,1:(nyears-1)]
m.phi.A.M <- ipm_sim[["phi.A.M"]][,1:(nyears-1)]
m.omega.F <- ipm_sim[["omega.F"]][,1:(nyears-1)]
m.omega.M <- ipm_sim[["omega.M"]][,1:(nyears-1)]
m.prop.F <- ipm_sim[["b.F"]][,1:(nyears-1)] / ipm_sim[["Ntot"]][,1:(nyears-1)]
m.prop.M <- ipm_sim[["b.M"]][,1:(nyears-1)] / ipm_sim[["Ntot"]][,1:(nyears-1)]

# Calculate the transient sensitivities (first derivatives from matrix model)  
sens.cs <- matrix(0, nrow=numIters, ncol=1)
sens.hs <- matrix(0, nrow=numIters, ncol=1)
sens.fs <- matrix(0, nrow=numIters, ncol=1)
sens.phi.J.F <- matrix(0, nrow=numIters, ncol=1)
sens.phi.J.M <- matrix(0, nrow=numIters, ncol=1)
sens.phi.A.F <- matrix(0, nrow=numIters, ncol=1)
sens.phi.A.M <- matrix(0, nrow=numIters, ncol=1)
sens.omega.F <- matrix(0, nrow=numIters, ncol=1)
sens.omega.M <- matrix(0, nrow=numIters, ncol=1)
sens.prop.F <- matrix(0, nrow=numIters, ncol=1)
sens.prop.M <- matrix(0, nrow=numIters, ncol=1)

# mean values for sensitivities
mu.cs <- rowMeans(m.cs)
mu.hs <- rowMeans(m.hs)
mu.fs <- rowMeans(m.fs)
mu.phi.J.F <- rowMeans(m.phi.J.F)
mu.phi.J.M <- rowMeans(m.phi.J.M)
mu.phi.A.F <- rowMeans(m.phi.A.F)
mu.phi.A.M <- rowMeans(m.phi.A.M)
mu.omega.F <- rowMeans(m.omega.F)
mu.omega.M <- rowMeans(m.omega.M)
mu.prop.F <- rowMeans(m.prop.F)
mu.prop.M <- rowMeans(m.prop.M)

for (i in 1:numIters) {
  sens.cs[i] <- (0.5 * mu.hs[i] * mu.fs[i] * mu.phi.J.F[i] *
                 mu.prop.F[i]) +
                (0.5 * mu.hs[i] * mu.fs[i] * mu.phi.J.M[i] *
                 mu.prop.M[i])
  
  sens.hs[i] <- (0.5 * mu.cs[i] * mu.fs[i] * mu.phi.J.F[i] *
                 mu.prop.F[i]) +
                (0.5 * mu.cs[i] * mu.fs[i] * mu.phi.J.M[i] *
                 mu.prop.M[i])
  
  sens.fs[i] <- (0.5 * mu.cs[i] * mu.hs[i] * mu.phi.J.F[i] *
                 mu.prop.F[i]) +
                (0.5 * mu.cs[i] * mu.hs[i] * mu.phi.J.M[i] *
                 mu.prop.M[i])
  
  sens.phi.J.F[i] <- (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] *
                      mu.prop.F[i])

  sens.phi.J.M[i] <- (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] *
                      mu.prop.M[i])
  
  sens.phi.A.F[i] <- mu.prop.F[i]
  sens.phi.A.M[i] <- mu.prop.M[i]
  
  sens.omega.F[i] <- mu.prop.F[i]
  sens.omega.M[i] <- mu.prop.M[i]
    
  # First derivatives calculated before simplifying prop.F+prop.M = 1
  # See Koons 2017 errata
  sens.prop.F[i] <- (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.F[i]) + 
                    mu.phi.A.F[i] + 
                    mu.omega.F[i] -
                    ((0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.F[i] * mu.prop.F[i]) + 
                     (mu.phi.A.F[i] * mu.prop.F[i]) + 
                     (mu.omega.F[i] * mu.prop.F[i]) + 
                     (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.M[i] * 
                      mu.prop.M[i]) + 
                     (mu.phi.A.M[i] * mu.prop.M[i]) + 
                     (mu.omega.M[i] * mu.prop.M[i]))
  
  sens.prop.M[i] <- (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.M[i]) + 
                    mu.phi.A.M[i] + 
                    mu.omega.M[i] -
                    ((0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.F[i] * mu.prop.F[i]) + 
                     (mu.phi.A.F[i] * mu.prop.F[i]) + 
                     (mu.omega.F[i] * mu.prop.F[i]) + 
                     (0.5 * mu.cs[i] * mu.hs[i] * mu.fs[i] * mu.phi.J.M[i] * 
                      mu.prop.M[i]) + 
                     (mu.phi.A.M[i] * mu.prop.M[i]) + 
                     (mu.omega.M[i] * mu.prop.M[i]))
}
 
# calculate contributions
cont.cs <- matrix(0, nrow=numIters, ncol=1)
cont.hs <- matrix(0, nrow=numIters, ncol=1)
cont.fs <- matrix(0, nrow=numIters, ncol=1)
cont.phi.J.F <- matrix(0, nrow=numIters, ncol=1)
cont.phi.J.M <- matrix(0, nrow=numIters, ncol=1)
cont.phi.A.F <- matrix(0, nrow=numIters, ncol=1)
cont.phi.A.M <- matrix(0, nrow=numIters, ncol=1)
cont.omega.F <- matrix(0, nrow=numIters, ncol=1)
cont.omega.M <- matrix(0, nrow=numIters, ncol=1)
cont.prop.F <- matrix(0, nrow=numIters, ncol=1)
cont.prop.M <- matrix(0, nrow=numIters, ncol=1)

varcors <- list()
for (i in 1:numIters) {
  stoch <- cbind(m.cs[i,], 
                 m.hs[i,], 
                 m.fs[i,],
                 m.phi.J.F[i,], 
                 m.phi.J.M[i,],
                 m.phi.A.F[i,], 
                 m.phi.A.M[i,],
                 m.omega.F[i,], 
                 m.omega.M[i,],
                 m.prop.F[i,],
                 m.prop.M[i,])
  
  varcov <- var(stoch)
  
  # store correlations from covariance matrices
  varcors[[i]] <- cov2cor(varcov)
  
  sensvec <- c(sens.cs[i], 
               sens.hs[i], 
               sens.fs[i],
               sens.phi.J.F[i], 
               sens.phi.J.M[i],
               sens.phi.A.F[i], 
               sens.phi.A.M[i],
               sens.omega.F[i], 
               sens.omega.M[i],
               sens.prop.F[i], 
               sens.prop.M[i])
  
  # see Koons 2017 eq. 1 (literature cited in text)
  contmatrix <- matrix(0, 11, 11)
  for (r in 1:nrow(contmatrix)) {
    for (c in 1:ncol(contmatrix)) {
      contmatrix[r,c] <- varcov[r,c]*sensvec[r]*sensvec[c]
    }
  }
  contributions <- rowSums(contmatrix)
  cont.cs[i] <- contributions[1]
  cont.hs[i] <- contributions[2]
  cont.fs[i] <- contributions[3]
  cont.phi.J.F[i] <- contributions[4]
  cont.phi.J.M[i] <- contributions[5]
  cont.phi.A.F[i] <- contributions[6]
  cont.phi.A.M[i] <- contributions[7]
  cont.omega.F[i] <- contributions[8]
  cont.omega.M[i] <- contributions[9]
  cont.prop.F[i] <- contributions[10]
  cont.prop.M[i] <- contributions[11]
}

m.lam_real <- ipm_sim[["lambda"]][,1:(nyears-1)]
tempvar_real <- rowVars(m.lam_real)

# summarise results
cont_summary <- data.frame(cs=cont.cs, 
                           hs=cont.hs, 
                           fs=cont.fs, 
                           phi.J.F=cont.phi.J.F, 
                           phi.J.M=cont.phi.J.M,
                           phi.A.F=cont.phi.A.F, 
                           phi.A.M=cont.phi.A.M,
                           omega.F=cont.omega.F, 
                           omega.M=cont.omega.M,
                           prop.F=cont.prop.F, 
                           prop.M=cont.prop.M) %>%
                          gather() %>%
                          group_by(key) %>%
                          summarise(mean=mean(value)), 
                                    low=quantile(value, 0.025),
                                    high=quantile(value, 0.975))


# LTRE for contributions to (t to t+1) difference in lambda--------------------
# #2

# pull IPM posteriors
ipm_sim <- ipm$sims.list 
numIters <- nrow(ipm_sim[[1]])

m.cs <- ipm_sim[["cs"]][,1:(nyears-1)]
m.hs <- ipm_sim[["hs"]][,1:(nyears-1)]
m.fs <- ipm_sim[["fs"]][,1:(nyears-1)]
m.phi.J.F <- ipm_sim[["phi.J.F"]][,1:(nyears-1)]
m.phi.J.M <- ipm_sim[["phi.J.M"]][,1:(nyears-1)]
m.phi.A.F <- ipm_sim[["phi.A.F"]][,1:(nyears-1)]
m.phi.A.M <- ipm_sim[["phi.A.M"]][,1:(nyears-1)]
m.omega.F <- ipm_sim[["omega.F"]][,1:(nyears-1)]
m.omega.M <- ipm_sim[["omega.M"]][,1:(nyears-1)]
m.prop.F <- ipm_sim[["b.F"]][,1:(nyears-1)] / ipm_sim[["Ntot"]][,1:(nyears-1)]
m.prop.M <- ipm_sim[["b.M"]][,1:(nyears-1)] / ipm_sim[["Ntot"]][,1:(nyears-1)]

# assembling delta_lam contributions for every 2 year step across 24 years
# ldply (from plyr package) sends a List and gets back a Dataframe
yearly_cont_summary <- ldply(1:(nyears-2),
function(t1) {
  t2 <- t1 + 1

# Calculate sensitivities again
sens.cs <- matrix(0, nrow=numIters, ncol=1)
sens.hs <- matrix(0, nrow=numIters, ncol=1)
sens.fs <- matrix(0, nrow=numIters, ncol=1)
sens.phi.J.F <- matrix(0, nrow=numIters, ncol=1)
sens.phi.J.M <- matrix(0, nrow=numIters, ncol=1)
sens.phi.A.F <- matrix(0, nrow=numIters, ncol=1)
sens.phi.A.M <- matrix(0, nrow=numIters, ncol=1)
sens.omega.F <- matrix(0, nrow=numIters, ncol=1)
sens.omega.M <- matrix(0, nrow=numIters, ncol=1)
sens.prop.F <- matrix(0, nrow=numIters, ncol=1)
sens.prop.M <- matrix(0, nrow=numIters, ncol=1)

for (i in 1:numIters) {
  
  # mean values are calculated across the time period only
  mu.cs <- (m.cs[i,t1] + m.cs[i,t2]) / 2
  mu.hs <- (m.hs[i,t1] + m.hs[i,t2]) / 2
  mu.fs <- (m.fs[i,t1] + m.fs[i,t2]) / 2

  mu.phi.J.F <- (m.phi.J.F[i,t1] + m.phi.J.F[i,t2]) / 2
  mu.phi.J.M <- (m.phi.J.M[i,t1] + m.phi.J.M[i,t2]) / 2
  
  mu.phi.A.F <- (m.phi.A.F[i,t1] + m.phi.A.F[i,t2]) / 2
  mu.phi.A.M <- (m.phi.A.M[i,t1] + m.phi.A.M[i,t2]) / 2
  
  mu.omega.F <- (m.omega.F[i,t1] + m.omega.F[i,t2]) / 2
  mu.omega.M <- (m.omega.M[i,t1] + m.omega.M[i,t2]) / 2
    
  mu.prop.F <- (m.prop.F[i,t1] + m.prop.F[i,t2]) / 2
  mu.prop.M <- (m.prop.M[i,t1] + m.prop.M[i,t2]) / 2
  
  # sensitivities use the period mean values (not overall means)
  sens.cs[i] <- (0.5*mu.hs*mu.fs*mu.phi.J.F*mu.prop.F) +
                (0.5*mu.hs*mu.fs*mu.phi.J.M*mu.prop.M)
  
  sens.hs[i] <- (0.5*mu.cs*mu.fs*mu.phi.J.F*mu.prop.F) +
                (0.5*mu.cs*mu.fs*mu.phi.J.M*mu.prop.M)
  
  sens.fs[i] <- (0.5*mu.cs*mu.hs*mu.phi.J.F*mu.prop.F) +
                (0.5*mu.cs*mu.hs*mu.phi.J.M*mu.prop.M)
  
  sens.phi.J.F[i] <- (0.5*mu.cs*mu.hs*mu.fs*mu.prop.F)
  sens.phi.J.M[i] <- (0.5*mu.cs*mu.hs*mu.fs*mu.prop.M)
  
  sens.phi.A.F[i] <- mu.prop.F
  sens.phi.A.M[i] <- mu.prop.M
  
  sens.omega.F[i] <- mu.prop.F
  sens.omega.M[i] <- mu.prop.M
    
  sens.prop.F[i] <- (0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.F)+
                    mu.phi.A.F + 
                    mu.omega.F -
                    ((0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.F * mu.prop.F) + 
                      (mu.phi.A.F * mu.prop.F) + 
                      (mu.omega.F * mu.prop.F) + 
                     (0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.M * mu.prop.M) + 
                      (mu.phi.A.M * mu.prop.M) + 
                      (mu.omega.M * mu.prop.M))
  
  sens.prop.M[i] <- (0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.M)+
                    (mu.phi.A.M) + 
                    mu.omega.M -
                    ((0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.F * mu.prop.F) + 
                      (mu.phi.A.F * mu.prop.F) + 
                      (mu.omega.F * mu.prop.F) + 
                     (0.5 * mu.cs * mu.hs * mu.fs * mu.phi.J.M * mu.prop.M) + 
                      (mu.phi.A.M * mu.prop.M) + 
                      (mu.omega.M * mu.prop.M))
}

cont.cs <- matrix(0, nrow=numIters, ncol=1)
cont.hs <- matrix(0, nrow=numIters, ncol=1)
cont.fs <- matrix(0, nrow=numIters, ncol=1)
cont.phi.J.F <- matrix(0, nrow=numIters, ncol=1)
cont.phi.J.M <- matrix(0, nrow=numIters, ncol=1)
cont.phi.A.F <- matrix(0, nrow=numIters, ncol=1)
cont.phi.A.M <- matrix(0, nrow=numIters, ncol=1)
cont.omega.F <- matrix(0, nrow=numIters, ncol=1)
cont.omega.M <- matrix(0, nrow=numIters, ncol=1)
cont.prop.F <- matrix(0, nrow=numIters, ncol=1)
cont.prop.M <- matrix(0, nrow=numIters, ncol=1)

# contributions are diff in place of var/cov
# see Koons 2017 eq. 2 (literature cited in text)
for (i in 1:numIters) {
  diffvec <- c(m.cs[i,t2]-m.cs[i,t1],
               m.hs[i,t2]-m.hs[i,t1], 
               m.fs[i,t2]-m.fs[i,t1],
               m.phi.J.F[i,t2]-m.phi.J.F[i,t1], 
               m.phi.J.M[i,t2]-m.phi.J.M[i,t1],
               m.phi.A.F[i,t2]-m.phi.A.F[i,t1], 
               m.phi.A.M[i,t2]-m.phi.A.M[i,t1],
               m.omega.F[i,t2]-m.omega.F[i,t1], 
               m.omega.M[i,t2]-m.omega.M[i,t1],
               m.prop.F[i,t2]-m.prop.F[i,t1],
               m.prop.M[i,t2]-m.prop.M[i,t1])
          
  sensvec <- c(sens.cs[i], 
               sens.hs[i], 
               sens.fs[i],
               sens.phi.J.F[i], 
               sens.phi.J.M[i],
               sens.phi.A.F[i], 
               sens.phi.A.M[i],
               sens.omega.F[i], 
               sens.omega.M[i],
               sens.prop.F[i], 
               sens.prop.M[i])
  
  contributions <- diffvec * sensvec

  cont.cs[i] <- contributions[1]
  cont.hs[i] <- contributions[2]
  cont.fs[i] <- contributions[3]
  cont.phi.J.F[i] <- contributions[4]
  cont.phi.J.M[i] <- contributions[5]
  cont.phi.A.F[i] <- contributions[6]
  cont.phi.A.M[i] <- contributions[7]
  cont.omega.F[i] <- contributions[8]
  cont.omega.M[i] <- contributions[9]
  cont.prop.F[i] <- contributions[10]
  cont.prop.M[i] <- contributions[11]
}


df <- data.frame(cs=cont.cs, 
                 hs=cont.hs, 
                 fs=cont.fs, 
                 phi.J.F=cont.phi.J.F, 
                 phi.J.M=cont.phi.J.M,
                 phi.A.F=cont.phi.A.F, 
                 phi.A.M=cont.phi.A.M,
                 omega.F=cont.omega.F, 
                 omega.M=cont.omega.M,
                 prop.F=cont.prop.F, 
                 prop.M=cont.prop.M) %>%
      gather() %>%
      group_by(key) %>%
      summarise(year=t1+1986, mean=mean(value), 
                low=quantile(value, 0.025), high=quantile(value, 0.975))

return(df)
})

diff_lams <- ldply(1:(nyears-2),
function(t1) {
  t2 <- t1 + 1 
  lam_diff <- mean(ipm_sim[["lambda"]][,t2] - ipm_sim[["lambda"]][,t1])
  df <- data_frame(year=t1+1986, diff=lam_diff)
})

yearly_cont_summary <- yearly_cont_summary %>% 
                        left_join(diff_lams, by="year")

# example: extracting top two yearly contributors
#           per year in the same direction as lambda
# top_yearly_conts <- yearly_cont_summary %>% 
#                       group_by(year) %>% 
#                       subset((mean>0) == (diff>0)) %>% 
#                       top_n(2, abs(mean))


# Correlations among vital rates-----------------------------------------------
corCellNames <- c("cs", 
                 "hs", 
                 "fs",
                 "phi.J.F", 
                 "phi.J.M",
                 "phi.A.F", 
                 "phi.A.M",
                 "omega.F", 
                 "omega.M",
                 "prop.F", 
                 "prop.M")

LTREcors_max <- matrix(0, 11, 11,
                       dimnames=list(corCellNames, corCellNames))

LTREcors_low <- matrix(0, 11, 11,
                       dimnames=list(corCellNames, corCellNames))

LTREcors_high <- matrix(0, 11, 11,
                       dimnames=list(corCellNames, corCellNames))

LTREcors_P <- matrix(0, 11, 11,
                       dimnames=list(corCellNames, corCellNames))

# calculate each correlation among vital rates, including BCI
# remember, varcors are extracted from LTRE #1 covariance matrix
# these resemble the results you'd find if you ran cor() on 
# ipm_sim results
for (r in 1:11) {
  for (c in r:11) {
    values <- c()
    for (i in 1:numIters) {
      values <- c(values, varcors[[i]][r,c])
    }
    m <- density(values, na.rm=TRUE)
    max <- m$x[which(m$y==max(m$y))][1]
    
    LTREcors_max[r,c] <- max
    LTREcors_low[r,c] <- quantile(values, 0.025)
    LTREcors_high[r,c] <- quantile(values, 0.975)
    LTREcors_P[r,c] <- length(values[values>0]) / numIters
  }
}
