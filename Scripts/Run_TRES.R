# ------------------
# Main script for TRES IPM
# Includes calls to run Init, (#)LTRE, and (#)Visualization Scripts
#   Running all as-is will read in data, run JAGS model, and summarize output
#   Uncomment source() calls for either LTRE runs or visualization runs
#
# OUTPUT
#   ipm -- full JAGS ipm posterior from ipm.TRES.bug model
#   out -- dataframe with summarized parameter estimates from IPM
#
# LT 2/20/18
# ------------------

# Initialize packages and data
args <- commandArgs(trailing=TRUE)
if (length(args>0) && grepl("--endYear", args)) {
  endYear <- args[grep("--endYear", args)+1]  
}

source("Scripts/Init_TRES.R", echo=TRUE)

# Run model ---------------------------------------------------------------
# initializing JAGS data objects and priors
jags.data <- list(nyears=nyears,
                  numNests=numNests,
                  y.F=y.F, y.M=y.M,
                  numEggs=numEggs, 
                  numHatchlings=numHatchlings, 
                  numFledglings=numFledglings,
                  marray.F=marray.F, marray.M=marray.M,
                  r.F=r.F, r.M=r.M,
                  marray.BOX=marray.BOX, r.BOX=r.BOX)

jags.inits <- function() {
  list(m.cs=runif(1,0,10), 
       m.hs=runif(1,0,1), 
       m.fs=runif(1,0,1),
       m.phi.J.F=runif(1,0,1), 
       m.phi.J.M=runif(1,0.1,1), 
       m.phi.A.F=runif(1,0,1), 
       m.phi.A.M=runif(1,0,1),
       m.recap.SY.F=runif(1,0,1), 
       m.recap.SY.M=runif(1,0,1),
       m.recap.ASY.F=runif(1,0,1),
       m.recap.ASY.M=runif(1,0,1),
       m.numImms.F=runif(1,0,100), 
       m.numImms.M=runif(1,0,100),
       N.imm.F=c(NA,round(runif(nyears-1,0,100))), 
       N.imm.M=c(NA,round(runif(nyears-1,0,100))),
       m.phi.BOX=runif(1,0,1),
       m.recap.BOX=runif(1,0,1))
}

# output parameters
params <- c("lambda", "m.lambda",
            "phi.J.F","phi.J.M",
            "phi.A.F","phi.A.M",
            "recap.SY.F", "recap.SY.M",
            "recap.ASY.F", "recap.ASY.M",
            "numImms.F", "numImms.M",
            "omega.F", "omega.M",
            "N.SY.F", "N.SY.M",
            "N.ASY.F", "N.ASY.M",
            "N.imm.F", "N.imm.M",
            "b.F", "b.M",
            "Ntot",
            "cs", "hs", "fs",
            "m.cs", "m.hs", "m.fs",
            "m.phi.J.F", "m.phi.J.M",
            "m.phi.A.F", "m.phi.A.M",
            "m.recap.SY.F", "m.recap.SY.M",
            "m.recap.ASY.F", "m.recap.ASY.M",
            "fit.numEggs", "fit.numEggs.new",
            "fit.numHatchlings", "fit.numHatchlings.new",
            "fit.numFledglings", "fit.numFledglings.new",
            "fit.J.F", "fit.J.F.new", 
            "fit.J.M", "fit.J.M.new", 
            "fit.A.F", "fit.A.F.new", 
            "fit.A.M", "fit.A.M.new",
            "y.fit.A.F", "y.fit.A.F.new",
            "y.fit.A.M", "y.fit.A.M.new",
            "phi.BOX", "recap.BOX", "m.phi.BOX", "m.recap.BOX")

# call JAGS from R
ipm <- jags(data=jags.data, inits=jags.inits, parameters.to.save=params, 
            model.file="Scripts/ipm.TRES.bug",
            n.chains=1, n.thin=20, n.iter=100000, n.burnin=20000, 
            parallel=TRUE)

# summarize ipm output for analyses/visualization
out <- as.data.frame(ipm$summary)
out <- bind_cols(as.data.frame(rownames(out)), out)
colnames(out)[1] <- "param"

out <- separate(out, param, c("param", "year"), sep="\\[|\\]", 
                extra="drop", fill="right")

if (ncol(out) == 13) {
  colnames(out) <- c("param", "year", "mean", "sd", "low", 
                     "twentyfive", "mid", "seventyfive", "high", 
                     "rhat", "neff", "overlap", "f")
} else {
  colnames(out) <- c("param", "year", "mean", "sd", "low",
                     "twentyfive", "mid", "seventyfive", "high",
                     "overlap", "f")
}

out$year <- as.numeric(out$year) + 1986

ipm_sim <- ipm$sims.list    # the raw results at every iterations

saveRDS(out, file=paste0("Output/ipm_out_endYear", endYear, ".rds"))
saveRDS(ipm_sim, file=paste0("Output/ipm_sim_endYear", endYear, ".rds"))

# source("Scripts/LTRE_TRES.R", echo=TRUE)
# source("Scripts/Vis_TRES.R", echo=TRUE)