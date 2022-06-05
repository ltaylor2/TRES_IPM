# ------------------
# Initialize the data for TRES IPM
# Either draws directly from datafiles (if available)
#  or loads in pre-formatted data directly (as in repository)
#
# OUTPUT
#   nyears        -- number of timeseries years
#   numNests      -- number of observed nests
#   y.F           -- pop. count females
#   y.M           -- pop. count males
#   numEggs       -- number of eggs across all nests
#   numHatchlings -- number of hatchlings across all nests
#   numFledglings -- number of fledglings across all nests
#   marray.F      -- capture-recapture (female - J and Ad)
#   marray.M      -- capture-recapture (male - J and Ad)
#	marray.BOX	  -- capture-recapture (nest boxes)
#   r.F           -- number of released females (to avoid JAGS cycles)
#   r.M           -- number of released males (to avoid JAGS cycles)
#	r.BOX 		  -- number of released boxes (should be 0)
#
# LT 2022-06-04
# ------------------

# Uncomment to install all necessary packages (and their prereqs)
# 
# install.packages(c("jagsUI", "plyr", "tidyverse", "lubridate", "gtools",
# 				   "cowplot", "matrixStats", "beepr", "OpenStreetMap", 
# 				   "ggsn", "rgdal", "ggmap", "scales", "grid", "sp")

# Loading main packages ---------------------------------------------------
library(jagsUI)
library(tidyverse)
library(gtools)
library(cowplot)

# Reading-in and formatting raw data ------------------
# read in data
individuals <- read_csv("Data/individuals.csv",
						col_types=cols("Band_ID"=col_character(),
									   "Mother_ID"=col_character(),
									   "Father_ID"=col_character()))

nests <- read_csv("Data/nests.csv",
				  col_types=cols("Female_ID"=col_character(),
				  				 "Male_ID"=col_character()))

if (exists("endYear")) {
	individuals <- dplyr::filter(individuals, Birth_Year <= as.numeric(endYear))
	nests <- dplyr::filter(nests, Year <= as.numeric(endYear))
}

nyears <- max(nests$Year) - min(nests$Year) + 1

# Setting up pure population counts
nestCounts <- nests %>%
                group_by(Year) %>%
                dplyr::count()

numNests <- nestCounts$n

missingFemales <- nests %>%
               dplyr::filter(is.na(Female_ID)) %>%
               group_by(Year) %>%
               summarise(missingFemales=n())

missingMales <- nests %>%
             dplyr::filter(is.na(Male_ID)) %>%
             group_by(Year) %>%
             summarise(missingMales=n())

numSexes <- nests %>%
         group_by(Year) %>%
         summarise(knownFemales=length(unique(Female_ID[Female_ID!=""])),
         		   knownMales=length(unique(Male_ID[Male_ID!=""]))) %>%
         left_join(missingFemales, by="Year") %>%
         left_join(missingMales, by="Year")

numSexes[is.na(numSexes$missingFemales), "missingFemales"] <- 0
numSexes[is.na(numSexes$missingMales), "missingMales"] <- 0

popTotals <- numSexes %>%
              transmute(totalFemales=knownFemales+missingFemales, 
              			totalMales=knownMales+missingMales)

y.F <- popTotals$totalFemales
y.M <- popTotals$totalMales

# Setting up productivity data (up through 2003)
prodSummaries <- nests %>%
              group_by(Year) %>%
              summarize(numEggs=sum(Eggs_Laid),
                        numHatchlings=sum(Eggs_Hatched),
                        numFledglings=sum(Fledglings))

numEggs <- prodSummaries$numEggs
numHatchlings <- prodSummaries$numHatchlings
numFledglings <- prodSummaries$numFledglings

# Hard coding remaining sex assignments from observations
individuals[individuals$Band_ID == "910.26434", "Sex"] <- "F?" 
individuals[individuals$Band_ID == "2021.76304", "Sex"] <- "M?"
individuals[individuals$Band_ID == "2031.58387", "Sex"] <- "M?"
individuals[individuals$Band_ID == "2121.65215", "Sex"] <- "F?"
individuals[individuals$Band_ID == "2121.65885", "Sex"] <- "M?"

# split individuals by sexes
females <- individuals %>% subset(Sex=="F" | Sex=="F?")
males <- individuals %>% subset(Sex=="M" | Sex=="M?")
unks <- individuals %>% subset(Sex=="U" & Band_ID != "1560.03952")

# sort unknown-sex individuals 
# all fledglings who never returned to breed randomly
# the one adult breeder of un-assignable sex (1560.03952) was removed
for (r in 1:nrow(unks)) {
  if (runif(1,0,1) < 0.5) {
    females <- bind_rows(females, unks[r,])
  } else {
    males <- bind_rows(males, unks[r,])
  }
}

females.J <- females %>% subset(Banding_Age == "N")
males.J <- males %>% subset(Banding_Age == "N")

females.A <- females %>% subset(Banding_Age != "N")
males.A <- males %>% subset(Banding_Age != "N")

obs.J.F <- matrix(0, nrow=nrow(females.J), ncol=nyears, 
				  dimnames=list(as.character(females.J$Band_ID)))
obs.J.M <- matrix(0, nrow=nrow(males.J), ncol=nyears,
				  dimnames=list(as.character(males.J$Band_ID)))

obs.A.F <- matrix(0, nrow=nrow(females.A), ncol=nyears, 
				  dimnames=list(as.character(females.A$Band_ID)))
obs.A.M <- matrix(0, nrow=nrow(males.A), ncol=nyears, 
				  dimnames=list(as.character(males.A$Band_ID)))

# mark captures and recaptures of breeding adults
for (n in 1:nrow(nests)) {
  id1 <- as.character(nests$Female_ID[n])
  id2 <- as.character(nests$Male_ID[n])
  year <- as.numeric(nests$Year[n]) - 1986

  # will only set one capture value equal to 1, but trying to
  # remain unbiased as to the matching between individual and nest datasets
  obs.J.F[rownames(obs.J.F)==id1 | rownames(obs.J.F)==id2, year] <- 1
  obs.J.M[rownames(obs.J.M)==id1 | rownames(obs.J.M)==id2, year] <- 1

  obs.A.F[rownames(obs.A.F)==id1 | rownames(obs.A.F)==id2, year] <- 1
  obs.A.M[rownames(obs.A.M)==id1 | rownames(obs.A.M)==id2, year] <- 1
}

# assigned first capture of juveniles as nestlings(females and males)
for (f in 1:nrow(obs.J.F)) {
  id <- rownames(obs.J.F)[f]
  birthyear <- subset(individuals, Band_ID == id)$Birth_Year - 1986
  obs.J.F[f,birthyear] <- 1
}
for (m in 1:nrow(obs.J.M)) {
  id <- rownames(obs.J.M)[m]
  birthyear <- subset(individuals, Band_ID == id)$Birth_Year - 1986
  obs.J.M[m,birthyear] <- 1
}

createMArray <- function(obs) {
  nind <- dim(obs)[1]
  n.years <- dim(obs)[2]
  m.array <- matrix(0, nrow=n.years, ncol=n.years+1)

  for (t in 1:n.years) { m.array[t,1] <- sum(obs[,t]) }  # total number of individuals observed in year [row]
  for (i in 1:nind) {
    pos <- which(obs[i,] != 0)
    for (z in 1:(length(pos) - 1)) {
      m.array[pos[z], pos[z+1]] <- m.array[pos[z], pos[z+1]] + 1
    }
  }
  for (t in 1:n.years) {
    m.array[t, n.years+1] <- m.array[t,1] - sum(m.array[t,2:n.years])
  }
  out <- m.array[1:(n.years-1), 2:(n.years+1)]
  return(out)
}


# formulate the m-arrays for adults and first years
marray.J.F <- createMArray(obs.J.F)
marray.J.M <- createMArray(obs.J.M)

marray.A.F <- createMArray(obs.A.F)
marray.A.M <- createMArray(obs.A.M)

marray.F <- rbind(marray.J.F, marray.A.F)
marray.M <- rbind(marray.J.M, marray.A.M)

# number of released individuals during each year
# simple sum across m-array row
# don't need for WinBUGS, but JAGS detects cycles and throws errors
r.F <- rowSums(marray.F)
r.M <- rowSums(marray.M)

# formulate observations for nest boxes
obs.BOX <- matrix(0, nrow=length(unique(nests$Nest_ID)), ncol=nyears, 
				  dimnames=list(unique(nests$Nest_ID)))

# mark "captures and recaptures" of boxes
for (n in 1:nrow(nests)) {
  box <- as.character(nests$Nest_ID[n])
  year <- as.numeric(nests$Year[n]) - 1986
  obs.BOX[rownames(obs.BOX)==box, year] <- 1
}

marray.BOX <- createMArray(obs.BOX)
r.BOX <- rowSums(marray.BOX)

# Saving objects to file
# save(nyears, file="Data/nyears_data")
# save(numNests, file="Data/numNests_data")
# save(y.F, file="Data/y.F_data")
# save(y.M, file="Data/y.M_data")
# save(numEggs, file="Data/numEggs_data")
# save(numHatchlings, file="Data/numHatchlings_data")
# save(numFledglings, file="Data/numFledglings_data")
# save(marray.F, file="Data/marray.F_data")
# save(marray.M, file="Data/marray.M_data")
# save(r.F, file="Data/r.F_data")
# save(r.M, file="Data/r.M_data")
# save(marray.BOX, file="Data/marray.BOX_data")
# save(r.BOX, file="Data/r.BOX_data")

# # Loading R objects that store extracted and formatted data ---------------
# load("Data/nyears_data")        # number of timeseries years
# load("Data/numNests_data")      # number of observed nests
# load("Data/y.F_data")           # pop. count females
# load("Data/y.M_data")           # pop. count males
# load("Data/numEggs_data")       # number of eggs across all nests
# load("Data/numHatchlings_data") # number of hatchlings across all nests
# load("Data/numFledglings_data") # number of fledglings across all nests
# load("Data/marray.F_data")      # capture-recapture (female - J and Ad)
# load("Data/marray.M_data")      # capture-recapture (male - J and Ad)
# load("Data/r.F_data")           # number of released females 
# load("Data/r.M_data")           # number of released males

