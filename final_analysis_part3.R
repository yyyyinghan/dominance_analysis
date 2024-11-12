##################################################################################
### Analysis III: Calculate entropy for each BFM distribution ####################
##################################################################################
setwd("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary")
total_cond <- 9

# create a function to calculate entropy
entropy_fun <- function(dat){
  m <- data.frame(mod_num = 1:31, mod_prob = rep(0, times = 31))
  for (j in 1:nrow(m)){
    if (m[j,1]%in%dat[,1]){
      m[j,2] <- subset(dat, Var1 == m[j,1])$Freq/1000
    }else{
      m[j,2] <- 0
    }
  }
  entropy <- 0
  for (l in 1:31){
    if (m[l,2] == 0){
      entropy <-entropy # assuming log(0) = 0
    }else{
      entropy <-entropy + m[l,2]*log2(m[l,2]) 
    }
  }
  return(-entropy)
}

entropy_all <- matrix (, nrow = 50, ncol = total_cond) # store all entropy results in a matrix
for (k in 1:total_cond){
  cond_num = k
  
  for (i in 1:50){
    pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")
    dat <- readRDS(pth)[[5]]
    dat_bfm <- data.frame(table(dat$X1))
    entropy_all[i,k] <-entropy_fun(dat_bfm)
  }
}

# save the results
write.csv(entropy_all, "entropy_all.csv", row.names = FALSE)


