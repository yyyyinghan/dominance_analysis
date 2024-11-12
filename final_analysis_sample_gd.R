#########################################################################################################
############################## Results included in the final dissertation ###############################
#########################################################################################################

# I. Compute tau & r between sample RGD & ranks of averaged GD across 1000 bootstrap samples; RMSE;  
#.   Results => 2 inference methods * 50 random samples * 8 conditions = 800 tau & r & RMSE
total_cond <-9 # number of simulation conditions

# Use function KendallTauB() to compute tau
library(DescTools)
gd_res_all2 <- list()

# GD results of first two conditions (cannot compute tau & r for first two conditions)
for (i in 1:total_cond){
  #i = 1
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_avg", ".xlsx")
  pth2 <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_da_cond", i, ".RData")
  dat_sample <- readRDS(pth2)
  avg_gd_full <- read.xlsx(pth, sheet = "avg_gd_full", colNames = TRUE)
  avg_gd_bfm <- read.xlsx(pth, sheet = "avg_gd_bfm", colNames = TRUE)
  avg_rgd_full <- read.xlsx(pth, sheet = "avg_rgd_full", colNames = TRUE)
  avg_rgd_bfm <- read.xlsx(pth, sheet = "avg_rgd_bfm", colNames = TRUE)
  
  tau1 <- vector()
  tau2 <- vector()
  cor1 <- vector()
  cor2 <- vector()
  
  for (j in 1:50){
    sgd <- (dat_sample[[j]]$GD)[[1]]
    sgd <- c(t(sgd))
    
    bgd1 <-c(t(avg_gd_full[j,]))
    bgd2 <-c(t(avg_gd_bfm[j,]))
    tau1[j] <- KendallTauB(sgd, bgd1, conf.level=0.95)[1]
    tau2[j] <- KendallTauB(sgd, bgd2, conf.level=0.95)[1]
    cor1[j] <- cor(sgd, bgd1, method = "pearson")
    cor2[j] <- cor(sgd, bgd2, method = "pearson")
    
  }
  
  res <- data.frame (tau_full = tau1, tau_bfm = tau2, cor_full = cor1, cor_bfm = cor2)
  gd_res_all2[[i]] <- res
}

# Save results in a xlsx file
gd_list2 <- list("cond1" = gd_res_all2[[1]], "cond2" = gd_res_all2[[2]], "cond3" = gd_res_all2[[3]],
                "cond4" = gd_res_all2[[4]], "cond5" = gd_res_all2[[5]], "cond6" = gd_res_all2[[6]],
                "cond7" = gd_res_all2[[7]], "cond8" = gd_res_all2[[8]], "cond9" = gd_res_all2[[9]])

write.xlsx(gd_list2, "/Users/ying/Desktop/Dissertation writing/Analysis results/gd_results_sample_final.xlsx", rowNames = FALSE, overwrite = TRUE) 



