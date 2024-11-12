#########################################################################################################
############################## Results included in the final dissertation ###############################
#########################################################################################################
setwd("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary")

# I. Compute tau between pop RGD & rank of averaged GD across 1000 bootstrap samples; 
# II. Compute r betwen pop GD & averaged GD across 1000 bootstrap samples; 
# III. Compute RMSE betwen pop GD & averaged GD across 1000 bootstrap samples; 
total_cond <-9 # number of simulation conditions

# 1. Load data
library(openxlsx)
pop_gd <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_GD.xlsx", colNames = TRUE)
pop_rgd <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_RGD.xlsx", colNames = TRUE)

# Use function KendallTauB() to compute tau
library(DescTools)
gd_res_all <- list()

# GD results of first two conditions (cannot compute tau & r for first two conditions)
for (i in 1:2){
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_avg", ".xlsx")
  avg_gd_full <- read.xlsx(pth, sheet = "avg_gd_full", colNames = TRUE)
  avg_gd_bfm <- read.xlsx(pth, sheet = "avg_gd_bfm", colNames = TRUE)
  avg_rgd_full <- read.xlsx(pth, sheet = "avg_rgd_full", colNames = TRUE)
  avg_rgd_bfm <- read.xlsx(pth, sheet = "avg_rgd_bfm", colNames = TRUE)
  
  pgd <- c(t(pop_gd[i,]))
  rpgd <- c(t(pop_rgd[i,]))
  
  rmse1 <- vector()
  rmse2 <- vector()
  
  for (j in 1:50){
    rmse1[j] <- sqrt(sum((pgd - c(t(avg_gd_full[j,])))^2)/5)
    rmse2[j] <- sqrt(sum((pgd - c(t(avg_gd_bfm[j,])))^2)/5)
  }
  res <- data.frame (rmse_full = rmse1, rmse_bfm = rmse2)
  gd_res_all[[i]] <- res
}

# GD results condition 3~9
for (i in 3:total_cond){
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_avg", ".xlsx")
  avg_gd_full <- read.xlsx(pth, sheet = "avg_gd_full", colNames = TRUE)
  avg_gd_bfm <- read.xlsx(pth, sheet = "avg_gd_bfm", colNames = TRUE)
  avg_rgd_full <- read.xlsx(pth, sheet = "avg_rgd_full", colNames = TRUE)
  avg_rgd_bfm <- read.xlsx(pth, sheet = "avg_rgd_bfm", colNames = TRUE)
  
  pgd <- c(t(pop_gd[i,]))
  rpgd <- c(t(pop_rgd[i,]))
    
  tau1 <- vector()
  tau2 <- vector()
  cor1 <- vector()
  cor2 <- vector()
  rmse1 <- vector()
  rmse2 <- vector()
  
  for (j in 1:50){
    bgd1 <-c(t(avg_gd_full[j,]))
    bgd2 <-c(t(avg_gd_bfm[j,]))
    tau1[j] <- KendallTauB(pgd, bgd1, conf.level=0.95)[1]
    tau2[j] <- KendallTauB(pgd, bgd2, conf.level=0.95)[1]
    cor1[j] <- cor(pgd, bgd1, method = "pearson")
    cor2[j] <- cor(pgd, bgd2, method = "pearson")
    rmse1[j] <- sqrt(sum((pgd - c(t(avg_gd_full[j,])))^2)/5)
    rmse2[j] <- sqrt(sum((pgd - c(t(avg_gd_bfm[j,])))^2)/5)
  }
  
  res <- data.frame (tau_full = tau1, tau_bfm = tau2, cor_full = cor1, cor_bfm = cor2, rmse_full = rmse1, rmse_bfm = rmse2)
  gd_res_all[[i]] <- res
}

######################################################################################################
# check out GD difference for each predictor
  i = 3
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_avg", ".xlsx")
  avg_gd_full <- read.xlsx(pth, sheet = "avg_gd_full", colNames = TRUE)
  avg_gd_bfm <- read.xlsx(pth, sheet = "avg_gd_bfm", colNames = TRUE)
  
  pgd <- c(t(pop_gd[i,]))
  gd_diff1 <- matrix (, nrow = 50, ncol = 5)
  gd_diff2 <- matrix (, nrow = 50, ncol = 5)
  
  for (j in 1:50){
    gd_diff1[j,] <- (c(t(pgd - avg_gd_full[j,])))^2
    gd_diff2[j,] <- (c(t(pgd - avg_gd_bfm[j,])))^2
    print(j)
  }
  
  ######################################################################################################

# Save results in an excel file
gd_list <- list("cond1" = gd_res_all[[1]], "cond2" = gd_res_all[[2]], "cond3" = gd_res_all[[3]],
                "cond4" = gd_res_all[[4]], "cond5" = gd_res_all[[5]], "cond6" = gd_res_all[[6]],
                "cond7" = gd_res_all[[7]], "cond8" = gd_res_all[[8]], "cond9" = gd_res_all[[9]])

write.xlsx(gd_list, "gd_results_final.xlsx", rowNames = FALSE, overwrite = TRUE) 

# II. Obtain tau between pop RGD and RGD of each bootstrap sample for 50000 cases ( 50 * 1000)
tau_micro <- list()

for (i in 3:total_cond){
  pgd <- c(t(pop_gd[i,]))
  rpgd <- c(t(pop_rgd[i,]))
  
  pth3 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_rgd_full", ".RData")
  pth4 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_rgd_bfm", ".RData")
  pth5 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_full", ".RData")
  pth6 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_bfm", ".RData")
  
  rgd_full <- matrix(, nrow = 0, ncol = 5)
  rgd_bfm <- matrix(, nrow = 0, ncol = 5)
  gd_full <- matrix(, nrow = 0, ncol = 5)
  gd_bfm <- matrix(, nrow = 0, ncol = 5)
  
  for (j in 1:50){
    rgd_full <- rbind(rgd_full, readRDS(pth3)[[j]])
    rgd_bfm <- rbind(rgd_bfm, readRDS(pth4)[[j]])
    gd_full <- rbind(gd_full, readRDS(pth5)[[j]])
    gd_bfm <- rbind(gd_bfm, readRDS(pth6)[[j]])
  }
  
  nrow(rgd_full[complete.cases(rgd_full),]) # 50000
  nrow(rgd_bfm[!complete.cases(rgd_bfm),]) # 47223
  nrow(gd_full[complete.cases(gd_full),]) # 
  nrow(gd_bfm[!complete.cases(gd_bfm),]) # 
  
  # tau between pop RGD and RGD of each bootstrap sample (use pop_rgd & rgd_full, rgd_bfm)
  t1 <- vector()
  t2 <- vector()
  
  for (k in 1:nrow(rgd_full)){
    t1[k] <- KendallTauB(rpgd, c(t(rgd_full[k,])), conf.level=0.95)[1]
    
    if (is.na(rgd_bfm[k,1])){ # entire row is missing for BFM only includes one predictor
      t2[k] <- NA 
    }else{
      t2[k] <- KendallTauB(rpgd, c(t(rgd_bfm[k,])), conf.level=0.95)[1]
    }
    print (k)
  }
  tau_micro[[i]] <- data.frame(tau_full = t1, tau_bfm = t2, tau_diff = t2 - t1)
}  

# Save results in an excel file
gd_list2 <- list("cond1" = tau_micro[[1]], "cond2" = tau_micro[[2]], "cond3" = tau_micro[[3]],
                "cond4" = tau_micro[[4]], "cond5" = tau_micro[[5]], "cond6" = tau_micro[[6]],
                "cond7" = tau_micro[[7]], "cond8" = tau_micro[[8]], "cond9" = tau_micro[[9]])

write.xlsx(gd_list2, "gd_results_micro.xlsx", rowNames = FALSE, overwrite = TRUE) 


###########################################################################################################
i = 3
pgd <- c(t(pop_gd[i,]))
pth5 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_full", ".RData")
pth6 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_bfm", ".RData")

gd_full <- matrix(, nrow = 0, ncol = 5)
gd_bfm <- matrix(, nrow = 0, ncol = 5)

for (j in 1:50){
  gd_full <- rbind(gd_full, readRDS(pth5)[[j]])
  gd_bfm <- rbind(gd_bfm, readRDS(pth6)[[j]])
}

nrow(gd_full[complete.cases(gd_full),]) # 50000
nrow(gd_bfm[!complete.cases(gd_bfm),]) # 46977

gd_diff1 <- matrix(, nrow = nrow(gd_full), ncol = 5)
gd_diff2 <- matrix(, nrow = nrow(gd_bfm), ncol = 5)
for (j in 1:nrow(gd_full)){
  gd_diff1[j,] <- pgd - gd_full[j,]
  gd_diff2[j,] <- pgd - gd_bfm[j,]
  print(j)
}

gd_diff_micro <- data.frame(cbind(gd_diff1, gd_diff2))
colnames(gd_diff_micro) <- c("x1_diff_full", "x2_diff_full", "x3_diff_full", "x4_diff_full", "x5_diff_full",
                             "x1_diff_bfm", "x2_diff_bfm", "x3_diff_bfm", "x4_diff_bfm", "x5_diff_bfm")


gd_diff_micro$sq_diff1_full <- sqrt((gd_diff_micro$x1_diff_full)^2)
gd_diff_micro$sq_diff2_full <- sqrt((gd_diff_micro$x2_diff_full)^2)
gd_diff_micro$sq_diff3_full <- sqrt((gd_diff_micro$x3_diff_full)^2)
gd_diff_micro$sq_diff4_full <- sqrt((gd_diff_micro$x4_diff_full)^2)
gd_diff_micro$sq_diff5_full <- sqrt((gd_diff_micro$x1_diff_full)^2)

gd_diff_micro$sq_diff1_bfm <- sqrt((gd_diff_micro$x1_diff_bfm)^2)
gd_diff_micro$sq_diff2_bfm <- sqrt((gd_diff_micro$x2_diff_bfm)^2)
gd_diff_micro$sq_diff3_bfm <- sqrt((gd_diff_micro$x3_diff_bfm)^2)
gd_diff_micro$sq_diff4_bfm <- sqrt((gd_diff_micro$x4_diff_bfm)^2)
gd_diff_micro$sq_diff5_bfm <- sqrt((gd_diff_micro$x5_diff_bfm)^2)

gd_diff_micro$final_diff1 <- gd_diff_micro$sq_diff1_full - gd_diff_micro$sq_diff1_bfm
gd_diff_micro$final_diff2 <- gd_diff_micro$sq_diff2_full - gd_diff_micro$sq_diff2_bfm
gd_diff_micro$final_diff3 <- gd_diff_micro$sq_diff3_full - gd_diff_micro$sq_diff3_bfm
gd_diff_micro$final_diff4 <- gd_diff_micro$sq_diff4_full - gd_diff_micro$sq_diff4_bfm
gd_diff_micro$final_diff5 <- gd_diff_micro$sq_diff5_full - gd_diff_micro$sq_diff5_bfm

write.xlsx(describe(gd_diff_micro), "gd_diff_micro.xlsx", rowNames = TRUE)



# III. RMSE micro
rmse_micro <- list()
for (i in 1:total_cond){
  pgd <- c(t(pop_gd[i,]))
  pth5 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_full", ".RData")
  pth6 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_gd_bfm", ".RData")
  
  gd_full <- matrix(, nrow = 0, ncol = 5)
  gd_bfm <- matrix(, nrow = 0, ncol = 5)
  
  for (j in 1:50){
    gd_full <- rbind(gd_full, readRDS(pth5)[[j]])
    gd_bfm <- rbind(gd_bfm, readRDS(pth6)[[j]])
  }

  nrow(gd_full[complete.cases(gd_full),]) # 50000
  nrow(gd_bfm[!complete.cases(gd_bfm),]) # 46977 (2777)
  
  # insert zeros for missing GD of BFM
  for (l in 1:nrow(gd_bfm)){
    #l = 2
    temp <- gd_bfm[l,]
    if (length(temp[is.na(temp)]) == 0|length(temp[is.na(temp)]) == 5){
      gd_bfm[l,] <- gd_bfm[l,]
    }else{
      gd_bfm[l,][is.na(gd_bfm[l,])] <- 0
    }
    print(l)
  }
  
  
  # tau between pop RGD and RGD of each bootstrap sample (use pop_rgd & rgd_full, rgd_bfm)
  rmse1 <- vector()
  rmse2 <- vector()
  
  for (k in 1:nrow(gd_full)){
    rmse1[k] <- sqrt(sum((pgd - c(t(gd_full[k,])))^2)/5)
    
    if (is.na(gd_full[k,1])){
      rmse2[k] <- NA
    }else{
      rmse2[k] <- sqrt(sum((pgd - c(t(gd_bfm[k,])))^2)/5)
    }
    print(k)
  }
  
  
  rmse_micro[[i]] <- data.frame(rmse_full = rmse1, rmse_bfm = rmse2, rmse_diff = rmse1 - rmse1)
}  
