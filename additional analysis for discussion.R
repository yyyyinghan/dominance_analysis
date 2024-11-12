##############################################################
# Additional analysis => Table 36 & 37 in Discussion
# When computing RMSE, use relative GD not absolute GD
##############################################################

# load population GD
library(openxlsx)
pop_gd <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_GD.xlsx", colNames = TRUE)

rmse_re_gd <- list()
for (i in 3:4){
  pgd <- c(t(pop_gd[i,]))
  pgd <- pgd/sum(pgd)
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
  
  gd_full2 <- matrix(, nrow = 50000, ncol = 5)
  gd_bfm2 <- matrix(, nrow = 50000, ncol = 5)
  for (k in 1:nrow(gd_full)){
    gd_full2[k,] <-gd_full[k,]/sum(gd_full[k,], na.rm = TRUE)
    gd_bfm2[k,] <-gd_bfm[k,]/sum(gd_bfm[k,], na.rm = TRUE)
  }
  
  gd_full_dat <- data.frame(gd_full2)
  gd_bfm_dat <- data.frame(gd_bfm2)
  gd_full_dat$cond <-rep(1:50, each = 1000)
  gd_bfm_dat$cond <-rep(1:50, each = 1000)
  
  avg_gd_full <- matrix (, nrow = 50, ncol = 5)
  avg_gd_bfm <- matrix (, nrow = 50, ncol = 5)
  
  for (l in 1:50){
    avg_gd_full[l,] <- colMeans(subset(gd_full_dat, cond == l), na.rm = TRUE)[-6]
    avg_gd_bfm[l,] <- colMeans(subset(gd_bfm_dat, cond == l), na.rm = TRUE)[-6]
  }
  
  res <- data.frame(cbind(sqrt((avg_gd_full[,1]-pgd[1])^2), sqrt((avg_gd_bfm[,1]-pgd[1])^2),
                          sqrt((avg_gd_full[,2]-pgd[2])^2), sqrt((avg_gd_bfm[,2]-pgd[2])^2),
                          sqrt((avg_gd_full[,3]-pgd[3])^2), sqrt((avg_gd_bfm[,3]-pgd[3])^2), 
                          sqrt((avg_gd_full[,4]-pgd[4])^2), sqrt((avg_gd_bfm[,4]-pgd[4])^2), 
                          sqrt((avg_gd_full[,5]-pgd[5])^2), sqrt((avg_gd_bfm[,5]-pgd[5])^2)))
 
   colnames(res) <- c("rmse_x1_full", "rmse_x1_bfm", "rmse_x2_full", "rmse_x2_bfm",
                      "rmse_x3_full", "rmse_x3_bfm", "rmse_x4_full", "rmse_x4_bfm",
                      "rmse_x5_full", "rmse_x5_bfm")
   
   rmse_re_gd[[i]] <- res 
}
   
res <- list(cond3 = describe(rmse_re_gd[[3]]), cond4 = describe(rmse_re_gd[[4]]))
write.xlsx(res, "rmse_relative_gd.xlsx", rowNames = TRUE)

