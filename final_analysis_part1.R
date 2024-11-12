############################################################################
### Analysis I: Obtain GD & averaged bootstrapped GD; RGD & averaged RGD ###
setwd("/Users/ying/Desktop/Dissertation writing")

#################### This is for one condition ! #####################
#### REMINDER: Need to run 8 times using 8 different condition numbers
cond_num <- 9 # specify the condition number
######################################################################
###### specify number of bootstrap samples ###
b_sz = 1000

rgd_lst1 <- list() # ranks of GD, full DA
rgd_lst2<- list() # ranks of GD, BFM DA
avg_rgd1 <- matrix(, nrow = 50, ncol = 5) # averaged ranks of GD, full DA
avg_rgd2 <- matrix(, nrow = 50, ncol = 5) # averaged ranks of GD, BFM DA
gd_lst1 <- list() # GD, full DA
gd_lst2 <- list() # GD, BFM DA
avg_gd1 <- matrix(, nrow = 50, ncol = 5) # averaged GD, full DA
avg_gd2 <- matrix(, nrow = 50, ncol = 5) # averaged GD, BFM DA

for (i in 1:50){
  
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")
  dat <- readRDS(pth)
  
  gd_full <- matrix(, nrow = b_sz, ncol = 5) # matrix to store gd of full DA
  gd_bfm <- matrix(, nrow = b_sz, ncol = 5) # matrix to store gd of BFM
  res_gd_full <- matrix(, nrow = b_sz, ncol = 5) # matrix to store the ranks of gd of full DA
  res_gd_bfm <- matrix(, nrow = b_sz, ncol = 5) # matrix to store the ranks of gd of BFM
  colnames(res_gd_full) <-c("X1", "X2", "X3", "X4", "X5")
  colnames(res_gd_bfm) <-c("X1", "X2", "X3", "X4", "X5")
  colnames(gd_full) <-c("X1", "X2", "X3", "X4", "X5")
  colnames(gd_bfm) <-c("X1", "X2", "X3", "X4", "X5")
  
  for (j in 1:b_sz){
    gd1<- dat[[1]][[j]]$GD$r2
    res_gd_full[j,] <- rank(-gd1)
    gd_full[j,] <- gd1
    
   if(dat[[5]][j,1] <= 5){
     next
   }
    gd2 <- dat[[2]][[j]]$GD$r2
    name_vec <- names(gd2)
    rank_vec <- rank(-gd2)
    
    if(length(gd2) == 2){
      res_gd_bfm[j,name_vec[1]] <- rank_vec[1]
      res_gd_bfm[j,name_vec[2]] <- rank_vec[2]
      res_gd_bfm[j,][is.na(res_gd_bfm[j,])] <- 4
      gd_bfm[j,name_vec[1]] <- gd2[1]
      gd_bfm[j,name_vec[2]] <- gd2[2]
      
    }else if(length(gd2) == 3){
      res_gd_bfm[j,name_vec[1]] <- rank_vec[1]
      res_gd_bfm[j,name_vec[2]] <- rank_vec[2]
      res_gd_bfm[j,name_vec[3]] <- rank_vec[3]
      res_gd_bfm[j,][is.na(res_gd_bfm[j,])] <- 4.5
      gd_bfm[j,name_vec[1]] <- gd2[1]
      gd_bfm[j,name_vec[2]] <- gd2[2]
      gd_bfm[j,name_vec[3]] <- gd2[3]
    }else if(length(gd2) == 4){
      res_gd_bfm[j,name_vec[1]] <- rank_vec[1]
      res_gd_bfm[j,name_vec[2]] <- rank_vec[2]
      res_gd_bfm[j,name_vec[3]] <- rank_vec[3]
      res_gd_bfm[j,name_vec[4]] <- rank_vec[4]
      res_gd_bfm[j,][is.na(res_gd_bfm[j,])] <- 5
      gd_bfm[j,name_vec[1]] <- gd2[1]
      gd_bfm[j,name_vec[2]] <- gd2[2]
      gd_bfm[j,name_vec[3]] <- gd2[3]
      gd_bfm[j,name_vec[4]] <- gd2[4]
    }else{
      res_gd_bfm[j,] <- rank_vec
      gd_bfm[j,] <- gd2
    }
    print(j)
  }
  rgd_lst1[[i]] <-res_gd_full
  rgd_lst2[[i]] <-res_gd_bfm
  gd_lst1[[i]] <-gd_full
  gd_lst2[[i]] <-gd_bfm
  
  avg_rgd1[i,] <- colMeans(res_gd_full, na.rm = TRUE)
  avg_rgd2[i,] <- colMeans(res_gd_bfm, na.rm = TRUE)
  avg_gd1[i,] <- colMeans(gd_full, na.rm = TRUE) 
  avg_gd2[i,] <- colMeans(gd_bfm, na.rm = TRUE)
}

# Save the results 
saveRDS(rgd_lst1, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_rgd_full", ".RData"))
saveRDS(rgd_lst2, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_rgd_bfm", ".RData"))
saveRDS(gd_lst1, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_gd_full", ".RData"))
saveRDS(gd_lst2, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_gd_bfm", ".RData"))

library(openxlsx)
avg_results <-list("avg_rgd_full" = avg_rgd1, "avg_rgd_bfm" = avg_rgd2,
                   "avg_gd_full" = avg_gd1, "avg_gd_bfm" = avg_gd2)  

write.xlsx(avg_results, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_avg", ".xlsx"), rowNames = FALSE)



