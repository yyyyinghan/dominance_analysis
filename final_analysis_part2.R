##################################################################################
### Analysis II: Vectorized average dominance matrices (3 type * 2 methods) ######
# Reminder: Dijs for equal dominance & diagonal elements are set up as 0.5. 
#           DijS for unestablished dominance are also set up as 0.5.
##################################################################################
setwd("/Users/ying/Desktop/Dissertation writing")

#################### This is for one condition ! #####################
#### REMINDER: Need to run 9 times using 9 different condition numbers
######################################################################
cond_num = 9

b_size = 1000
'%ni%' <- Negate('%in%') # "not included" function

# create a function that creates 5*5 matrix for BFM DM
bfm_dm <- function(dm){
  var_lst<-rownames(dm)
  mm<-matrix(, nrow = 5, ncol = 5)
  rownames(mm)<- c("X1", "X2", "X3", "X4", "X5")
  colnames(mm)<- c("X1", "X2", "X3", "X4", "X5")
  
  for (i in 1:5){
    for (j in 1:5){
      if (rownames(mm)[i]%in%var_lst & colnames(mm)[j]%in%var_lst){
        mm[i,j]<-as.numeric(dm[rownames(mm)[i], colnames(mm)[j]])
      }else if(rownames(mm)[i]%in%var_lst & colnames(mm)[j]%ni%var_lst){
        mm[i,j]<-1
      }else if(rownames(mm)[i]%ni%var_lst & colnames(mm)[j]%in%var_lst){
        mm[i,j]<-0
      }else{
        mm[i,j]<-0.5
      } 
    }
  }
  return(mm)
}

dm_lst1 <- list() # dm of full DA
dm_lst2 <- list() # dm of BFM DA
dm_com1 <- matrix (, nrow = 50, ncol = 25) # mean complete dm of full DA
dm_com2 <- matrix (, nrow = 50, ncol = 25) # mean complete dm of BFM DA
dm_con1 <- matrix (, nrow = 50, ncol = 25) # mean conditional dm of full DA
dm_con2 <- matrix (, nrow = 50, ncol = 25) # mean conditional dm of BFM DA
dm_gen1 <- matrix (, nrow = 50, ncol = 25) # mean general dm of full DA
dm_gen2 <- matrix (, nrow = 50, ncol = 25) # mean general dm of BFM DA

for (i in 1:50){
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")
  dat <- readRDS(pth)
  
  dm_f1 <- matrix(, nrow = b_size, ncol = 25)
  dm_f2 <- matrix(, nrow = b_size, ncol = 25)
  dm_f3 <- matrix(, nrow = b_size, ncol = 25)
  dm_bfm1 <- matrix(, nrow = b_size, ncol = 25)
  dm_bfm2 <- matrix(, nrow = b_size, ncol = 25)
  dm_bfm3 <- matrix(, nrow = b_size, ncol = 25)
  
  for (j in 1:b_size){
    
    dm_f1[j,] <- c(dat[[1]][[j]]$DM1)
    dm_f2[j,] <- c(dat[[1]][[j]]$DM2)
    dm_f3[j,] <- c(dat[[1]][[j]]$DM3)
    
    if(dat[[5]][j,1] <= 5){
      next
    }
    dm_bfm1[j,] <- c(bfm_dm(dat[[2]][[j]]$DM1))
    dm_bfm2[j,] <- c(bfm_dm(dat[[2]][[j]]$DM2))
    dm_bfm3[j,] <- c(bfm_dm(dat[[2]][[j]]$DM3))
    print(j)
  }
  
  dm_lst1[[i]] <- list(dm_f1, dm_f2, dm_f3)
  dm_lst2[[i]] <- list(dm_bfm1, dm_bfm2, dm_bfm3)
  dm_com1[i,] <- colMeans(dm_f1, na.rm = TRUE)
  dm_con1[i,] <- colMeans(dm_f2, na.rm = TRUE)
  dm_gen1[i,] <- colMeans(dm_f3, na.rm = TRUE)
  dm_com2[i,] <- colMeans(dm_bfm1, na.rm = TRUE)
  dm_con2[i,] <- colMeans(dm_bfm2, na.rm = TRUE)
  dm_gen2[i,] <- colMeans(dm_bfm3, na.rm = TRUE)
  
}

# Save the results 
saveRDS(dm_lst1, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_dm_full", ".RData"))
saveRDS(dm_lst2, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_dm_bfm", ".RData"))

library(openxlsx)
avg_results1 <-list("avg_dm_com_full" = dm_com1, "avg_dm_con_full" = dm_con1,
                    "avg_dm_gen_full" = dm_gen1)
avg_results2 <-list("avg_dm_com_bfm" = dm_com2, "avg_dm_con_bfm" = dm_con2,
                    "avg_dm_gen_bfm" = dm_gen2)

write.xlsx(avg_results1, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_avg_dm_full", ".xlsx"), rowNames = FALSE)
write.xlsx(avg_results2, file=paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_avg_dm_bfm", ".xlsx"), rowNames = FALSE)

