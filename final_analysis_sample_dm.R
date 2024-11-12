# II. Compute DD (Dominance Distance)
total_cond <- 9

# Obtain vectorized DM of 3 types of 50 random samples
sample_dm_lst <- list()
for (i in 1:total_cond){
  pth2 <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_da_cond", i, ".RData")
  dat_sample <- readRDS(pth2)
  
  dm_f1 <- matrix(, nrow = 50, ncol = 25)
  dm_f2 <- matrix(, nrow = 50, ncol = 25)
  dm_f3 <- matrix(, nrow = 50, ncol = 25)
  
  for (j in 1:50){
    dm_f1[j,] <- c(dat_sample[[j]]$DM1)
    dm_f2[j,] <- c(dat_sample[[j]]$DM2)
    dm_f3[j,] <- c(dat_sample[[j]]$DM3)
    
  }
  
  sample_dm_lst[[i]] <- list(DM1 = dm_f1, DM2 = dm_f2, DM3 = dm_f3)

}


# Compute of DD
dm_res_all <- list() # store all DM results in a list

for (k in 1:total_cond){
  cond_num <- k
  
  pth1 <-paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_avg_dm_full", ".xlsx")
  dm1_com <-read.xlsx(pth1, sheet = "avg_dm_com_full", colNames = TRUE)
  dm1_con <-read.xlsx(pth1, sheet = "avg_dm_con_full", colNames = TRUE)
  dm1_gen <-read.xlsx(pth1, sheet = "avg_dm_gen_full", colNames = TRUE)
  
  pth2 <-paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", cond_num, "_avg_dm_bfm", ".xlsx")
  dm2_com <-read.xlsx(pth2, sheet = "avg_dm_com_bfm", colNames = TRUE)
  dm2_con <-read.xlsx(pth2, sheet = "avg_dm_con_bfm", colNames = TRUE)
  dm2_gen <-read.xlsx(pth2, sheet = "avg_dm_gen_bfm", colNames = TRUE)
  
  dd_com1 <- vector()
  dd_com2 <- vector()
  dd_con1 <- vector()
  dd_con2 <- vector()
  dd_gen1 <- vector()
  dd_gen2 <- vector()
  
  for (i in 1:50){
    
    sample_com <-  (sample_dm_lst[[k]]$DM1)[i,]
    sample_con <- (sample_dm_lst[[k]]$DM2)[i,]
    sample_gen <- (sample_dm_lst[[k]]$DM3)[i,]
    
    dat_com1 <- dm1_com[i,] 
    dat_com2 <- dm2_com[i,]
    dat_con1 <- dm1_con[i,] 
    dat_con2 <- dm2_con[i,]
    dat_gen1 <- dm1_gen[i,] 
    dat_gen2 <- dm2_gen[i,]
    
    dd_com1[i] <- sqrt(sum((sample_com - dat_com1)^2))
    dd_com2[i] <- sqrt(sum((sample_com - dat_com2)^2))
    
    dd_con1[i] <- sqrt(sum((sample_con - dat_con1)^2))
    dd_con2[i] <- sqrt(sum((sample_con - dat_con2)^2))
    
    dd_gen1[i] <- sqrt(sum((sample_gen - dat_gen1)^2))
    dd_gen2[i] <- sqrt(sum((sample_gen - dat_gen2)^2))
    
  }
  dm_res_all[[k]] <- data.frame(com_full = dd_com1, com_bfm = dd_com2, con_full = dd_con1, con_bfm = dd_con2, 
                                gen_full = dd_gen1, gen_bfm = dd_gen2)
}

# Save results in an excel file
dm_list <- list("cond1" = dm_res_all[[1]], "cond2" = dm_res_all[[2]], "cond3" = dm_res_all[[3]],
                "cond4" = dm_res_all[[4]], "cond5" = dm_res_all[[5]], "cond6" = dm_res_all[[6]],
                "cond7" = dm_res_all[[7]], "cond8" = dm_res_all[[8]], "cond9" = dm_res_all[[9]])

write.xlsx(dm_list, "/Users/ying/Desktop/Dissertation writing/Analysis results/dm_results_sample_final.xlsx", rowNames = FALSE, overwrite = TRUE) 


