# III. Compute DD (Dominance Distance)
#.   Results => 2 inference methods * 50 random samples * 8 conditions = 800 DD
total_cond <- 9
library(openxlsx)

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
  
  pop_com <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_comp.xlsx", sheet = cond_num,  colNames = TRUE)
  pop_con <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_cond.xlsx", sheet = cond_num, colNames = TRUE)
  pop_gen <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_gen.xlsx", sheet = cond_num, colNames = TRUE)
  
  v_pop_com <- c(pop_com[,1], pop_com[,2], pop_com[,3], pop_com[,4], pop_com[,5])
  v_pop_con <- c(pop_con[,1], pop_con[,2], pop_con[,3], pop_con[,4], pop_con[,5])
  v_pop_gen <- c(pop_gen[,1], pop_gen[,2], pop_gen[,3], pop_gen[,4], pop_gen[,5])
  
  dd_com1 <- vector()
  dd_com2 <- vector()
  dd_con1 <- vector()
  dd_con2 <- vector()
  dd_gen1 <- vector()
  dd_gen2 <- vector()
  for (i in 1:50){
    dat_com1 <- dm1_com[i,] 
    dat_com2 <- dm2_com[i,]
    dat_con1 <- dm1_con[i,] 
    dat_con2 <- dm2_con[i,]
    dat_gen1 <- dm1_gen[i,] 
    dat_gen2 <- dm2_gen[i,]
    
    dd_com1[i] <- sqrt(sum((v_pop_com - dat_com1)^2))
    dd_com2[i] <- sqrt(sum((v_pop_com - dat_com2)^2))
    
    dd_con1[i] <- sqrt(sum((v_pop_con - dat_con1)^2))
    dd_con2[i] <- sqrt(sum((v_pop_con - dat_con2)^2))
    
    dd_gen1[i] <- sqrt(sum((v_pop_gen - dat_gen1)^2))
    dd_gen2[i] <- sqrt(sum((v_pop_gen - dat_gen2)^2))
    
  }
  dm_res_all[[k]] <- data.frame(com_full = dd_com1, com_bfm = dd_com2, con_full = dd_con1, con_bfm = dd_con2, 
                                gen_full = dd_gen1, gen_bfm = dd_gen2)
}

# Save results in an excel file
dm_list <- list("cond1" = dm_res_all[[1]], "cond2" = dm_res_all[[2]], "cond3" = dm_res_all[[3]],
                "cond4" = dm_res_all[[4]], "cond5" = dm_res_all[[5]], "cond6" = dm_res_all[[6]],
                "cond7" = dm_res_all[[7]], "cond8" = dm_res_all[[8]], "cond9" = dm_res_all[[9]])

write.xlsx(dm_list, "/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/dm_results_final.xlsx", rowNames = FALSE, overwrite = TRUE) 

# IV. Obtain DD between pop DM and DM of each bootstrap sample for 50000 cases ( 50 * 1000)
dd_micro <- list()
for (i in 1:total_cond){
  cond_num <- i
  pop_com <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_comp.xlsx", sheet = cond_num,  colNames = TRUE)
  pop_con <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_cond.xlsx", sheet = cond_num, colNames = TRUE)
  pop_gen <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/pop_results/pop_da_gen.xlsx", sheet = cond_num, colNames = TRUE)
  
  v_pop_com <- c(pop_com[,1], pop_com[,2], pop_com[,3], pop_com[,4], pop_com[,5])
  v_pop_con <- c(pop_con[,1], pop_con[,2], pop_con[,3], pop_con[,4], pop_con[,5])
  v_pop_gen <- c(pop_gen[,1], pop_gen[,2], pop_gen[,3], pop_gen[,4], pop_gen[,5])
  
  pth3 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_dm_full", ".RData")
  pth4 <- paste0("/Users/ying/Desktop/Dissertation writing/Analysis results/cond", i, "_dm_bfm", ".RData")
  
  dm_com_full <-matrix(, nrow = 0, ncol = 25)
  dm_com_bfm <-matrix(, nrow = 0, ncol = 25)
  dm_con_full <-matrix(, nrow = 0, ncol = 25)
  dm_con_bfm <-matrix(, nrow = 0, ncol = 25)
  dm_gen_full <-matrix(, nrow = 0, ncol = 25)
  dm_gen_bfm <-matrix(, nrow = 0, ncol = 25)
  
  for (j in 1:50){
    dm_com_full <- rbind(dm_com_full, readRDS(pth3)[[j]][[1]])
    dm_com_bfm <- rbind(dm_com_bfm, readRDS(pth4)[[j]][[1]])
    dm_con_full <- rbind(dm_con_full, readRDS(pth3)[[j]][[2]])
    dm_con_bfm <- rbind(dm_con_bfm, readRDS(pth4)[[j]][[2]])
    dm_gen_full <- rbind(dm_gen_full, readRDS(pth3)[[j]][[3]])
    dm_gen_bfm <- rbind(dm_gen_bfm, readRDS(pth4)[[j]][[3]])
  }
  
  #nrow(dm_com_bfm[complete.cases(dm_com_bfm),])
  #nrow(dm_con_bfm[complete.cases(dm_con_bfm),])
  #nrow(dm_gen_bfm[!complete.cases(dm_gen_bfm),])
  
  # DD between pop DM and DM of each bootstrap sample 
  DD_COM1 <- vector()
  DD_COM2 <- vector()
  DD_CON1 <- vector()
  DD_CON2 <- vector()
  DD_GEN1 <- vector()
  DD_GEN2 <- vector()
  
  for (k in 1:nrow(dm_com_full)){
    DD_COM1[k] <- sqrt(sum((v_pop_com - dm_com_full[k,])^2))
    DD_CON1[k] <- sqrt(sum((v_pop_con - dm_con_full[k,])^2))
    DD_GEN1[k] <- sqrt(sum((v_pop_gen - dm_gen_full[k,])^2))
    
    if (is.na(dm_com_bfm[k,1])){
      DD_COM2[k] <- NA
      DD_CON2[k] <- NA
      DD_GEN2[k] <- NA
    }else{
      DD_COM2[k] <- sqrt(sum((v_pop_com - dm_com_bfm[k,])^2))
      DD_CON2[k] <- sqrt(sum((v_pop_con - dm_con_bfm[k,])^2))
      DD_GEN2[k] <- sqrt(sum((v_pop_gen - dm_gen_bfm[k,])^2))
    }
    print(k)
  }
  dd_micro[[i]] <- data.frame(DD_COM_FULL = DD_COM1, DD_COM_BFM = DD_COM2, DD_COM_diff = DD_COM1 - DD_COM2,
                              DD_CON_FULL = DD_CON1, DD_CON_BFM = DD_CON2, DD_CON_diff = DD_CON1 - DD_CON2,
                              DD_GEN_FULL = DD_GEN1, DD_GEN_BFM = DD_GEN2, DD_GEN_diff = DD_GEN1 - DD_GEN2)
}  

# Save results in an excel file
dm_list2 <- list("cond1" = dd_micro[[1]], "cond2" = dd_micro[[2]], "cond3" = dd_micro[[3]],
                 "cond4" = dd_micro[[4]], "cond5" = dd_micro[[5]], "cond6" = dd_micro[[6]],
                 "cond7" = dd_micro[[7]], "cond8" = dd_micro[[8]], "cond9" = dd_micro[[9]])

write.xlsx(dm_list2, "dm_results_micro.xlsx", rowNames = FALSE, overwrite = TRUE) 

