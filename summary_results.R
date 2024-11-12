#############################################
# Summary of results & graphic presentation
setwd("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary")
library(ggplot2)
total_cond <- 9

# I. Descriptive statistics
# 1. Averaged r squared & Averaged adjusted r squared across 1000 bootstrap samples
r2_full <- matrix(, nrow = 0, ncol = 2)
r2_bfm <- matrix(, nrow = 0, ncol = 2)

for (k in 1:total_cond){
  cond_num = k
  
  avg_r2_full <- matrix(, nrow = 50, ncol = 2)
  avg_r2_bfm <- matrix(, nrow = 50, ncol = 2)
  
  for (i in 1:50){
    pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")
    dat <- readRDS(pth)
    
    avg_r2_full[i,] <- colMeans(dat[[3]])
    avg_r2_bfm[i,] <- colMeans(dat[[4]])
  }
  
  r2_full <- rbind(r2_full, avg_r2_full)
  r2_bfm <- rbind(r2_bfm, avg_r2_bfm)
}

r2_bs <- data.frame(r2_full = r2_full[,1], adjr2_full = r2_full[,2],
                    r2_bfm = r2_bfm[,1], adjr2_bfm = r2_bfm[,2],
                    cond = rep(c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50))


r2_res <- aggregate (r2_full ~ cond, r2_bs, mean)
r2_res$r2_bfm <- aggregate (r2_bfm ~ cond, r2_bs, mean)[,2]
r2_res$adjr2_full <- aggregate (adjr2_full ~ cond, r2_bs, mean)[,2]
r2_res$adjr2_bfm <- aggregate (adjr2_bfm ~ cond, r2_bs, mean)[,2]
write.csv(r2_res, "r2_results.csv", row.names = FALSE)

# graphic presentation of r2
r2_bs_long<- data.frame(r2 = c(r2_full[,1], r2_bfm[,1]), adjr2 = c(r2_full[,2], r2_bfm[,2]),
                         cond = rep(r2_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), nrow(r2_bs)))

p_r2 <- ggplot(r2_bs_long, aes(x=factor(cond), y=r2, fill = Method)) + geom_boxplot() 
p_r2 <- p_r2 + xlab("Simulation Condition")+ylab('r squared') + theme_classic() + 
        theme(legend.position=c(0.91, 0.91)) + theme(legend.background=element_rect(fill="white", colour="black"))+
        theme(axis.title.x=element_text(size=14, face="bold"))+
        theme(axis.title.y=element_text(size=14, face="bold"))+
        theme(legend.title = element_text(size=14), 
              legend.text = element_text(size=12))+
        theme(axis.text.x = element_text(size = 12))+
        theme(axis.text.y = element_text(size = 12))
          
# save the plot
ggsave(p_r2, file="plot_r2.png", width = 20, height = 16, units = "cm")

# 2. % of full model as BFM in 1000 bootstrap samples
bfm_pct <- matrix(, nrow = 50, ncol = total_cond)
for (k in 1:total_cond){
  cond_num = k
  
  for (i in 1:50){
    pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")
    dat_bfm <- readRDS(pth)[[5]]
    bfm_pct[i, k] <- nrow(dat_bfm[dat_bfm$X1 == 31,])/1000
  }
}

# obtain mean, median, max, min percentage, 
bfm_pct_res <- matrix(, nrow = total_cond, ncol = 4 )

for (i in 1: total_cond){
  bfm_pct_res[i,1] <- mean(bfm_pct[,i])
  bfm_pct_res[i,2] <- median(bfm_pct[,i])
  bfm_pct_res[i,3] <- min(bfm_pct[,i])
  bfm_pct_res[i,4] <- max(bfm_pct[,i])
}

bfm_pct_res <- data.frame(bfm_pct_res)
colnames(bfm_pct_res) <- c("Mean", "Median", "Min", "Max")
write.csv(bfm_pct_res, "full_model_pct.csv", row.names = FALSE)

# 3. Summary of size of BFM
bfm_sz <- matrix(, nrow = 50*1000, ncol = 9)

for (i in 1:total_cond){
  cond_num <- i
  mod_bfm <- vector()
  for (j in 1:50){
    pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", j, ".RData")
    mod_bfm <- c(mod_bfm, readRDS(pth)[[5]]$X1)
  }
  bfm_sz[,i] <- mod_bfm
}

bfm_sz2 <- matrix(, nrow = 50*1000, ncol = 9)
for (i in 1:9){
  for (j in 1: nrow(bfm_sz)){
    if(bfm_sz[j,i] <=5){
      bfm_sz2[j,i] <-1
    }else if(bfm_sz[j,i] >=6 &bfm_sz[j,i] <=15){
      bfm_sz2[j,i] <-2
    }else if(bfm_sz[j,i] >=16 &bfm_sz[j,i] <=25){
      bfm_sz2[j,i] <-3
    }else if(bfm_sz[j,i] >=26 &bfm_sz[j,i] <=30){
      bfm_sz2[j,i] <-4
    }else{
      bfm_sz2[j,i] <-5
    }
  }
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


bfm_sz2 <- data.frame(bfm_sz2)
colnames(bfm_sz2) <- c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9")
res_bfm_sz <- data.frame(rbind(apply(bfm_sz2, 2, mean), apply(bfm_sz2, 2, median), apply(bfm_sz2, 2, getmode), apply(bfm_sz2, 2, sd)))
rownames(res_bfm_sz) <- c("Mean","Median", "Mode", "SD")
write.xlsx(res_bfm_sz, "bfm_size.xlsx", rowNames = TRUE, overwrite = TRUE)

# 4. Entropy of 9 conditions
entropy <- read.csv("entropy_all.csv")
entropy_dat <- data.frame(entropy = c(entropy[,1], entropy[,2], entropy[,3], entropy[,4],
                                      entropy[,5], entropy[,6], entropy[,7], entropy[,8],
                                      entropy[,9]), cond = rep(c("COND1", "COND2", "COND3",
                                                                 "COND4", "COND5", "COND6",
                                                                 "COND7", "COND8", "COND9"), each = 50))
# averaged entropy across 50 random samples of the same condition
entropy_res <- aggregate(entropy ~ cond, entropy_dat, mean)

p_entropy <- ggplot(entropy_dat, aes(x=factor(cond), y=entropy)) + geom_boxplot() +
             stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")

p_entropy <- p_entropy + xlab("Simulation Condition")+ylab('Entropy') + theme_classic() + 
             theme(axis.title.x=element_text(size=14, face="bold"))+
             theme(axis.title.y=element_text(size=14, face="bold"))+
             theme(axis.text.x = element_text(size = 12))+
             theme(axis.text.y = element_text(size = 12))
  
# save the plot
ggsave(p_entropy, file="plot_entropy.png", width = 20, height = 16, units = "cm")

# II. Main analysis results
# write a function to categorize performance of two inference methods
# BFM better => 1, Full DA better => -1, equally good => 0
diff_binary <- function(vec){
  diff_binary <- vector()
  for (i in 1:length(vec)){
    
    if (is.na(vec[i])){
      diff_binary[i] <- NA
    }else if(vec[i] > 0){
      diff_binary[i] <- 1
    }else if(vec[i] < 0){
      diff_binary[i] <- (-1)
    }else{
      diff_binary[i] <- 0
    }
  }
  return(diff_binary)
}

##################### Analysis based on GD ##########################
# 1. Tau between pop RGD & rank of averaged BGD => summary table
v_tau1 <- vector()
v_tau2 <- vector()
v_r1 <- vector()
v_r2 <- vector()

for (i in 3:total_cond){ # start from cond3 because no tau & r data in the first 2 conditions 
  gd_bs <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/gd_results_final.xlsx", sheet = paste0("cond", i), colNames = TRUE)
  
  tau_full <- gd_bs$tau_full
  tau_bfm <- gd_bs$tau_bfm
  r_full <- gd_bs$cor_full
  r_bfm <- gd_bs$cor_bfm
  
  v_tau1 <- c(v_tau1, tau_full)
  v_tau2 <- c(v_tau2, tau_bfm)
  v_r1 <- c(v_r1, r_full)
  v_r2 <- c(v_r2, r_bfm)
}

tau_r_bs <- data.frame (tau_full = v_tau1, tau_bfm = v_tau2, r_full = v_r1, r_bfm = v_r2)
# No tau for first two conditions because the variation of PGD is zero
tau_r_bs$cond <- rep (c("COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50)
tau_r_bs$tau_diff <- tau_r_bs$tau_bfm - tau_r_bs$tau_full
tau_r_bs$r_diff <- tau_r_bs$r_bfm - tau_r_bs$r_full
tau_r_bs$bfm_better_tau <- diff_binary(tau_r_bs$tau_diff)
tau_r_bs$bfm_better_r <- diff_binary(tau_r_bs$r_diff)

# tau
table(tau_r_bs$cond, tau_r_bs$bfm_better_tau)/50 # include in the results
nrow(tau_r_bs[tau_r_bs$bfm_better_tau == 0,])/nrow(tau_r_bs) # 96%
subset(tau_r_bs, bfm_better_tau != 0)

# summary results of tau
tau_bs_res <- aggregate(tau_full ~ cond, tau_r_bs, mean)
tau_bs_res$tau_bfm <- aggregate(tau_bfm ~ cond, tau_r_bs, mean)$tau_bfm
tau_bs_res$sd_full <- aggregate(tau_full ~ cond, tau_r_bs, sd)$tau_full
tau_bs_res$sd_bfm <- aggregate(tau_bfm ~ cond, tau_r_bs, sd)$tau_bfm

write.csv(tau_bs_res, "tau_res.csv", row.names = FALSE)

# graphic presentation of tau
tau_bs_long<- data.frame(tau = c(tau_r_bs$tau_full, tau_r_bs$tau_bfm), 
                        cond = rep(tau_r_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(tau_r_bs)))

p_tau <- ggplot(tau_bs_long, aes(x=factor(cond), y=tau, fill = Method)) + geom_boxplot() 
p_tau <- p_tau + xlab("Simulation Condition")+ylab("Kendall's tau") + theme_classic() + 
  theme(legend.position=c(0.91, 0.12)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# save the plot
ggsave(p_tau, file="plot_tau.png", width = 20, height = 16, units = "cm")

# 2. Tau between pop RGD & rank of BGD 
#.  => summary table 
#.  => percentage tau(bfm) > tau(full); tau(bfm) = tau(full); tau(bfm) < tau(full)
tau_all1 <- vector()
tau_all2 <- vector()
t_diff <- vector()

for (i in 3:total_cond){ # start from cond3 because no tau & r data in the first 2 conditions 
  gd_micro <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/gd_results_micro.xlsx", sheet = paste0("cond", i), colNames = TRUE)
  
  tau_full <- gd_micro$tau_full
  tau_bfm <- gd_micro$tau_bfm
  tau_diff <- gd_micro$tau_diff
  
  tau_all1 <- c(tau_all1, tau_full)
  tau_all2 <- c(tau_all2, tau_bfm)
  t_diff <- c(t_diff, tau_diff)
}

tau_all <- data.frame (tau_full = tau_all1, tau_bfm = tau_all2, tau_diff = t_diff)
# No tau for first two conditions because the variation of PGD is zero
tau_all$cond <- rep (c("COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50*1000)
tau_all$bfm_better_tau <- diff_binary(tau_all$tau_diff)
table(tau_all$cond, tau_all$bfm_better_tau)

complete_sz <- vector() # 47223, 46773, 49999, 50000, 50000, 50000, 50000
for (i in 3:9){
  d <-subset(tau_all, cond == paste0("COND",i))
  nrow(d)
  d <- d[complete.cases(d),]
  complete_sz[i] <- nrow(d)
}

# summary results of tau
tau_all_res <- aggregate(tau_full ~ cond, tau_all, mean)
tau_all_res$tau_bfm <- aggregate(tau_bfm ~ cond, tau_all, mean)$tau_bfm
tau_all_res$sd_full <- aggregate(tau_full ~ cond, tau_all, sd)$tau_full
tau_all_res$sd_bfm <- aggregate(tau_bfm ~ cond, tau_all, sd)$tau_bfm
tau_all_res$tau_diff <-aggregate(tau_diff ~ cond, tau_all, mean)$tau_diff
tau_all_res$sd_diff <-aggregate(tau_diff ~ cond, tau_all, sd)$tau_diff
# save the summary table
write.csv(tau_all_res, "tau_micro_res.csv", row.names = FALSE)

# 3.RMSE between pop GD & the averaged BGD => summary table
v_rmse1 <- vector()
v_rmse2 <- vector()

for (i in 1:total_cond){ 
  gd_bs <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/gd_results_final.xlsx", sheet = paste0("cond", i), colNames = TRUE)
  
  rmse_full <- gd_bs$rmse_full
  rmse_bfm <- gd_bs$rmse_bfm
  v_rmse1 <- c(v_rmse1, rmse_full)
  v_rmse2 <- c(v_rmse2, rmse_bfm)
  
}

rmse_bs <- data.frame (rmse_full = v_rmse1, rmse_bfm = v_rmse2)
rmse_bs$cond <- rep (c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50)
rmse_bs$diff <- rmse_bs$rmse_full - rmse_bs$rmse_bfm
rmse_bs$bfm_better_rmse <- diff_binary(rmse_bs$diff)

table(rmse_bs$cond, rmse_bs$bfm_better_rmse)
aggregate(diff ~ cond, rmse_bs, mean)

# summary results of RMSE (random sample)
rmse_bs_res <- aggregate(rmse_full ~ cond, rmse_bs, mean)
rmse_bs_res$rmse_bfm <- aggregate(rmse_bfm ~ cond, rmse_bs, mean)$rmse_bfm
rmse_bs_res$sd_full <- aggregate(rmse_full ~ cond, rmse_bs, sd)$rmse_full
rmse_bs_res$sd_bfm <- aggregate(rmse_bfm ~ cond, rmse_bs, sd)$rmse_bfm
rmse_bs_res$rmse_diff <- aggregate(diff ~ cond, rmse_bs, mean)$diff
rmse_bs_res$sd_diff <- aggregate(diff ~ cond, rmse_bs, sd)$diff


# save the summary table
write.csv(rmse_bs_res, "rmse_res.csv", row.names = FALSE)

# graphic presentation of RMSE
rmse_bs_long<- data.frame(rmse = c(rmse_bs$rmse_full, rmse_bs$rmse_bfm), 
                          cond = rep(rmse_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(rmse_bs)))

p_rmse <- ggplot(rmse_bs_long, aes(x=factor(cond), y=rmse, fill = Method)) + geom_boxplot() 
p_rmse <- p_rmse + xlab("Simulation Condition")+ylab("RMSE") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# save the plot
ggsave(p_rmse, file="plot_rmse.png", width = 20, height = 16, units = "cm")

# 4. RMSE between pop GD & BGD 
#.  => summary table 
#.  => percentage 
rmse_all1 <- vector()
rmse_all2 <- vector()
diff <- vector()

for (i in 1:total_cond){ 
  rmse_micro <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/rmse_results_micro.xlsx", sheet = paste0("cond", i), colNames = TRUE)
  
  rmse_full <- rmse_micro$rmse_full
  rmse_bfm <- rmse_micro$rmse_bfm
  rmse_diff <- rmse_micro$rmse_diff
  
  rmse_all1 <- c(rmse_all1, rmse_full)
  rmse_all2 <- c(rmse_all2, rmse_bfm)
  diff <- c(diff, rmse_diff)
  print(i)
}


length(rmse_all1)
length(rmse_all2)
length(diff)


rmse_all <- data.frame (rmse_full = rmse_all1, rmse_bfm = rmse_all2, rmse_diff = diff)
# No rmse for first two conditions because the variation of PGD is zero
rmse_all$cond <- rep (c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50*1000)
rmse_all$bfm_better_rmse <- diff_binary(rmse_all$rmse_diff)
table(rmse_all$cond, rmse_all$bfm_better_rmse)

complete_sz <- vector() # 50000, 50000, 47223, 46773, 49999, 50000, 50000, 50000, 50000, 
for (i in 1:9){
  d <-subset(rmse_all, cond == paste0("COND",i))
  nrow(d)
  d <- d[complete.cases(d),]
  complete_sz[i] <- nrow(d)
}

# summary results of rmse
rmse_all_res <- aggregate(rmse_full ~ cond, rmse_all, mean)
rmse_all_res$rmse_bfm <- aggregate(rmse_bfm ~ cond, rmse_all, mean)$rmse_bfm
rmse_all_res$sd_full <- aggregate(rmse_full ~ cond, rmse_all, sd)$rmse_full
rmse_all_res$sd_bfm <- aggregate(rmse_bfm ~ cond, rmse_all, sd)$rmse_bfm
rmse_all_res$rmse_diff <-aggregate(rmse_diff ~ cond, rmse_all, mean)$rmse_diff
rmse_all_res$sd_diff <-aggregate(rmse_diff ~ cond, rmse_all, sd)$rmse_diff
# save the summary table
write.csv(rmse_all_res, "rmse_micro_res.csv", row.names = FALSE)

##################### Analysis based on DM ##########################
# 1. DD between pop DM & averaged BDM => summary table
dd_com1 <- vector()
dd_com2 <- vector()  
dd_con1 <- vector()
dd_con2 <- vector()
dd_gen1 <- vector()
dd_gen2 <- vector()

for (i in 1:total_cond){ 
  dm_bs <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/dm_results_final.xlsx", sheet = paste0("cond", i), colNames = TRUE)
  
  dd_com_full <- dm_bs$com_full
  dd_com_bfm <- dm_bs$com_bfm
  dd_con_full <- dm_bs$con_full
  dd_con_bfm <- dm_bs$con_bfm
  dd_gen_full <- dm_bs$gen_full
  dd_gen_bfm <- dm_bs$gen_bfm
  
  dd_com1 <- c(dd_com1, dd_com_full)
  dd_com2 <- c(dd_com2, dd_com_bfm)
  dd_con1 <- c(dd_con1, dd_con_full)
  dd_con2 <- c(dd_con2, dd_con_bfm)
  dd_gen1 <- c(dd_gen1, dd_gen_full)
  dd_gen2 <- c(dd_gen2, dd_gen_bfm)
  
}

dd_bs <- data.frame (com_full = dd_com1, com_bfm = dd_com2, con_full = dd_con1, con_bfm = dd_con2, gen_full = dd_gen1, gen_bfm = dd_gen2)
dd_bs$cond <- rep (c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50)
dd_bs$com_diff <- dd_bs$com_full - dd_bs$com_bfm
dd_bs$con_diff <- dd_bs$con_full - dd_bs$con_bfm
dd_bs$gen_diff <- dd_bs$gen_full - dd_bs$gen_bfm
dd_bs$bfm_better_com <- diff_binary(dd_bs$com_diff)
dd_bs$bfm_better_con <- diff_binary(dd_bs$con_diff)
dd_bs$bfm_better_gen <- diff_binary(dd_bs$gen_diff)

# Percentage of BFM equal or better than Full
table(dd_bs$cond, dd_bs$bfm_better_com)
table(dd_bs$cond, dd_bs$bfm_better_con)
table(dd_bs$cond, dd_bs$bfm_better_gen)

# summary of results
dd_bs_res <- aggregate(com_full ~ cond, dd_bs, mean)
dd_bs_res$com_bfm <- aggregate(com_bfm ~ cond, dd_bs, mean)$com_bfm
dd_bs_res$SD_full1 <- aggregate(com_full ~ cond, dd_bs, sd)$com_full
dd_bs_res$SD_bfm1 <- aggregate(com_bfm ~ cond, dd_bs, sd)$com_bfm
dd_bs_res$diff_com <- aggregate (com_diff ~ cond, dd_bs, mean)$com_diff
dd_bs_res$sd_diff_com <- aggregate (com_diff ~ cond, dd_bs, sd)$com_diff

dd_bs_res$con_full <- aggregate(con_full ~ cond, dd_bs, mean)$con_full
dd_bs_res$con_bfm <- aggregate(con_bfm ~ cond, dd_bs, mean)$con_bfm
dd_bs_res$SD_full2 <- aggregate(con_full ~ cond, dd_bs, sd)$con_full
dd_bs_res$SD_bfm2 <- aggregate(con_bfm ~ cond, dd_bs, sd)$con_bfm
dd_bs_res$diff_con <- aggregate (con_diff ~ cond, dd_bs, mean)$con_diff
dd_bs_res$sd_diff_con <- aggregate (con_diff ~ cond, dd_bs, sd)$con_diff


dd_bs_res$gen_full <- aggregate(gen_full ~ cond, dd_bs, mean)$gen_full
dd_bs_res$gen_bfm <- aggregate(gen_bfm ~ cond, dd_bs, mean)$gen_bfm
dd_bs_res$SD_full3 <- aggregate(gen_full ~ cond, dd_bs, sd)$gen_full
dd_bs_res$SD_bfm3 <- aggregate(gen_bfm ~ cond, dd_bs, sd)$gen_bfm
dd_bs_res$diff_gen <- aggregate (gen_diff ~ cond, dd_bs, mean)$gen_diff
dd_bs_res$sd_diff_gen <- aggregate (gen_diff ~ cond, dd_bs, sd)$gen_diff


# save the summary table
write.csv(dd_bs_res, "dd_res.csv", row.names = FALSE)

# graphic presentation of DD for complete dominance
dd_bs_long1<- data.frame(dd = c(dd_bs$com_full, dd_bs$com_bfm), 
                         cond = rep(dd_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(dd_bs)))

p_dd_com <- ggplot(dd_bs_long1, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_dd_com <- p_dd_com + xlab("Simulation Condition")+ylab("DD for Complete Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# graphic presentation of DD for conditional dominance
dd_bs_long2<- data.frame(dd = c(dd_bs$con_full, dd_bs$con_bfm), 
                         cond = rep(dd_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(dd_bs)))

p_dd_con <- ggplot(dd_bs_long2, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_dd_con <- p_dd_con + xlab("Simulation Condition")+ylab("DD for Conditional Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# graphic presentation of DD for general dominance
dd_bs_long3<- data.frame(dd = c(dd_bs$gen_full, dd_bs$gen_bfm), 
                         cond = rep(dd_bs$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(dd_bs)))

p_dd_gen <- ggplot(dd_bs_long3, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_dd_gen <- p_dd_gen + xlab("Simulation Condition")+ylab("DD for General Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# save the plot
ggsave(p_dd_com, file="plot_dd_com.png", width = 20, height = 16, units = "cm")
ggsave(p_dd_con, file="plot_dd_con.png", width = 20, height = 16, units = "cm")
ggsave(p_dd_gen, file="plot_dd_gen.png", width = 20, height = 16, units = "cm")

# 2. DD between pop DM & individual BDM => summary table
#.  => summary table 
#.  => percentage DD(bfm) > DD(full); DD(bfm) = DD(full); DD(bfm) < DD(full)

DD_COM1 <- vector()
DD_COM2 <- vector()  
DD_CON1 <- vector()
DD_CON2 <- vector()
DD_GEN1 <- vector()
DD_GEN2 <- vector()

for (i in 1:total_cond){ 
  dm_micro <- read.xlsx("/Users/ying/Desktop/Dissertation writing/Analysis results/result_summary/dm_results_micro.xlsx", sheet = paste0("cond", i), colNames = TRUE)

  DD_COM1 <- c(DD_COM1, dm_micro$DD_COM_FULL)
  DD_COM2 <- c(DD_COM2, dm_micro$DD_COM_BFM)
  DD_CON1 <- c(DD_CON1, dm_micro$DD_CON_FULL)
  DD_CON2 <- c(DD_CON2, dm_micro$DD_CON_BFM)
  DD_GEN1 <- c(DD_GEN1, dm_micro$DD_GEN_FULL)
  DD_GEN2 <- c(DD_GEN2, dm_micro$DD_GEN_BFM)
               
}

DD <- data.frame (COM_FULL = DD_COM1, COM_BFM = DD_COM2, CON_FULL = DD_CON1, CON_BFM = DD_CON2, GEN_FULL = DD_GEN1, GEN_BFM = DD_GEN2)
DD$cond <- rep (c("COND1", "COND2", "COND3", "COND4", "COND5", "COND6", "COND7", "COND8", "COND9"), each = 50*1000)
DD$COM_DIFF <- DD$COM_FULL - DD$COM_BFM
DD$CON_DIFF <- DD$CON_FULL - DD$CON_BFM
DD$GEN_DIFF <- DD$GEN_FULL - DD$GEN_BFM
DD$BFM_BETTER_COM <- diff_binary(DD$COM_DIFF)
DD$BFM_BETTER_CON <- diff_binary(DD$CON_DIFF)
DD$BFM_BETTER_GEN <- diff_binary(DD$GEN_DIFF)


# Percentage of BFM equal or better than Full
tb1 <- table(DD$cond, DD$BFM_BETTER_COM)
tb2 <- table(DD$cond, DD$BFM_BETTER_CON)
tb3 <- table(DD$cond, DD$BFM_BETTER_GEN)
tb_lst <- list (com = tb1, con = tb2, gen = tb3)
write.xlsx (tb_lst, "tb_lst.xlsx")

# summary of results
DD_RES <- aggregate(COM_FULL ~ cond, DD, mean)
DD_RES$COM_BFM <- aggregate(COM_BFM ~ cond, DD, mean)$COM_BFM
DD_RES$SD_FULL1 <- aggregate(COM_FULL ~ cond, DD, sd)$COM_FULL
DD_RES$SD_BFM1 <- aggregate(COM_BFM ~ cond, DD, sd)$COM_BFM
DD_RES$DIFF_COM <- aggregate (COM_DIFF ~ cond, DD, mean)$COM_DIFF
DD_RES$SD_DIFF_COM <- aggregate (COM_DIFF ~ cond, DD, sd)$COM_DIFF

DD_RES$CON_FULL <- aggregate(CON_FULL ~ cond, DD, mean)$CON_FULL
DD_RES$CON_BFM <- aggregate(CON_BFM ~ cond, DD, mean)$CON_BFM
DD_RES$SD_FULL2 <- aggregate(CON_FULL ~ cond, DD, sd)$CON_FULL
DD_RES$SD_BFM2 <- aggregate(CON_BFM ~ cond, DD, sd)$CON_BFM
DD_RES$DIFF_CON <- aggregate (CON_DIFF ~ cond, DD, mean)$CON_DIFF
DD_RES$SD_DIFF_CON <- aggregate (CON_DIFF ~ cond, DD, sd)$CON_DIFF

DD_RES$GEN_FULL <- aggregate(GEN_FULL ~ cond, DD, mean)$GEN_FULL
DD_RES$GEN_BFM <- aggregate(GEN_BFM ~ cond, DD, mean)$GEN_BFM
DD_RES$SD_FULL3 <- aggregate(GEN_FULL ~ cond, DD, sd)$GEN_FULL
DD_RES$SD_BFM3 <- aggregate(GEN_BFM ~ cond, DD, sd)$GEN_BFM
DD_RES$DIFF_GEN <- aggregate (GEN_DIFF ~ cond, DD, mean)$GEN_DIFF
DD_RES$SD_DIFF_GEN <- aggregate (GEN_DIFF ~ cond, DD, sd)$GEN_DIFF

# save the summary table
write.csv(DD_RES, "dd_micro_res.csv", row.names = FALSE)

# graphic presentation of DD for complete dominance
DD_long1<- data.frame(dd = c(DD$COM_FULL, DD$COM_BFM), 
                         cond = rep(DD$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(DD)))

p_DD_com <- ggplot(DD_long1, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_DD_com <- p_DD_com + xlab("Simulation Condition")+ylab("DD for Complete Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# graphic presentation of DD for conditional dominance
DD_long2<- data.frame(dd = c(DD$CON_FULL, DD$CON_BFM), 
                      cond = rep(DD$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(DD)))

p_DD_con <- ggplot(DD_long2, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_DD_con <- p_DD_con + xlab("Simulation Condition")+ylab("DD for Conditional Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# graphic presentation of DD for general dominance
DD_long3<- data.frame(dd = c(DD$GEN_FULL, DD$GEN_BFM), 
                      cond = rep(DD$cond, time = 2), Method = rep (c("Full DA", "BFM DA"), each = nrow(DD)))

p_DD_gen <- ggplot(DD_long3, aes(x=factor(cond), y=dd, fill = Method)) + geom_boxplot() 
p_DD_gen <- p_DD_gen + xlab("Simulation Condition")+ylab("DD for General Dominance") + theme_classic() + 
  theme(legend.position=c(0.12, 0.9)) + theme(legend.background=element_rect(fill="white", colour="black"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

# save the plot
ggsave(p_DD_com, file="plot_dd_micro_com.png", width = 20, height = 16, units = "cm")
ggsave(p_DD_con, file="plot_dd_micro_con.png", width = 20, height = 16, units = "cm")
ggsave(p_DD_gen, file="plot_dd_micro_gen.png", width = 20, height = 16, units = "cm")











      