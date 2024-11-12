#########################################################################################################
############################## DA on 50 random samples of 9 conditions ###############################
#########################################################################################################
total_cond <- 9 # specify the total number of conditions
######################################################################
##### create a list of all subset models #####
sub_mod <- data.frame(mod_id = 1:31, 
                      mod = c("Y~X1", "Y~X2", "Y~X3", "Y~X4", "Y~X5", "Y~X1+X2", "Y~X1+X3", "Y~X1+X4", "Y~X1+X5",
                              "Y~X2+X3", "Y~X2+X4", "Y~X2+X5", "Y~X3+X4", "Y~X3+X5", "Y~X4+X5", "Y~X1+X2+X3",
                              "Y~X1+X2+X4", "Y~X1+X2+X5", "Y~X1+X3+X4", "Y~X1+X3+X5", "Y~X1+X4+X5", "Y~X2+X3+X4",
                              "Y~X2+X3+X5", "Y~X2+X4+X5", "Y~X3+X4+X5", "Y~X1+X2+X3+X4","Y~X1+X2+X3+X5", 
                              "Y~X1+X2+X4+X5", "Y~X1+X3+X4+X5","Y~X2+X3+X4+X5", "Y~X1+X2+X3+X4+X5"))


# Conduct DA on 50 random samples & gather all DA results
r_lst <- list()
#best_model_lst <- list()

for (i in 1:total_cond){
  pth <- paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_cond", i, ".RData")
  dat <- readRDS(pth)
  
  r_tb <- matrix(, nrow = 50, ncol = 2)
  best_model <- matrix(, nrow = 50, ncol = 2)
  da_lst <- list()
  
  for (j in 1:50){
    dd <- dat[[j]]
    lm.1<-lm(Y~X1+X2+X3+X4+X5, dd) 
    r_tb[j,1]<-summary(lm.1)[[8]] # r-squared
    r_tb[j,2]<-summary(lm.1)[[9]] # adjusted r-squared
    # GD & DM of three types
    da_lst[[j]]<-list(GD = averageContribution(dominanceAnalysis(lm.1)),
                       DM1 = dominanceMatrix(dominanceAnalysis(lm.1), type = "complete"),
                       DM2 = dominanceMatrix(dominanceAnalysis(lm.1), type = "conditional"),
                       DM3 = dominanceMatrix(dominanceAnalysis(lm.1), type = "general"))
    
    #bfm<-data.frame(ols_step_all_possible(lm.1))
    #bfm<- bfm[order(bfm$adjr, decreasing = TRUE),]
    #ind <- rownames(bfm[1,])
    #best_model[j,1] <- as.numeric(ind)
    #best_model[j,2] <- sub_mod[ind, 2]
    print(j)
  }
  
  # save results
  saveRDS(da_lst, file = paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_da_cond", i, ".RData"))
  r_lst[[i]] <- r_tb
  
}

library(openxlsx)
write.xlsx(r_lst, "/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_r2.xlsx")




