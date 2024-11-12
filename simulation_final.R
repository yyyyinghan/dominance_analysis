############################################################################
############ Obtain DA results from simulations (8 conditions) #############
############################################################################

#################### This is for one condition ! #####################
#### REMINDER: Need to run 8 times using 8 different condition numbers
cond_num <- 6 # specify condition number
######################################################################

###### Preparation ####
setwd("/Users/ying/Desktop/Dissertation writing")
library(openxlsx)
library(olsrr)
library(MASS)
library(dominanceanalysis)
###### specify number of bootstrap samples ###
b_sz = 1000
##### create a list of all subset models #####
sub_mod <- data.frame(mod_id = 1:31, 
                      mod = c("Y~X1", "Y~X2", "Y~X3", "Y~X4", "Y~X5", "Y~X1+X2", "Y~X1+X3", "Y~X1+X4", "Y~X1+X5",
                              "Y~X2+X3", "Y~X2+X4", "Y~X2+X5", "Y~X3+X4", "Y~X3+X5", "Y~X4+X5", "Y~X1+X2+X3",
                              "Y~X1+X2+X4", "Y~X1+X2+X5", "Y~X1+X3+X4", "Y~X1+X3+X5", "Y~X1+X4+X5", "Y~X2+X3+X4",
                              "Y~X2+X3+X5", "Y~X2+X4+X5", "Y~X3+X4+X5", "Y~X1+X2+X3+X4","Y~X1+X2+X3+X5", 
                              "Y~X1+X2+X4+X5", "Y~X1+X3+X4+X5","Y~X2+X3+X4+X5", "Y~X1+X2+X3+X4+X5"))

start <- Sys.time()
################################ Simulation code ####################################
##### I. Draw 50 random samples from multivariate normal distribution & save the data
rd_samples <- list() 
sample_size <- 250                                      
sample_meanvector <- rep(0, times = 6)
sample_covariance_matrix <- read.xlsx("corr_matrix_all.xlsx", sheet = paste0("cond", cond_num), colNames = FALSE) 

for (i in 1:50){
  sample_distribution <- mvrnorm(n = sample_size,
                                 mu = sample_meanvector, 
                                 Sigma = sample_covariance_matrix)
  dat <- data.frame(sample_distribution)
  colnames(dat) <- c("Y", "X1", "X2", "X3", "X4", "X5")
  rd_samples[[i]] <- dat
}

# save 50 random samples in a list for each condition
saveRDS(rd_samples, file=paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/rdsample_cond", cond_num, ".RData")) 

##### II. Generate 1000 bootstrap samples
for (i in 1:50){
  bt_ind<-sample(1:250, size=250*b_sz, replace = TRUE) 
  bt_sample <-rd_samples[[i]][bt_ind,]
  bt_sample$sample_n<-rep(1:b_sz, each = 250) 
  
  # III. Fit regression model & run dominance analysis 
  # Full model DA (bootstrapping inference)
  mod_lst1<-matrix (, nrow = b_sz, ncol = 2) # store complete lm result
  da_lst1<-list() # store complete DA result
  bfm_lst<-list() # used in the BFM DA
  
  for(j in 1:b_sz){
    dd <- subset(bt_sample, sample_n == j)
    lm.1<-lm(Y~X1+X2+X3+X4+X5, dd) 
    mod_lst1[j,1]<-summary(lm.1)[[8]]
    mod_lst1[j,2]<-summary(lm.1)[[9]]
    da_lst1[[j]]<-list(GD = averageContribution(dominanceAnalysis(lm.1)),
                       DM1 = dominanceMatrix(dominanceAnalysis(lm.1), type = "complete"),
                       DM2 = dominanceMatrix(dominanceAnalysis(lm.1), type = "conditional"),
                       DM3 = dominanceMatrix(dominanceAnalysis(lm.1), type = "general"))
    bfm_lst[[j]]<-data.frame(ols_step_all_possible(lm.1))
    print(j)
  }
  
  # BFM DA (bootstrapping inference)
  # Identify BFM of each bootstrap sample
  best_model<-matrix(, nrow = b_sz, ncol = 2)
  for (k in 1:b_sz){
    result <- bfm_lst[[k]]
    result <- result[order(result$adjr, decreasing = TRUE),]
    ind <- rownames(result[1,])
    best_model[k,1] <- ind
    best_model[k,2] <- sub_mod[ind, 2]
  }
  best_model <- data.frame(best_model)
  best_model$X1 <- as.numeric (best_model$X1)
  
  mod_lst2<-matrix (, nrow = b_sz, ncol = 2) # store complete lm result
  da_lst2<-list() # store complete DA result
  for (l in 1:b_sz){
    dd <- subset(bt_sample, sample_n == l)
    lm.best<-lm(best_model[l,2], dd)
    mod_lst2[l,1]<-summary(lm.best)[[8]]
    mod_lst2[l,2]<-summary(lm.best)[[9]]
    if (best_model[l,]$X1 <= 5){ # only 1 predictor in BFM
      da_lst2[[l]] <- NA
    }else{
      da_lst2[[l]]<-list(GD = averageContribution(dominanceAnalysis(lm.best)),
                         DM1 = dominanceMatrix(dominanceAnalysis(lm.best), type = "complete"),
                         DM2 = dominanceMatrix(dominanceAnalysis(lm.best), type = "conditional"),
                         DM3 = dominanceMatrix(dominanceAnalysis(lm.best), type = "general"))
    }
    print(l)
  }
  
  final_result <- list(DA_FULL = da_lst1, DA_BFM = da_lst2, LM_FULL = mod_lst1, 
                       LM_BFM = mod_lst2, BEST_MOD = best_model)

  saveRDS(final_result, file=paste0("/Users/ying/Desktop/Dissertation writing/Simulation results/cond", cond_num, "/cond", cond_num,  "_", i, ".RData")) 
}

end <- Sys.time()
end - start



