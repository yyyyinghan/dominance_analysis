##############################################################################
######## Conduct DA on population for original Conditions 1-8  ###############
# Reminder: Dijs for equal dominance & diagonal elements are set up as 0.5. 
#           DijS for unestablished dominance are set up as NA.
#############################################################################
setwd("/Users/ying/Desktop/Dissertation writing")
library(openxlsx)
total_cond <- 9 # number of simulation conditions
#############################################################################

# Function that creates dummy variables used to identify all subset models
subset_dum <- function(p){
  
  p_ind <- 1:p
  dum <- matrix(0, nrow = 0, ncol = p)
  
  for (u in 1:p){
    m <- combn(p_ind, u)
    nn <- matrix(0, nrow =ncol(m), ncol = p)
    for (v in 1:ncol(m)){
      lst <- m[,v]
      nn[v,lst] <- 1
    }
    dum <- rbind(dum, nn)
  }
  return (dum) 
}

#subset_dum(5)

# Function 
find_identical<-function(vct, dum){
  for (w in 1:nrow(dum)){
    if(identical(dum[w,], vct)){
      break
    }
  }
  return(w)
}


# 2. Input: p, dat (correlation matrix including RXX and RXY)
GD <- function(p, dat){
  
  dum = subset_dum(p)
  r2 <- vector()
  
  for (k in 1:nrow(dum)){
    v = dum[k,]
    ind = which(v == 1)
    
    if (sum(v) == 1){
      r2[k] <- (dat[ind, 1])^2
    }else{
      rxy <- dat[,1][ind]
      rxx <- dat[,2:ncol(dat)][ind, ind]
      r2[k] <- t(rxy)%*%solve(rxx)%*%rxy
    }
  }
  
  add_r2 <- matrix(, nrow = nrow(dum), ncol = p)
  
  for (i in 1:nrow(dum)){
    v2 = dum[i,]
    ind2 = which( v2 == 1)
    
    for (j in 1:p){
      if(j %in% ind2){
        add_r2[i, j] <- NA
      }else{
        vec1 <- dum[i,]
        vec1[j] <- 1
        indx <- find_identical(vec1, dum)
        add_r2[i, j] <- r2[indx]-r2[i]
      }
    }
  }
  
  dum <- rbind (rep(0,p), dum)
  add_r2 <- rbind (r2[1:p], add_r2)
  result <- data.frame(cbind(add_r2, dum))
  result$K <- rowSums(dum)
  
  avg <- matrix(, nrow = p, ncol = p)
  for (h in 1:p){
    avg[,h] <- aggregate(result[,h] ~ K, result, mean)[,2]
  }
  gd <- colMeans(avg) # general dominance
  
  # Obtain complete dominance matrix
  dm1 <- matrix (, nrow = p, ncol = p)
  for (m in 1:p){
    for (n in 1:p){
      dd <- subset(result, result[,m+p]==0 & result[,n+p]==0)
      diff <- dd[,m]-dd[,n]
      
      if(m == n){
        dm1[m,n] <- 0.5
      }else if(length(diff[diff > 0]) == length(diff)){ # Xm D Xn
        dm1[m,n] <- 1
      }else if(length(diff[diff < 0]) == length(diff)){ # Xn D Xm
        dm1[m,n] <- 0
      }else if (length(diff[diff == 0]) == length(diff)){
        dm1[m,n] <-0.5
      }else{
        dm1[m,n] <- dm1[m,n]
      }
    }
  }
  
  
  
  # Obtain conditional dominance matrix
  dm2 <- matrix (, nrow = p, ncol = p)
  for (c in 1:p){
    for (d in 1:p){
      avg_diff <- avg[,c] - avg[,d]
      
      if(c == d){
        dm2[c,d] <- 0.5
      }else if(length(avg_diff[avg_diff > 0]) == length(avg_diff)){
        dm2[c,d] <- 1
      }else if(length(avg_diff[avg_diff < 0]) == length(avg_diff)){
        dm2[c,d] <- 0
      }else if(length(avg_diff[avg_diff == 0]) == length(avg_diff)){
        dm2[c,d] <- 0.5
      }else{
        dm2[c,d] <- dm2[c,d]
      }
    }
  }
  
  # Obtain general dominance matrix
  dm3 <- matrix (, nrow = p, ncol = p)
  for (s in 1:p){
    for (t in 1:p){
      if (s == t){
        dm3[s,t] <- 0.5
      }else if(gd[s] > gd[t]){
        dm3[s,t] <- 1
      }else if(gd[s] < gd[t]){
        dm3[s,t] <- 0
      }else if(gd[s] == gd[t]){
        dm3[s,t] <- 0.5
      }
    }
  }
  
  return(list(complete_dm = dm1, conditional_dm = dm2, 
              general_dm = dm3, general_dominance = gd))
}

# Extract the DA results
result1 <-list()
result2 <-list()
result3 <-list()
result_gd <- matrix(, nrow = total_cond, ncol = 5)


for (i in 1:total_cond){
  sim <- read.xlsx("corr_matrix_all.xlsx", sheet = i, colNames = FALSE)
  sim <- sim[2:6,]
  result_all <- GD(5, sim)
  result1[[i]] <- result_all$complete_dm
  result2[[i]] <- result_all$conditional_dm
  result3[[i]] <- result_all$general_dm
  result_gd[i,] <-result_all$general_dominance
}

result_gd <- data.frame(result_gd)

# Save pop DA results
dat_list1 <-list("sheet1" = result1[[1]], "sheet2" = result1[[2]], 
                 "sheet3" = result1[[3]], "sheet4" = result1[[4]],
                 "sheet5" = result1[[5]], "sheet6" = result1[[6]],
                 "sheet7" = result1[[7]], "sheet8" = result1[[8]],
                 "sheet9" = result1[[9]])


dat_list2 <-list("sheet1" = result2[[1]], "sheet2" = result2[[2]], 
                 "sheet3" = result2[[3]], "sheet4" = result2[[4]],
                 "sheet5" = result2[[5]], "sheet6" = result2[[6]],
                 "sheet7" = result2[[7]], "sheet8" = result2[[8]],
                 "sheet9" = result2[[9]])


dat_list3 <-list("sheet1" = result3[[1]], "sheet2" = result3[[2]], 
                 "sheet3" = result3[[3]], "sheet4" = result3[[4]],
                 "sheet5" = result3[[5]], "sheet6" = result3[[6]],
                 "sheet7" = result3[[7]], "sheet8" = result3[[8]],
                 "sheet9" = result3[[9]])


write.xlsx(dat_list1, "/Users/ying/Desktop/Dissertation writing/DM_update/pop_da_comp_update.xlsx", rowNames = FALSE, overwrite = TRUE) 
write.xlsx(dat_list2, "/Users/ying/Desktop/Dissertation writing/DM_update/pop_da_cond_update.xlsx", rowNames = FALSE, overwrite = TRUE) 
write.xlsx(dat_list3, "/Users/ying/Desktop/Dissertation writing/DM_update/pop_da_gen_update.xlsx", rowNames = FALSE, overwrite = TRUE) 
write.xlsx(result_gd, "/Users/ying/Desktop/Dissertation writing/DM_update/pop_GD.xlsx", rowNames = FALSE, overwrite = TRUE) 

