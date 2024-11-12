############# Analysis for finding appropriate k (n = kp) ###############
############# Appendix A ###########

# Conduct simulation to obtain adequate k (n = kp) 
# Relative bias = (R2 - Rho2)/Rho2 <= 0.05
# Rho2 can be obtained via Claudy approximation of Olkin-Pratt estimator
# When p = 5 => Try R2 (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) and k (30, 35, 40, 45, 50, 55, 60, 65)
# When p = 7 => Try R2 (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) and k (15, 20, 25, 30)

R2_set <- rep (10:90, times =1)*0.01
p = 5
k <- c(30, 35, 40, 45, 50, 55, 60, 65)
result <- matrix ( , ncol = 5, nrow = 0)

for (i in 1:length(R2_set)){
  R2 = R2_set[i]
  
  for (j in 1:8){
    n = k[j]*p
    A = (n-4)*(1-R2)/(n-p-1)
    B = (1+2*(1-R2)/(n-p+1))
    Rho2 = 1-A*B
    RB = (R2-Rho2)/Rho2
    result <- rbind (result, c (k[j], n, R2, Rho2, RB))
    
  }
}

result_tb <- data.frame (result)
colnames(result_tb) = c("k", "n", "R2", "Rho2", "RBias")
#print(result_tb)
write.csv(result_tb, "/Users/ying/Desktop/Dissertation writing/Finalized results/adequate_k.csv", row.names = FALSE)

result_tb2<-subset(result_tb, RBias <=0.055)
#print(result_tb2)
