######### Selecting correlation values for different dominance structure conditions #######
setwd("/Users/ying/Desktop/Dissertation writing")

# I. Compute R2 for no dominancy, single dominancy & single independent dominancy
fun_r2 <- function(r1, r2, p, k){  # function used to compute R2 
  mm <- matrix(c(k*r1, 1, p*r2, p*r2, p*r2, p*r2, 
                 r1, p*r2, 1, r2, r2, r2,
                 r1, p*r2, r2, 1, r2, r2,
                 r1, p*r2, r2, r2, 1, r2,
                 r1, p*r2, r2, r2, r2, 1), 
               nrow = 5, ncol = 6, byrow = TRUE)

  rxy <- mm[,1]
  rxx <- mm[,2:ncol(mm)]
  r2 <- t(rxy)%*%solve(rxx)%*%rxy
  
  return(r2)
}

# 1. Single dominancy 
# (0.1, 0.2, 0.75, 5) for lower model fit (R2 = 0.25)
fun_r2(0.1, 0.2, 0.75, 5)
# (0.1, 0.2, 0.75, 7) for higher model fit (R2 = 0.49)
fun_r2(0.1, 0.2, 0.75, 7)

# 2. No dominancy
# Special case of single dominancy where k = p = 1
# (0.3, 0.2, 1, 1) for lower model fit (R2 = 0.25)
fun_r2(0.3, 0.2, 1, 1)
# (0.45, 0.2, 1, 1) for higher model fit (R2 = 0.56)
fun_r2(0.45, 0.2, 1, 1)

# 3. Single independent dominancy
# (0.2, 0.2, 0, 1) for lower model fit (R2 = 0.24)
fun_r2(0.26, 0.2, 0, 1)
# (0.3, 0.2, 0, 1) for higher model fit (R2 = 0.56)
fun_r2(0.4, 0.2, 0, 1)

# II. Compute R2 for two-factor structure
mtrx2 <- function(rx1, rx2, rx3, ry1, ry2){
  mm <- matrix(c(ry1, 1, rx1, rx1, rx3, rx3, 
                 ry1, rx1, 1, rx1, rx3, rx3,
                 ry1, rx1, rx1, 1, rx3, rx3,
                 ry2, rx3, rx3, rx3, 1, rx2,
                 ry2, rx3, rx3, rx3, rx2, 1), 
               nrow = 5, ncol = 6, byrow = TRUE)
  return(mm)
}

# (0.3, 0.3, 0.1, 0.3, 0.33) for lower model fit (R2 = 0.29)
# (0.3, 0.3, 0.1, 0.4, 0.44) for higher model fit (R2 = 0.51)
rm2 <- mtrx2(0.3, 0.3, 0.1, 0.3, 0.33)
xy1<-rm2[1:3,1]
xy2<-rm2[4:5,1]
xx1<-rm2[1:3,2:4]
xx2<-rm2[4:5,5:6]
# compute R2 of two subsets
t(xy1)%*%solve(xx1)%*%xy1
t(xy2)%*%solve(xx2)%*%xy2

# compute overall R2
rxy2 <- rm2[,1]
rxx2 <- rm2[,2:ncol(rm2)]
t(rxy2)%*%solve(rxx2)%*%rxy2

# III. Compute R2 for condition 9
r9 <- read.xlsx("corr_matrix_all.xlsx", sheet = "cond9", colNames = FALSE)[-1,]
rxy <- r9[,1]
rxx <- r9[,2:ncol(r9)]
t(rxy)%*%solve(rxx)%*%rxy
