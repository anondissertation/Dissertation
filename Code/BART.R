install.packages("BART")

library(BART)
library(matrixStats)

X <- read.csv("C:/DISSERTATION/Data/X_nocons.csv")
X <- X[,2:46]
X <- data.matrix(X)

start_time <- Sys.time()
for (knob in 0:8){
  for (dataset in 0:9){
    print(paste("fitting ", as.character(knob), as.character(dataset), sep=""))
    name <- paste("C:/DISSERTATION/Data/dataset_", as.character(knob), as.character(dataset), ".csv", sep="")
    df <- read.csv(name)
    y <- unlist(df["y"])
    D <- unlist(df["D"])
    p_hat <- unlist(read.csv(paste("C:/DISSERTATION/Results/AML/p_hat/dataset_", as.character(knob), as.character(dataset), ".csv", sep=""))["phat"])
    
    XD <- cbind(X, p_hat, D)
    X_tr <- cbind(X, p_hat, rep(1,nrow(X)))
    X_nt <- cbind(X, p_hat, rep(0,nrow(X)))
    
    stdBART_temp <-    wbart(x.train=XD, y.train=y, ndpost=200L)
    bart_all_T_preds <- pwbart(x.test=X_tr, stdBART_temp$treedraws)
    bart_all_C_preds <- pwbart(x.test=X_nt, stdBART_temp$treedraws)
    
    bart_ITE_draws_matrix <- bart_all_T_preds-bart_all_C_preds
    
    tauhat <- colMeans(bart_ITE_draws_matrix)
    CI <- colQuantiles(bart_ITE_draws_matrix, probs = c(0.025,0.975))
    results <- cbind(tauhat, CI)
    write.csv(results, paste("C:/DISSERTATION/Results/BART/dataset_", as.character(knob), as.character(dataset), ".csv", sep=""))
  }
}
end_time <- Sys.time()
end_time - start_time