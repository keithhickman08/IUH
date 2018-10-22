# Online Applied Data Mining, IUB, 2017
# Case Study: Prediction Algae Blooms
# Chapter 4 : Prediction for the Seven Algae
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban
#######################################################################################
#obtain the best workflow for each of the sevena algae
#res.all includes the trained models and we are here picking the best models using NMSE
wfs <- sapply(taskNames(res.all),
              function(t) topPerformer(res.all,metric="nmse",task=t))
wfs[["a1"]] # best model for "a1"
wfs[["a7"]] # best model for "a7"
#best workflows for the entire test set
full.test.algae <- cbind(test.algae, algae.sols) # test data
#pts includes true values and predictions
pts <- array(dim = c(140,7,2),
             dimnames = list(1:140, paste0("a",1:7), c("trues","preds")))
for(i in 1:7) {
  res <- runWorkflow(wfs[[i]],
                     as.formula(paste(names(wfs)[i],"~.")),
                     algae[,c(1:11,11+i)],
                     full.test.algae[,c(1:11,11+i)])
  pts[,i,"trues"] <- res$trues
  pts[,i,"preds"] <- res$preds
}
# prediction and true values for "a1" and "a3" on the first 3 test cases
pts[1:3,c("a1","a3"),] 
#NSM scores of our models on the seven algae
avg.preds <- apply(algae[,12:18], 2, mean)
apply((pts[,,"trues"] - pts[,,"preds"])^2, 2 ,sum) /
  apply( (scale(pts[,,"trues"], avg.preds, FALSE))^2, 2, sum)