
#----------some functions---------------------------------------------------------

#Root Mean Squared Error
rmse <- function(error)
{sqrt(mean(error^2))}
# Mean Absolute Error
mae <- function(error)
{mean(abs(error))}     

#--function for making point transparent in a plot---
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

#----------define linear models--------------------------------------------------------------

#define the linear models (excluded CRU because station data came from CRU)
y = list(allstations_erai_SA, allstations_erai_A, allstations_erai_SEA, 
         allstations_MERRA2_SA, allstations_MERRA2_A, allstations_MERRA2_SEA,
         allstations_jra55_SA, allstations_jra55_A, allstations_jra55_SEA,
         allstations_cfsr_SA, allstations_cfsr_A, allstations_cfsr_SEA)

lm_all_results <- list()
for (i in 1:length(y)) {
  lm_obj <- lm(model ~ obs, data = y[[i]])
  tmp <- c(lm_obj$coefficients,
           summary(lm_obj)$r.squared,
           mae(lm_obj$residuals),
           rmse(lm_obj$residuals))
  names(tmp) <- c("intercept", "model", "r.squared", "mae", "rmse")
  lm_all_results[[i]] <- tmp
}
lm_all_results <- as.data.frame(do.call(rbind, lm_all_results))
row.names(lm_all_results) = c("erai_SA", "erai_A", "erai_SEA", "MERRA2_SA", "MERRA2_A", "MERRA2_SEA", 
                              "jra55_SA", "jra55_A", "jra55_SEA","cfsr_SA", "cfsr_A", "cfsr_SEA")

#----------create the plots -------------------------------------

#names=c("erai_SA", "erai_A", "erai_SEA", "MERRA2_SA", "MERRA2_A", "MERRA2_SEA", 
#         "jra55_SA", "jra55_A", "jra55_SEA","cfsr_SA", "cfsr_A", "cfsr_SEA")

colours = c("blue", "blue", "blue", 
            "forestgreen", "forestgreen", "forestgreen",
            "darkorchid4", "darkorchid4", "darkorchid4",
            "red", "red","red")

par(mfrow=c(4,3), pty='s',mai = c(0.3, 0.5, 0.1, 0.2), font=2) 
for (i in 1:length(y)) {
  plot(y[[i]]$obs, y[[i]]$model, 
     xlab = "", ylab = "",
     grid(), pch=20, col=addTrans(colours[i],100), bg = "blue")
     abline(0,1)
     abline(lm_all_results$intercept[i],lm_all_results$model[i], col = "black", lty = "dashed", lwd=1.2)
     legend("topleft", cex=0.9, bty="n", y.intersp = 0.6, 
            legend=c((paste("r2 =", format(lm_all_results$r.squared[i], digits = 2))), 
                    (paste("MAE =", format(lm_all_results$mae[i], digits = 2))),
                    (paste("RMSE =", format(lm_all_results$rmse[i], digits = 2))))                                                            )
}

#QQplots
par(mfrow=c(4,3), pty='s',mai = c(0.3, 0.5, 0.1, 0.2), font=2) 
for (i in 1:length(y)) {
  qqplot(sort(y[[i]]$obs), sort(y[[i]]$model), 
     xlab = "", ylab = "",
     pch=20, col=colours[i], bg = "blue")
     abline(0,1)
     legend("topleft", cex=0.9, bty="n", legend=names[i])
}


plot(quantile(y[[4]]$obs, probs = seq(0, 1, 0.01), na.rm=T), quantile(y[[4]]$model, probs = seq(0, 1, 0.01)))
abline(0:1)


#--------------------------------------------------------------------------------------

#plot each individual station datasets against the corresponding reananlysis pixel (for the dataframes with
#multiple stations as columns) This works but havent figured out how to add the linear model stats to the graphs  
stationnames = colnames(stationcomparison_ERAI_SA)

par(mfrow=c(6,6), pty="s", mai = c(0.1, 0.5, 0.2, 0.2), las = 2)
for (i in 1:length(stationnames)) {
  if (i %% 2 ==0) {next}
  plot(stationcomparison_ERAI_SA[,i], stationcomparison_ERAI_SA[,i+1],
              xlab = "", ylab = "", mgp=c(2.3,1,0), xlim=c(20,33), ylim=c(20,33),
              grid(),  pch=20, col="blue", bg = "blue")
              abline(0,1)
              legend("bottomright", stationnames[i], bty = 'n')
      }
              

#attempting to loop through the station dfs and calculate linear model stats: slope, intercept, mae, rmse, r2)     
#calculate statistics of station comparisons
lm_ERAI = data.frame(slope=rep(0, 35), intercept=rep(0,35), r2=rep(0,35),
                     mae=rep(0,35), rmse=rep(0,35))

lm_ERAI = list()
for (i in length(stationnames)) {
  if (i %% 2 ==0) {next}
  lm_obj = lm(stationcomparison_ERAI[,i] ~ stationcomparison_ERAI[,i+1])
  slope = summary(lm_obj$coefficients[2,1])
  intercept = summary(lm_obj$coefficients[1,1])
  r2 = summary(lm_obj)$r.squared
  mae = mae(lm_obj$residuals)
  rmse = rmse(lm_obj)$residuals
  lm_ERAI = as.data.frame(slope, intercept, r2, mae, rmse)
}
lm_ERAI= as.data.frame(do.call(rbind, lm_ERAI))       
























