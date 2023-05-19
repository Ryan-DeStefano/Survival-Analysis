cir = na.omit(cirrhosis)

for (i in 1:nrow(cir)){
  if (cir$Status[i] == "D"){
    cir$Status[i] = 1
  }
  else{
    cir$Status[i] = 0
  }
}

cir$Status = as.integer(cir$Status)

cir$Age = cir$Age / 365

write.csv(cir, "cir.csv", row.names=FALSE)

# Regression Analysis
cr.obj <-coxph(Surv(N_Days,Status)~as.factor(Drug)+as.factor(Sex)+as.factor(Stage)+Age+Cholesterol, data=cir)

summary(cr.obj)

schoen = residuals(cr.obj, type = "schoenfeld")

cir.times = sort(cir[cir$Status != 0 , ]$N_Days)

plot(cir.times, schoen[,1], xlab = "Days", ylab = "Schoenfeld Residuals", main = "Predictor: Drug" )
smooth.sres = lowess(cir.times, schoen[,1])
lines(smooth.sres$x, smooth.sres$y, lty = 1)

plot(cir.times, schoen[,2], xlab = "Days", ylab = "Schoenfeld Residuals", main = "Predictor: Sex" )
smooth.sres = lowess(cir.times, schoen[,2])
lines(smooth.sres$x, smooth.sres$y, lty = 1)

plot(cir.times, schoen[,3], xlab = "Days", ylab = "Schoenfeld Residuals", main = "Predictor: Stage" )
smooth.sres = lowess(cir.times, schoen[,3])
lines(smooth.sres$x, smooth.sres$y, lty = 1)

plot(cir.times, schoen[,6], xlab = "Days", ylab = "Schoenfeld Residuals", main = "Predictor: Age" )
smooth.sres = lowess(cir.times, schoen[,6])
lines(smooth.sres$x, smooth.sres$y, lty = 1)

plot(cir.times, schoen[,7], xlab = "Days", ylab = "Schoenfeld Residuals", main = "Predictor: Cholesterol" )
smooth.sres = lowess(cir.times, schoen[,7])
lines(smooth.sres$x, smooth.sres$y, lty = 1)

cox.zph(cr.obj, transform = "log")
