data_cbe_path<-file.path(getwd(), 'cbe.dat')

print(data_cbe_path)

CBE <- read.table(data_cbe_path, header = T)
class(CBE)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)

layout(1:2)
png("CBE.png")
plot(cbind(Beer.ts, Choc.ts), main="Beer (Ml) & Chocolate (tonnes) Production", col="blue")
dev.off()
browseURL("CBE.png")

Beer.annual.ts <- aggregate(Beer.ts)/12
Choc.annual.ts <- aggregate(Choc.ts)/12

png("CBE_agg_annual.png")
plot(cbind(Beer.annual.ts, Choc.annual.ts), main=" annual average Beer (Ml) & Chocolate (tonnes) Production", col="orange", lwd=3)
dev.off()
browseURL("CBE_agg_annual.png")

png("CBE_box_plot.png")
layout(1:2)
boxplot(Choc.ts ~ cycle(Choc.ts), names=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec" ), main="Monthly Production Distribution for 1958-1990 ",ylab = "Chocolate",col = "orange")
boxplot(Beer.ts ~ cycle(Beer.ts), names=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec" ),  ylab = "Beer",col = "orange")
dev.off()
browseURL("CBE_box_plot.png")

png("Beer_decomp.png")
Beer.decomp <- decompose(Beer.ts, type = c("additive"), filter = NULL)
plot(Beer.decomp, xlab = "Time", lwd=3)
dev.off()
browseURL("Beer_decomp.png")

png("Choc_decomp.png")
Choc.decomp<- decompose(Choc.ts, type = c("additive"), filter = NULL)
plot(Choc.decomp, xlab = "Time", lwd=3)
dev.off()
browseURL("Choc_decomp.png")

png("Beer_trend_seasonal.png")
plot(Beer.decomp$trend + Beer.decomp$seasonal, main="Trend+Seasonal", xlab='Time', ylab='Beer Production (Ml)', col="brown", lwd=3)
dev.off()
browseURL("Beer_trend_seasonal.png")

png("Choc_trend_seasonal.png")
plot(Choc.decomp$trend + Choc.decomp$seasonal, main="Trend+Seasonal", xlab='Time', ylab='Chocolate Production (tonnes)', col="brown", lwd=3)
dev.off()
browseURL("Choc_trend_seasonal.png")

#Seasonally Adjusting
png("BeerSeasonalAdj.png")
BeerSeasonAdj <- Beer.ts - Beer.decomp$seasonal
plot.ts(BeerSeasonAdj, main="Beer Production (Ml)",xlab = "Time",ylab = "Beer Seasonal Adjusted",col = "orange")
lines(Beer.decomp$trend,col='black', lwd=3)
lines(Beer.annual.ts, col='red', lwd=3)
legend("topleft", 
  legend = c("Seasonal Adjusted", "Trend - SMA(order=6)", "Annual Average"), col=c("orange", "black", "red"), lty=1, cex=1, text.font=4, bg='lightblue')
dev.off()
browseURL("BeerSeasonalAdj.png")

png("ChocSeasonalAdj.png")
ChocSeasonAdj <- Choc.ts - Choc.decomp$seasonal
plot.ts(ChocSeasonAdj, main="Chocolate Production (tonnes)",xlab = "Time",ylab = "Chocolate Seasonal Adjusted",col = "orange")
lines(Choc.decomp$trend,col='black', lwd=3)
lines(Choc.annual.ts, col='red', lwd=3)
legend("topleft", 
  legend = c("Seasonal Adjusted", "Trend - SMA(order=6)", "Annual Average"), col=c("orange", "black", "red"), lty=1, cex=1, text.font=4, bg='lightblue')
dev.off()
browseURL("ChocSeasonalAdj.png")

emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Car","Petrol (litre)","servicing(h)","tyre","clutch"),
  q_i_00 = c(0.33, 2000, 40,3,2),
  unit_price_00=c(1800, 0.8, 40, 80, 200),
  q_i_04 = c(0.5, 1500, 20, 2, 1),
  pi_04  = c(2000,1.6, 60, 120, 360),stringsAsFactors = FALSE)
LI.t.denomin = emp.data[,3]*emp.data[,4]
LI.t.nomin = emp.data[,3]*emp.data[,6]
LI.t = sum(LI.t.nomin)/sum(LI.t.denomin)
print(cbind("The Laspeyre price index: ",LI.t))
############## Problem 3
PI.t = sum(emp.data[,5]*emp.data[,6])/sum(emp.data[,5]*emp.data[,4])
print(cbind("The Paasche Price Index: ",PI.t))
print(cbind("The Irving-Fisher Price Index: ",sqrt(PI.t*LI.t)))