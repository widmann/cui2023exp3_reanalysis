pdf(file = "Figure4.pdf", width = 8, height = 6)

par(mfrow = c(2,2))


time <- 1:100
condition1 <- exp(-time/10*0.8)*5+3 
condition2 <- exp(-time/10*0.8)*5
diff <- condition1 - condition2
plot(time, condition1, 
     type = "l", 
     col = "red", 
     lwd = 2, 
     xlim = c(0,100),
     ylim = c(0,10),
     ylab = "Outcome",
     xlab = "Trial",
     main = "Trends within conditions")
lines(time, condition2, col = "blue", lwd = 2)

legend("topright", col = c("red", "blue"), lty = 1, lwd = 2, legend = c("Condition A", "Condition B"), bty = "n")


plot(time, diff, 
     type = "l", 
     col = "black", 
     lwd = 2, 
     xlim = c(0,100),
     ylim = c(0,10),
     ylab = "Condition Effect",
     xlab = "Trial",
     main = "Trends of condition effect")



cum_mean <- (cumsum(condition1) - cumsum(condition2))/cummax(time)
lines(time, cum_mean, lwd = 2, lty = 2, col = "gray")

legend("topright", lty = 1:2, lwd = 2, col = c("black", "gray"), legend = c("Conditional Mean", "Marginal Mean"), bty = "n")




time <- 1:100
condition1 <- exp(-time/10*0.8)*10 
condition2 <- exp(-time/10*0.8)*2
diff <- condition1 - condition2
plot(time, condition1, 
     type = "l", 
     col = "red", 
     lwd = 2, 
     xlim = c(0,100),
     ylab = "Outcome",
     xlab = "Trial",
     main = "Trends within conditions")
lines(time, condition2, col = "blue", lwd = 2)
legend("topright", col = c("red", "blue"), lty = 1, lwd = 2, legend = c("Condition A", "Condition B"), bty = "n")


plot(time, diff, 
     type = "l", 
     col = "black", 
     lwd = 2, 
     xlim = c(0,100),
     ylim = c(0,10),
     ylab = "Condition Effect",
     xlab = "Trial",
     main = "Trends of condition effect")

cum_mean <- (cumsum(condition1) - cumsum(condition2))/cummax(time)
lines(time, cum_mean, lwd = 2, lty = 2, col = "gray")

legend("topright", lty = 1:2, lwd = 2, col = c("black", "gray"), legend = c("Conditional Mean", "Marginal Mean"), bty = "n")

dev.off()