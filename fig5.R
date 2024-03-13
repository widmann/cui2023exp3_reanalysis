library(margins)
dev.off()
pdf(file = "Figure5.pdf", width = 8, height = 4.5)
set.seed(222)
par(mfrow = c(1,2))
time <- 1:100
condition1 <- exp(-time/10*0.8)*5+2 
condition2 <- exp(-time/10*0.8)*5+1
diff <- condition1 - condition2

set.seed(22)
x1 <- sample(rep(1:2,50), size = 100)
y1 <- vector(length = 100)

for (i in 1:100){
  y1[i] <- ifelse(x1[i]==1, condition1[i], condition2[i]) + rnorm(n = 100, sd = 1)
}

means1 <- aggregate(y1 ~ x1, data = NULL, mean)




plot(time, condition1, 
     type = "l", 
     col = alpha("red", 0.5), 
     lwd = 2, 
     xlim = c(0,110),
     ylim = c(0,6),
     ylab = "Outcome",
     xlab = "Trial",
     main = "After detrending")
lines(time, condition2, col = alpha("blue", 0.5), lwd = 2)

legend("topright", col = c("red", "blue"), lty = 1, lwd = 2, legend = c("Condition A", "Condition B"), bty = "n")


points(time,y1, col = ifelse(x1 == 1, "red", "blue"))
myDat <- data.frame(y1, x1, time = scale(time, scale = F))
myDat$x1 <- factor(myDat$x1, labels = c("A","B"))
fit_naive1 <- lm(y1 ~ x1, myDat)
fit_trend1 <- lm(y1 ~ 1 +x1 + time + I(time^2) + I(time^3) + I(time^4), myDat)
myDat$x1 <- relevel(myDat$x1, ref = "B")
fit_trend2 <- lm(y1 ~ x1 + time + I(time^2) + I(time^3) + I(time^4), myDat)
fit_naive2 <- lm(y1 ~ x1, myDat)


summary(fit_naive1)
summary(fit_trend1)

m_trend1 <- margins(fit_trend1)
m_trend2 <- margins(fit_trend2)

summary(m_trend1)
summary(m_trend2)

b_naive1 <- coef(fit_naive1)
b_naive2 <- coef(fit_naive2)

points(x = c(105,105),y = c(b_naive1[1], b_naive2[1]), pch = 15, col = c("red", "blue"), cex = 1.5)
arrows(x0=105, y0= confint(fit_naive1)[1,1], x1=105, y1=confint(fit_naive1)[1,2], code=3, angle=90, length=0.2, col="red", lwd=2)
arrows(x0=105, y0= confint(fit_naive2)[1,1], x1=105, y1=confint(fit_naive2)[1,2], code=3, angle=90, length=0.2, col="blue", lwd=2)

###### Plot again after correction for the trend

# create new data set without trend
myDat2 <- myDat
myDat2$x1 <- relevel(myDat2$x1, ref = "A")
fit_trend <- lm(y1 ~ 1 + time + I(time^2) + I(time^3) + I(time^4), myDat)
myDat2$y1 <- mean(myDat2$y1) + resid(fit_trend)

plot(time, condition1, 
     type = "l", 
     col = alpha("red", 0), 
     lwd = 2, 
     xlim = c(0,110),
     ylim = c(0,6),
     ylab = "Outcome",
     xlab = "Trial",
     main = "Before detrending")
lines(time, condition2, col = alpha("blue", 0), lwd = 2)

legend("topright", col = c("red", "blue"), lty = 1, lwd = 2, legend = c("Condition A", "Condition B"), bty = "n")


points(time,myDat2$y1, col = ifelse(myDat2$x1 == "A", "red", "blue"))

fit_naive1 <- lm(y1 ~ x1, myDat2)
fit_trend1 <- lm(y1 ~ 1 +x1 + time + I(time^2) + I(time^3) + I(time^4), myDat2)
myDat2$x1 <- relevel(myDat2$x1, ref = "B")
fit_trend2 <- lm(y1 ~ x1 + time + I(time^2) + I(time^3) + I(time^4), myDat2)
fit_naive2 <- lm(y1 ~ x1, myDat2)


summary(fit_naive1)
summary(fit_trend1)

m_trend1 <- margins(fit_trend1)
m_trend2 <- margins(fit_trend2)

summary(m_trend1)
summary(m_trend2)

b_naive1 <- coef(fit_naive1)
b_naive2 <- coef(fit_naive2)

points(x = c(105,105),y = c(b_naive1[1], b_naive2[1]), pch = 15, col = c("red", "blue"), cex = 1.5)
arrows(x0=105, y0= confint(fit_naive1)[1,1], x1=105, y1=confint(fit_naive1)[1,2], code=3, angle=90, length=0.2, col="red", lwd=2)
arrows(x0=105, y0= confint(fit_naive2)[1,1], x1=105, y1=confint(fit_naive2)[1,2], code=3, angle=90, length=0.2, col="blue", lwd=2)


dev.off()


