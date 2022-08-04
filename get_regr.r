library(dplyr)
library(ISwR)
library(corrplot)
library(nortest)


nz <- read.csv2("",sep = ';', header = TRUE, dec = ".")
pred1 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred2 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred3 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred4 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred5 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred6 <- read.csv2("", sep = ';', header = TRUE, dec = ".")
pred7 <- read.csv2("",sep = ';', header = TRUE, dec = ".")


M <- merge(nz, pred1, by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred2,by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred3,by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred4,by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred5,by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred6,by = "X.DATE.", incomparables = NA, sort = FALSE) %>%
  merge(pred7,by = "X.DATE.", incomparables = NA, sort = FALSE)
M
save(M, file = "Stocks.RData")
write.csv2(M, "Stocks.csv")

F_ <- diff(log(M$X.CLOSE.F.))
pred_1 <- diff(log(M$X.CLOSE.INDEX.))
pred_2 <- diff(log(M$X.CLOSE.GOLD.))
pred_3 <- diff(log(M$X.CLOSE.OIL.))
pred_4 <- diff(log(M$X.CLOSE.FRANK.))
pred_5 <- diff(log(M$X.CLOSE.EURO.))
pred_6 <- diff(log(M$X.CLOSE.PLATINUM.))
pred_7 <- diff(log(M$X.CLOSE.CROWN.))


reg <- lm(F_ ~ pred_1 + pred_2 + pred_3 + pred_4 + pred_5 + pred_6 + pred_7)
summary(reg)


states <- as.data.frame(M[,c("X.CLOSE.F.", "X.CLOSE.INDEX.", "X.CLOSE.GOLD.","X.CLOSE.OIL.", "X.CLOSE.FRANK.", "X.CLOSE.EURO.", "X.CLOSE.PLATINUM.", "X.CLOSE.CROWN.")])
cor_table <- cor(states)

col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#000000"))
corrplot(cor_table, method = "shade", col = col4(10), cl.length = 11, order = "AOE", addCoef.col = "red")


new_reg <- lm(F_ ~ pred_7 + pred_6 + pred_1 + pred_5)
summary(new_reg)
summary(reg)


hist(rstudent(new_reg), xlab = 'Standardized Residuals', col = "cadetblue1")
qqnorm(rstudent(new_reg),col = "cadetblue2")
ks.test(rstudent(new_reg), "pnorm", 0, 1)
pearson.test(rstudent(new_reg))
acf(rstudent(new_reg))
