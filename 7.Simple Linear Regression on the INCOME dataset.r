income_input = as.data.frame(  read.csv("C:/Users/sahil/Desktop/msc/income.csv")   )
View(income_input)
income_input[1:10,]
summary(income_input)
library(lattice)
splom(~income_input[c(2:5)],  groups=NULL, data=income_input, axis.line.tck = 0,
axis.text.alpha = 0)
pairs(income_input[c(2:5)],)
results <- lm(Income~Age + Education + Gender, income_input)
summary(results)
results2 <- lm(Income ~ Age + Education, income_input)
summary(results2)
confint(results2, level = .95)
Age <- 41
Education <- 12
new_pt <- data.frame(Age, Education)
new_pt
conf_int_pt <- predict(results2, new_pt, level=.95, interval="confidence")
conf_int_pt 
pred_int_pt <- predict(results2, new_pt, level=.95, interval="prediction")
pred_int_pt
