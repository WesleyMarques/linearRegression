dados <- read.table("~/workspaceR/LinearRegression/data/prostate.data")
df <- melt(dados)

summary(dados)

boxplot(dados, main="Boxsplot", xlab="Variables", ylab="Count")

ggplot(df,aes(x = value)) + facet_wrap(~variable, scales = "free_x") + geom_histogram()
correlationMatrix <- cor(dados)
corrplot(correlationMatrix, method="number", type="lower", order="hclust")

train <- filter(dados,train)
test <- filter(dados,!train)
lm <- lm(lpsa ~ lcavol+svi+lcp,data = train)
prediction <- predict(lm, select(test,lcavol, svi, lcp))
lm_prediction <- data.frame(pred = prediction, obs = test$lpsa)
ggplot(lm_prediction, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("Previsão x Observado (validação)")
round(defaultSummary(lm_prediction), digits = 3)
corrplot(correlationMatrix, method="circle", type="lower", order="hclust")
