##Not run
data(Wilshire)
#Rebuilding model1 from ecm example
trn <- Wilshire[Wilshire$date<='2015-12-01',]
xeq <- xtr <- trn[c('CorpProfits', 'FedFundsRate', 'UnempRate')]
model1 <- ecm(trn$Wilshire5000, xeq, xtr)
model2 <- ecm(trn$Wilshire5000, xeq, xtr, linearFitter='earth')
model3 <- ecmave(trn$Wilshire5000, xeq, xtr, linearFitter='earth', k=5)
#Use 2015-12-01 and onwards data as test data to predict
tst <- Wilshire[Wilshire$date>='1980-12-01',]
#predict on tst using model1 and initial FedFundsRate
tst$model1Pred <- ecmpredict(model1, tst, tst$Wilshire5000[1])
tst$model2Pred <- ecmpredict(model2, tst, tst$Wilshire5000[1])
tst$model3Pred <- ecmpredict(model3, tst, tst$Wilshire5000[1])

# Plot the result
ggplot(tst, aes(x=date)) + 
  geom_line(aes(y=Wilshire5000)) +
  geom_line(aes(y=model3Pred), color="red") + 
  geom_line(aes(y=model2Pred), color="blue")

print(accuracy(model1))
print(accuracy(model2))
print(accuracy(model3))
