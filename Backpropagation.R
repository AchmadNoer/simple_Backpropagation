#panggil package
#install.packages("neuralnet")
library(neuralnet)
library(caret)
library(pracma)

#training set
Cuaca = c(1,1,2,3,3,3,2,1,1,3,1,2,2,3) #cerah=1, mendung=2, hujan=3
Suhu = c(1,1,1,2,3,3,3,2,3,2,2,2,1,2) #panas=1, sedang=2, dingin=3
Lembab = c(1,1,1,1,0,0,0,1,0,0,0,1,0,1) #tinggi=2, normal=1
Angin = c(0,1,0,0,0,1,1,0,0,0,1,1,0,1) #tidak=1, ya=2
Status = c(0,0,1,1,1,0,1,0,1,1,1,1,1,0) #main=1, tidur=0
dataTrain = data.frame(Lembab, Angin, Status)

#neural network classifier
set.seed(99)
#W <- c(.1,.1,.1,.1,.1,.1,.1,.1,.1)
NNClassifier = neuralnet(Status~Lembab+Angin, dataTrain, hidden = c(2),
                         threshold = 0.01, learningrate = 0.001, stepmax = 1e+06, 
                         algorithm = "backprop", act.fct = "tanh", linear.output = FALSE)
plot(NNClassifier)

#test set
#Cuaca = c(1,1,2,3,3,3,2,1,1,3,1,2,2,3) #cerah=1, mendung=2, hujan=3
#Suhu = c(1,1,1,2,3,3,3,2,3,2,2,2,1,2) #panas=1, sedang=2, dingin=3
#dataTest = data.frame(Cuaca, Suhu, Status)

#hasil prediksi
Predict = compute(NNClassifier, dataTrain)
prediksi <- Predict$net.result
predRounded <- ifelse(prediksi>0.5, 1, 0)

hasilAkhir = cbind(Status, predRounded, prediksi)
colnames(hasilAkhir) = c("Data Asli", "Pred. Convert", "Prediksi")
hasilAkhir

stats <- confusionMatrix(table(Status, predRounded))
akurasi <- stats$overall['Accuracy']
print(akurasi)
