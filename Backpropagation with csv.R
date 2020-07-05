#panggil package
require(neuralnet)
require(caret)
require(pracma)

#baca data
myData <- read.csv("dataDiabetes.csv", header = TRUE)
myData <- myData[1:200,]

#scale & normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
myData <- as.data.frame(lapply(myData, normalize))
set.seed(99)

#test set
dataTest <- myData[1:20,]

#training set
Glucose = myData[21:200,1]
BloodPressure = myData[21:200,2]
BMI = myData[21:200,3]
Age = myData[21:200,4]
Diabetes = myData[21:200,5]
dataTrain = data.frame(Glucose, BloodPressure, BMI, Age, Diabetes)

#neural network classifier
NNClassifier = neuralnet(formula = Diabetes~Glucose+BloodPressure+BMI+Age, data = dataTest, hidden = c(3,2),
                         threshold = 0.05, learningrate = 0.05, stepmax = 1e+06, 
                         algorithm = "backprop", act.fct = "tanh", linear.output = FALSE)
plot(NNClassifier)

#hasil prediksi
Predict = compute(NNClassifier, dataTest)
prediksi <- Predict$net.result
predRounded <- ifelse(prediksi>0.5, 1, 0)
hasilAkhir = cbind(dataTest$Diabetes, predRounded, prediksi)
colnames(hasilAkhir) = c("Data Asli", "Pred. Round", "Prediksi")

#hasil akurasi
stats <- confusionMatrix(table(dataTest$Diabetes, predRounded))
akurasi <- stats$overall['Accuracy']

hasilAkhir
print(akurasi)
