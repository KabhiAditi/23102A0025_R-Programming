library(EBImage)
install.packages("keras")
library(keras)
install_tensorflow()

install.packages("tensorflow")
install.packages("keras3")
library(EBImage)
library(tensorflow)
library(keras3)

# Install/configure TensorFlow
install_tensorflow()

setwd("C:/Users/91976/Desktop/Rlab4")
pics <- c('p1.jpg', 'p2.jpg', 'p3.jpg', 'p4.jpg', 'p5.jpg', 'p6.jpg', 
          'c1.jpg', 'c2.jpg', 'c3.jpg', 'c4.jpg', 'c5.jpg', 'c6.jpg')
mypic <- list()
for(i in 1:12) {mypic[[i]] <- readImage(pics[i])}

print(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[1]])
str(mypic)

#Resize
for(i in 1:12) {mypic[[i]] <- resize(mypic[[i]], 28, 28)}
str(mypic)

#Reshape
for(i in 1:12) {mypic[[i]] <- array_reshape(mypic[[i]], c(28,28,3))}
str(mypic)

trainx <- NULL
for(i in 7:11) {trainx <- rbind(trainx, mypic[[i]])}
str(trainx)
testx <- rbind(mypic[[6]], mypic[[12]])
trainy <- c(0,0,0,0,0,1,1,1,1,1)
testy <- c(0,1)


library(reticulate)

py_config()
install.packages("tensorflow")
library(tensorflow)

install_tensorflow()
#One Hot Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels

#Model
model <- keras_model_sequential()
model %>%
        layer_dense(units=256, activation = 'relu', input_shape = c(2352)) %>%
        layer_dense(units=128, activation = 'relu') %>%
        layer_dense(units=2, activation = 'softmax')
summary(model)

#compile
model %>%
        compile(loss = 'binary_crossentropy', 
                optimizer = optimizer_rmsprop(),
                metrics = c('accuracy'))

# Fit Model
history <- model %>%
          fit(trainx,
              trainLabels,
              epochs = 30,
              batch_size = 32,
              validation_split = 0.2)


plot(history)

#Evaluation & Prediction - train data
model %>% evaluate(trainx, trainLabels)
pred <- model %>% predict_classes(trainx)
table(Predicted = pred, Actual = trainy)
prob <- model %>% predict_proba(trainx)
cbind(prob, Prected = pred, Actual = trainy)

#Evaluation & Prediction - test data
model %>% evaluate(testx, testLabels)
pred <- model %>% predict_classes(testx)
table(Predicted = pred, Actual = testy)
