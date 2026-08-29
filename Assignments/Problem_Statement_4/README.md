# Image Classification Using ANN in R

## Objective

To implement an image classification model using an **Artificial Neural Network (ANN)** in R with **Keras and TensorFlow**.

The project demonstrates image preprocessing, neural network model creation, training, and evaluation.

## Problem Description

Image classification involves assigning images to predefined classes. In this project, a small set of images is processed and classified into **two classes (0 and 1)** using a feed-forward neural network.

The images are resized, reshaped, and converted into a suitable format before being provided to the neural network.

## Dataset

The dataset consists of **12 JPEG images**:

* `p1.jpg` to `p6.jpg`
* `c1.jpg` to `c6.jpg`

The images are resized to **28 × 28 pixels** and represented using **3 RGB channels**.

The data is divided into training and testing sets, with class labels `0` and `1`.

## Libraries Used

The following R packages are used:

* **EBImage** – Image reading, processing, and visualization
* **Keras / Keras3** – Building and training the neural network
* **TensorFlow** – Deep learning backend
* **Reticulate** – Interface between R and Python

## Major Operations

1. Load the required R packages and configure TensorFlow.
2. Load the input images using `EBImage`.
3. Inspect the images using summary, histogram, and structure functions.
4. Resize all images to **28 × 28 pixels**.
5. Reshape the images into **28 × 28 × 3** arrays.
6. Create training and testing datasets.
7. Apply one-hot encoding to the class labels.
8. Build a sequential neural network using Keras.
9. Compile the model using:

   * Binary Cross-Entropy loss
   * RMSprop optimizer
   * Accuracy metric
10. Train the model for **30 epochs** with a batch size of **32**.
11. Plot the training history.
12. Evaluate the model on training and testing data.
13. Generate predictions and compare predicted classes with actual classes.

## Model Architecture

The neural network consists of:

```text
Input Layer (2352 features)
        ↓
Dense Layer (256 neurons, ReLU)
        ↓
Dense Layer (128 neurons, ReLU)
        ↓
Output Layer (2 neurons, Softmax)
```

The input size is **2352**, calculated as:

`28 × 28 × 3 = 2352`

## How to Run

### 1. Install R

Install R and preferably **RStudio**.

### 2. Install Required Packages

Run the following commands in R:

```r
install.packages("keras")
install.packages("tensorflow")
install.packages("keras3")
install.packages("reticulate")
```

### 3. Configure TensorFlow

```r
library(tensorflow)
install_tensorflow()
```

### 4. Add the Dataset

Place the following image files in the project directory:

```text
p1.jpg
p2.jpg
p3.jpg
p4.jpg
p5.jpg
p6.jpg
c1.jpg
c2.jpg
c3.jpg
c4.jpg
c5.jpg
c6.jpg
```

### 5. Update the Working Directory

Modify the `setwd()` path in the R script according to your project location:

```r
setwd("your/project/path")
```

### 6. Run the Script

Open `R_Lab04_23102A0025.R` in RStudio and run the script.

The program will load and preprocess the images, train the ANN model, and perform model evaluation and prediction.

## Project Structure

```text
Image-Classification-R/
│
├── R_Lab04_23102A0025.R
├── p1.jpg
├── p2.jpg
├── p3.jpg
├── p4.jpg
├── p5.jpg
├── p6.jpg
├── c1.jpg
├── c2.jpg
├── c3.jpg
├── c4.jpg
├── c5.jpg
├── c6.jpg
└── README.md
```

## Conclusion

This project demonstrates how an **Artificial Neural Network can be implemented in R for image classification** using Keras and TensorFlow. It covers the complete workflow from image preprocessing to model training, prediction, and evaluation.
