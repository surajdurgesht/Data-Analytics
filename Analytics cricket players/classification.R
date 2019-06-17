library(AppliedPredictiveModeling)
library(RColorBrewer)
library("plyr")
library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("caret")
library("gridExtra")
library("grid")

data(twoClassData)
twoClass=cbind(as.data.frame(predictors),classes)
twoClassColor <- brewer.pal(3,'Set1')[1:2]
names(twoClassColor) <- c('Class1','Class2')
ggplot(data = twoClass,aes(x = PredictorA, y = PredictorB)) + 
  geom_point(aes(color = classes), size = 6, alpha = .5) +
  scale_colour_manual(name = 'classes', values = twoClassColor) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

nbp <- 250;
PredA <- seq(min(twoClass$PredictorA), max(twoClass$PredictorA), length = nbp)
PredB <- seq(min(twoClass$PredictorB), max(twoClass$PredictorB), length = nbp)
Grid <- expand.grid(PredictorA = PredA, PredictorB = PredB)

PlotGrid <- function(pred,title) {
  surf <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB, 
                                       color = classes)) +
             geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
             scale_fill_manual(name = 'classes', values = twoClassColor) +
             ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
             scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  pts <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB,  
                                      color = classes)) +
            geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                         color = "red", breaks = c(1.5)) +
            geom_point(size = 4, alpha = .5) + 
            ggtitle("Decision boundary") +
            theme(legend.text = element_text(size = 10)) +
            scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  grid.arrange(surf, pts, top = textGrob(title, gp = gpar(fontsize = 20)), ncol = 2)
}

library("caret")
V <- 10
T <- 4
TrControl <- trainControl(method = "repeatedcv",
                          number = V,
                          repeats = T)

Seed <- 345
ErrsCaret <- function(Model, Name) {
  Errs <- data.frame(t(postResample(predict(Model, newdata = twoClass), twoClass[["classes"]])),
                     Resample = "None", model = Name)
  rbind(Errs, data.frame(Model$resample, model = Name))
}

Errs <- data.frame()
CaretLearnAndDisplay <- function (Errs, Name, Formula, Method, ...) {
  set.seed(Seed)
  Model <- train(as.formula(Formula), data = twoClass, method = Method, trControl = TrControl, ...)
  Pred <- predict(Model, newdata = Grid)
  PlotGrid(Pred, Name)
  Errs <- rbind(Errs, ErrsCaret(Model, Name))
}
Errs <- CaretLearnAndDisplay(Errs, "Linear Discrimant Analysis", "classes ~ .", "lda")
Errs <- CaretLearnAndDisplay(Errs, "Quadratic Discrimant Analysis", "classes ~ . ", "qda")
Errs <- CaretLearnAndDisplay(Errs, "Logistic", "classes ~ .", "glm")

ErrsKNN <- data.frame()
KNNKS <- c(1, 9, 13, 21, 29)
for (k in KNNKS) {
  ErrsKNN <- CaretLearnAndDisplay(ErrsKNN, sprintf("k-NN with k=%i", k), 
                                  "classes ~ .","knn", tuneGrid = data.frame(k = c(k)))
}

Errs <- rbind(Errs, ErrsKNN)
Errs <- CaretLearnAndDisplay(Errs, "k-NN with CV choice", "classes ~ .", "knn",
                             tuneGrid = data.frame(k = KNNKS))

library(rpart.plot)
Tree <- train(classes ~ ., data = twoClass, method = "rpart", control = rpart::rpart.control(minsplit = 5, cp = 0),
              tuneGrid = data.frame(cp = .02), trControl = TrControl)
Tree$finalModel
rpart.plot(Tree$finalModel)
prp(Tree$finalModel, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, 
    box.col = twoClassColor[Tree$finalModel$frame$yval])

Errs <- CaretLearnAndDisplay(Errs, "Bagging", "classes ~ .", "treebag", 
                             control = rpart.control(minsplit = 5))

ErrKNN <- ErrAndPlotErrs(ErrsKNN)
FindBestErr(ErrKNN)


ErrCaretAccuracy <- function(Errs) {
  Errs <- group_by(Errs, model)
  cbind(dplyr::summarize(Errs, mAccuracy = mean(Accuracy, na.rm = TRUE), mKappa = mean(Kappa, na.rm = TRUE),
                         sdAccuracy = sd(Accuracy, na.rm = TRUE), sdKappa = sd(Kappa, na.rm = TRUE)))
}
###################################################################

df <- read.csv("D:\\Programming\\DA\\lab 5\\data.csv",nrows = 208)
df[is.na(df)] <- 0
xdf=cbind(as.data.frame(df),classes)

twoClassColor <- brewer.pal(3,'Set1')[1:2]
names(twoClassColor) <- c('Class1','Class2')
ggplot(data = xdf,aes(x = ODI , y = TEST )) + 
  geom_point(aes(color = classes), size = 6, alpha = .5) +
  scale_colour_manual(name = 'classes', values = twoClassColor) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

nbp <- 250;
PredA <- seq(min(xdf$ODI), max(xdf$ODI), length = nbp)
PredB <- seq(min(xdf$TEST), max(xdf$TEST), length = nbp)
Grid <- expand.grid(ODI = PredA, TEST = PredB)

PlotGrid <- function(pred,title) {
  surf <- (ggplot(data = xdf, aes(x = ODI, y = TEST, 
                                       color = classes)) +
             geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
             scale_fill_manual(name = 'classes', values = twoClassColor) +
             ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
             scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  pts <- (ggplot(data = xdf, aes(x = ODI, y = TEST,  
                                      color = classes)) +
            geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                         color = "red", breaks = c(1.5)) +
            geom_point(size = 4, alpha = .5) + 
            ggtitle("Decision boundary") +
            theme(legend.text = element_text(size = 10)) +
            scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  grid.arrange(surf, pts, top = textGrob(title, gp = gpar(fontsize = 20)), ncol = 2)
}

library("caret")
V <- 10
T <- 4
TrControl <- trainControl(method = "repeatedcv",
                          number = V,
                          repeats = T)

Seed <- 345
ErrsCaret <- function(Model, Name) {
  Errs <- data.frame(t(postResample(predict(Model, newdata = xdf), xdf[["classes"]])),
                     Resample = "None", model = Name)
  rbind(Errs, data.frame(Model$resample, model = Name))
}

Errs <- data.frame()
CaretLearnAndDisplay <- function (Errs, Name, Formula, Method, ...) {
  set.seed(Seed)
  Model <- train(as.formula(Formula), data = xdf, method = Method, trControl = TrControl, ...)
  Pred <- predict(Model, newdata = Grid)
  PlotGrid(Pred, Name)
  Errs <- rbind(Errs, ErrsCaret(Model, Name))
}
Errs <- CaretLearnAndDisplay(Errs, "Linear Discrimant Analysis", "classes ~ .", "lda")
Errs <- CaretLearnAndDisplay(Errs, "Quadratic Discrimant Analysis", "classes ~ . ", "qda")
Errs <- CaretLearnAndDisplay(Errs, "Logistic", "classes ~ .", "glm")

library(rpart.plot)
Tree <- train(classes ~ ., data = xdf, method = "rpart", control = rpart::rpart.control(minsplit = 5, cp = 0),
              tuneGrid = data.frame(cp = .02), trControl = TrControl)
Tree$finalModel
rpart.plot(Tree$finalModel)
