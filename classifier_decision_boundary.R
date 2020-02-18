#Authors: Adriano Henrique Cantão and José Augusto Baranauskas
#First version date: August 2019

#installing libraries
installPackages <- function(packs) {
  for (pack in packs){
    if (!pack %in% installed.packages()) install.packages(pack)
  }
}
installPackages(packs=c("ggplot2","grid","gridExtra","mlbench","caret","e1071",
                        "C50","rpart","randomForest","nnet","neuralnet","mlogit"))

#function to get plot ids to arrange the plotting order in grid.arrange()
select_grobs <- function(lay) {
  id <- unique(c(t(lay))) 
  id[!is.na(id)]
} 

boundary <- function(models, datasetName, inducerName, k=1, setFiletype="pdf"){ 
  set.seed(2020)
  setFilename <- gsub("\\([\\w\\d\\s\\=\\.,]*\\)", "", datasetName, perl=TRUE, ignore.case=TRUE)
  setFilename <- paste( setFilename, ".", inducerName, ".", setFiletype, sep="")
  
  print( paste("Dataset.: ",datasetName, sep="") )
  print( paste("Filename: ",setFilename, sep="") )
  cat("\n")
  
  #creating and preparing the dataset
  p <- eval( parse( text=datasetName ) )
  data <- data.frame(p$x[,1], p$x[,2], p$class)
  names(data) <- c("x1","x2","class")
  
  #splittng data into train(data) and test
  sample_index <- sample(seq_len(nrow(data)), size = floor(nrow(data) / 2), replace = FALSE)
  test <- data[-sample_index, ]
  data <- data[sample_index, ]
  
  plotTitle <- "Dataset"
  #replacing the text 'samples' by the variable value and save the whole string as title
  plotSubtitle <- gsub("samples", samples/2, datasetName)
  
  #preparing background grid area
  x1_min <- min(p$x[,1])-0.2
  x1_max <- max(p$x[,1])+0.2
  x2_min <- min(p$x[,2])-0.2
  x2_max <- max(p$x[,2])+0.2
  hs <- 0.05 #change by a variable number...
  grid <- as.matrix(expand.grid(seq(x1_min, x1_max, by = hs), seq(x2_min, x2_max, by = hs)))
  grid.df <- as.data.frame(grid)
  colnames(grid.df) <- colnames(data[,1:2])
  
  #saving the raw dataset - without any model
  plotList <- list()
  plotList[[length(plotList)+1]] <- 
    ggplot(data) + 
    geom_point(aes(x=x1, y=x2, color = as.character(class)), size = 1) + 
    theme_bw(base_size = 15) +
    xlim(x1_min, x1_max) + 
    ylim(x2_min, x2_max) +
    labs(title = plotTitle, subtitle = plotSubtitle) +
    coord_fixed(ratio = 0.8) +
    theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none',
          plot.title  = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -1, color = "black"),
          plot.subtitle=element_text(size = 10, face ="plain", hjust = 0.1, vjust = -1, color = "brown"))
  Z <- NULL
  require(caret)
  i = 0
  for(mod in models){
    i = i+1
    model <- eval( parse( text=mod ) )
    #getting accuracy in train data
    prediction_train <- predict(model, data[,1:2],type = "class")
    conf_matrix_train <- table(prediction_train, data[,3])
    accuracy_train <- (sum(diag(conf_matrix_train)) / sum(conf_matrix_train)) * 100
    #getting accuracy in test data
    prediction_test <- predict(model, test[,1:2],type = "class")
    conf_matrix_test <- table(prediction_test, test[,3])
    accuracy_test <- (sum(diag(conf_matrix_test)) / sum(conf_matrix_test)) * 100
    plotTitle <- paste0(inducerName, k[i])
    plotSubtitle <- paste("training accuracy:",round(accuracy_train,2),"% - test accuracy:",round(accuracy_test,2),"%")
    Z <- predict(model, grid.df, type = "class")
    if(inducerName == "Deep Learning"){
      Z = round(Z[,2])
    }
    plotList[[length(plotList)+1]] <- 
      ggplot()+
      geom_raster(aes_string(x = grid[,1],y = grid[,2],fill= as.factor(Z) ), alpha = 0.3, show.legend = F)+ 
      geom_point(data = data, aes(x=x1, y=x2, color = as.character(class)), size = 1) + 
      theme_bw(base_size = 15) +
      labs(title = plotTitle, subtitle = plotSubtitle) +
      coord_fixed(ratio = 0.8) + 
      theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none', 
            plot.title  = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -1, color = "black"),
            plot.subtitle=element_text(size = 10, face ="plain", hjust = 0.1, vjust = -1, color = "brown"))
  }
  #arrange the plots according to the number of plots
  if(length(plotList) == 2){
    hlay <- rbind(c(1, 2))
  }else if(length(plotList) == 3){
    hlay <- rbind(c(1, NA),
                  c(2, 3))
  }else if(length(plotList) == 4){
    hlay <- rbind(c(1, 2),
                  c(3, 4))
  }else if(length(plotList) == 5){
    hlay <- rbind(c( 1,2,3),
                  c(NA,4,5))
  }else if(length(plotList) == 6){
    hlay <- rbind(c(1,2,3),
                  c(4,5,6))
  }else if(length(plotList) == 7){
    hlay <- rbind(c( 1,2,3,4),
                  c(NA,5,6,7))
  }else if(length(plotList) == 8){
    hlay <- rbind(c(1,2,3,4),
                  c(5,6,7,8))
  }else if(length(plotList) == 9){
    hlay <- rbind(c( 1,2,3,4,5),
                  c(NA,6,7,8,9))
  }else if(length(plotList) == 10){
    hlay <- rbind(c(1,2,3,4,5 ),
                  c(6,7,8,9,10))
  }else {
    print("It is configured only up to 10 plots in a single image. If you need more, add it right before this message.")
  }
  #arranging many plots inside a single one - the line bellow print the output 
  grid.arrange(grobs=plotList[select_grobs(hlay)], layout_matrix=hlay)
  #saving to file
  ggsave(filename = setFilename,
         #plot = arrangeGrob( grobs=plotList, ncol=length(plotList)),
         plot = grid.arrange(grobs=plotList[select_grobs(hlay)], layout_matrix=hlay),
         path = outputPath,
         width = 297,
         height = 210,
         units = "mm")
  #return (plotList)
}
#-----------------------------------------------------------------------
# outputPath = "M:/Estágio PAE/201902 - IA/Boundaries/"
samples <- 200
outputPath = "C:/temp/boundaries/"
setFiletype <- "pdf"
require(mlbench)
require(ggplot2)
require(caret)
require(lattice)
require(gridExtra)

datasetNames <- c("mlbench.2dnormals(n=samples, cl=2)",
                  "mlbench.cassini  (n=samples)",
                  "mlbench.hypercube(n=samples, d=2, sd=0.3)",
                  "mlbench.circle   (n=samples, d=2)",
                  "mlbench.ringnorm (n=samples, d=2)",
                  "mlbench.shapes   (n=samples)",
                  "mlbench.simplex  (n=samples, d=2, sd = 0.1)",
                  "mlbench.smiley   (n=samples, sd1 = 0.1, sd2 = 0.05)",
                  "mlbench.spirals  (n=samples, cycles=1, sd=0.05)",
                  "mlbench.spirals  (n=samples, cycles=3, sd=0.05)",
                  "mlbench.threenorm(n=samples, d=2)",
                  "mlbench.twonorm  (n=samples, d=2)",
                  "mlbench.xor      (n=samples, d=2)")
#double samples then split in half for testing accuracy
samples = 200
samples <- samples * 2
for(datasetName in datasetNames){
  # KNN
  require(caret)
  inducerName <- "KNN"
  k = c(1,3,5,10,30)  #k[i]
  models <- c( "knn3(class ~ ., data=data, k = k[1])",
               "knn3(class ~ ., data=data, k = k[2])",
               "knn3(class ~ ., data=data, k = k[3])",
               "knn3(class ~ ., data=data, k = k[4])",
               "knn3(class ~ ., data=data, k = k[5])")
  boundary(models, datasetName, inducerName, k, setFiletype)
  #---------------------------------------------------------------- 
  # SVM
  require(e1071) #cart
  inducerName <- "SVM"
  k = c("linear","polynomial","radial","sigmoid")
  models <- c( "svm(class ~ ., data=data, kernel='linear')",
               "svm(class ~ ., data=data, kernel='polynomial')",
               "svm(class ~ ., data=data, kernel='radial')",
               "svm(class ~ ., data=data, kernel='sigmoid')")
  boundary(models, datasetName, inducerName, k, setFiletype)
  #----------------------------------------------------------------
  #Trees
  require(C50)
  require(rpart)
  library(randomForest)
  inducerName <- "Trees"
  k = c("C.50","CART", "Random Forest 3 trees", "Random Forest 128 trees")
  models <- c("C5.0(class ~ ., data=data)",
              "rpart(class ~ ., data=data)",
              "randomForest(class ~ ., data=data, ntree=5)",
              "randomForest(class ~ ., data=data, ntree=128)")
  boundary(models, datasetName, inducerName, k, setFiletype)
  #----------------------------------------------------------------
  #Naive Bayes
  require(e1071)
  inducerName <- "Naive Bayes"
  models <- "naiveBayes(class ~ ., data=data)"
  boundary(models, datasetName, inducerName, k="", setFiletype)
  #----------------------------------------------------------------
  # #Artificial Neural Network: [qty neurons]
  require(nnet)
  inducerName <- "ANN; iters=100; neurons="
  k = c(1,10,100,500,1000)
  models <- c("nnet(class ~ ., data=data, size = k[1], maxit = 100, MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = k[2], maxit = 100, MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = k[3], maxit = 100, MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = k[4], maxit = 100, MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = k[5], maxit = 100, MaxNWts = 10000)")
  boundary(models, datasetName, inducerName, k=k, setFiletype)
  #----------------------------------------------------------------
  #Artificial Neural Network: [qty iterations]
  require(nnet)
  inducerName <- "ANN; neurons=10; iters="
  k = c(10,100,500,1000,10000)
  models <- c("nnet(class ~ ., data=data, size = 10, maxit = k[1], MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = 10, maxit = k[2], MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = 10, maxit = k[3], MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = 10, maxit = k[4], MaxNWts = 10000)",
              "nnet(class ~ ., data=data, size = 10, maxit = k[5], MaxNWts = 10000)")
  boundary(models, datasetName, inducerName, k=k, setFiletype)
  #----------------------------------------------------------------
  #Artificial Neural Network: [fitting]
  require(nnet)
  inducerName <- "ANN; n=100; fitting="
  k = c("entropy","linout","censored", "softmax")
  models <- c("nnet(class ~ ., data=data, size = 100, maxit = 1000, entropy  = TRUE)",
              "nnet(class ~ ., data=data, size = 100, maxit = 1000, linout   = TRUE)",
              "nnet(class ~ ., data=data, size = 100, maxit = 1000, censored = TRUE)",
              "nnet(class ~ ., data=data, size = 100, maxit = 1000)")
  boundary(models, datasetName, inducerName, k=k, setFiletype)
  #----------------------------------------------------------------
# #--------NOT-WORKING---------------------------------------------
# # Linear Discriminant Analysis
# require(MASS)
# inducerName <- "Linear Discriminant Analysis (LDA)"
# models <- "lda(class ~ ., data=data)"
# 
# boundary(models, datasetName, inducerName, k="", setFiletype)
# grid.arrange( grobs=plotList, ncol=length(plotList))
# 
}


