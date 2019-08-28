#Authors: Adriano Henrique Cantão and José Augusto Baranauskas
#August 2019

#TO-DO: boundary(models, datasetName, inducerName, k, setFiletype)
#For teaching: User manually calls the function passing the model, dataset name and its libraries
#load or install libraries, then execute.

boundary <- function(models, datasetName, inducerName, k=1, setFiletype="pdf"){ 
  setFilename <- gsub("\\([\\w\\d\\s\\=\\.,]*\\)", "", datasetName, perl=TRUE, ignore.case=TRUE)
  setFilename <- paste( inducerName, ".", setFilename, ".", setFiletype, sep="")
  
  print( paste("Dataset.: ",datasetName, sep="") )
  print( paste("Filename: ",setFilename, sep="") )
  cat("\n")
  
  p <- eval( parse( text=datasetName ) )
  data <- data.frame(p$x[,1], p$x[,2], p$class)
  names(data) <- c("x1","x2","class")
  
  x1_min <- min(p$x[,1])-0.2
  x1_max <- max(p$x[,1])+0.2
  x2_min <- min(p$x[,2])-0.2
  x2_max <- max(p$x[,2])+0.2
  
  title <- datasetName
  
  #get the dataset without any model
  plotList <- list()
  plotList[[length(plotList)+1]] <- 
    ggplot(data) + geom_point(aes(x=x1, y=x2, color = as.character(class)), size = 1) + theme_bw(base_size = 15) +
    xlim(x1_min, x1_max) + ylim(x2_min, x2_max) +
    ggtitle( title ) +
    coord_fixed(ratio = 0.8) +
    theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none', 
          plot.title = element_text(size = 8, face = "bold", hjust = 0.5, color = "brown") )
  
  #plots dataset and decision boundary
  hs <- 0.05
  grid <- as.matrix(expand.grid(seq(x1_min, x1_max, by = hs), seq(x2_min, x2_max, by = hs)))
  grid.df <- as.data.frame(grid)
  colnames(grid.df) <- colnames(data[,1:2])
  Z <- NULL
  require(caret)
  i = 0
  for(mod in models){
    i = i+1
    model <- eval( parse( text=mod ) )
    plotTitle <- paste(inducerName, k[i])
    Z <- predict(model, grid.df, type = "class")
    
    plotList[[length(plotList)+1]] <- 
      ggplot()+
      geom_tile(aes_string(x = grid[,1],y = grid[,2],fill= Z ), alpha = 0.3, show.legend = F)+ 
      geom_point(data = data, aes(x=x1, y=x2, color = as.character(class)), size = 1) + theme_bw(base_size = 15) +
      ggtitle( plotTitle ) +
      coord_fixed(ratio = 0.8) + 
      theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none', 
            plot.title = element_text(size = 8, face = "bold", hjust = 0.5, color = "brown") )
  }
  #arrange the plots side-by-side
  grid.arrange(grobs=plotList, ncol=length(plotList))
  #save to file
  ggsave(filename = setFilename, 
         plot = arrangeGrob( grobs=plotList, ncol=length(plotList)),
         path = outputPath,
         width = 297, 
         height = 210, 
         units = "mm")
  #return (plotList)
}
#-----------------------------------------------------------------------
outputPath = "M:/Estágio PAE/201902 - IA/Boundaries/"
setFiletype <- "pdf"
require(mlbench)
require(ggplot2)
require(caret)
require(lattice)
require(gridExtra)
#datasetNames <- c("mlbench.2dnormals(n=3000, cl=2)",
#                 "mlbench.cassini(n=5000)")

datasetNames <- c("mlbench.2dnormals(n=3000, cl=2)",
                  "mlbench.cassini(n=5000)",
                  "mlbench.hypercube(n=3000, d=2, sd=0.1)",
                  "mlbench.circle(n=3000, d=2)",
                  "mlbench.ringnorm(n=3000, d=2)",
                  "mlbench.shapes(n=3000)",
                  "mlbench.simplex(n = 1000, d = 3, sides = 1, sd = 0.1, center=TRUE)",
                  "mlbench.smiley(n=1000, sd1 = 0.1, sd2 = 0.05)",
                  "mlbench.spirals(n=1000,cycles=1,sd=0.05)",
                  "mlbench.spirals(n=1000,cycles=3,sd=0.05)",
                  "mlbench.threenorm(n=3000, d=2)",
                  "mlbench.twonorm(n=1000, d=2)",
                  "mlbench.xor(n=3000, d=2)")

for(datasetName in datasetNames){
  #----------------------------------------------------------------
  # KNN
  require(caret)
  inducerName <- "KNN"
  k = c(1,3,5,10)  #k[i]
  models <- c( "knn3(class ~ ., data=data, k = k[1])",
               "knn3(class ~ ., data=data, k = k[2])",
               "knn3(class ~ ., data=data, k = k[3])",
               "knn3(class ~ ., data=data, k = k[4])")
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
  k = c("C.50","CART", "Random Forest")
  models <- c("C5.0(class ~ ., data=data)",
              "rpart(class ~ ., data=data)",
              "randomForest(class ~ ., data=data)")
  boundary(models, datasetName, inducerName, k, setFiletype)
  
  #----------------------------------------------------------------
  #Naive Bayes
  require(e1071)
  inducerName <- "Naive Bayes"
  models <- "naiveBayes(class ~ ., data=data)"
  boundary(models, datasetName, inducerName, k="", setFiletype)
}

#--------NOT-WORKING---------------------------------------------
# Linear Discriminant Analysis
require(MASS)
inducerName <- "Linear Discriminant Analysis (LDA)"
models <- "lda(class ~ ., data=data)"

plotList = boundary(models, datasetName, inducerName, k="", setFiletype)
grid.arrange( grobs=plotList, ncol=length(plotList))

#--------NOT-WORKING---------------------------------------------
#Neural Network
require(nnet)
inducerName <- "NN (1)"
models <- "nnet(class ~ ., data=data, size = 1, maxit = 1000, trace = FALSE)"

plotList = boundary(models, datasetName, inducerName, k="", setFiletype)
grid.arrange( grobs=plotList, ncol=length(plotList))
