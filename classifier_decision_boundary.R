#Authors: Adriano Henrique Cantão and José Augusto Baranauskas
#August 2019

#For teaching: User manually calls the function passing the model, dataset name and its libraries

#installing libraries
installPackages <- function(packs) {
  for (pack in packs){
    if (!pack %in% installed.packages()) install.packages(pack)
  }
}
installPackages(packs=c("ggplot2","grid","gridExtra","mlbench","caret","e1071","C50","rpart","randomForest","nnet","neuralnet"))

#function to get plot ids to arrange the plotting order in grid.arrange()
select_grobs <- function(lay) {
  id <- unique(c(t(lay))) 
  id[!is.na(id)]
} 

boundary <- function(models, datasetName, inducerName, k=1, setFiletype="pdf"){ 
  set.seed(2019)
  setFilename <- gsub("\\([\\w\\d\\s\\=\\.,]*\\)", "", datasetName, perl=TRUE, ignore.case=TRUE)
  #  setFilename <- paste( inducerName, ".", setFilename, ".", setFiletype, sep="")
  setFilename <- paste( setFilename, ".", inducerName, ".", setFiletype, sep="")
  
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
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "brown") )
  
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
    prediction <- predict(model, data[,1:2],type = "class")
    conf_matrix <- table(prediction, data[,3])
    accuracy <- (sum(diag(conf_matrix)) / sum(conf_matrix)) * 100
    plotTitle <- paste(inducerName, k[i], '(acc:',round(accuracy,2),'%)')
    Z <- predict(model, grid.df, type = "class")
    if(inducerName == "Deep Learning"){
      Z = round(Z[,2])
    }
    plotList[[length(plotList)+1]] <- 
      ggplot()+
      geom_tile(aes_string(x = grid[,1],y = grid[,2],fill= as.factor(Z) ), alpha = 0.3, show.legend = F)+ 
      geom_point(data = data, aes(x=x1, y=x2, color = as.character(class)), size = 1) + theme_bw(base_size = 15) +
      ggtitle( plotTitle ) +
      coord_fixed(ratio = 0.8) + 
      theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none', 
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "brown") )
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
  #arrange many plots inside a single one - the line bellow print the output 
  grid.arrange(grobs=plotList[select_grobs(hlay)], layout_matrix=hlay)
  #save to file
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
outputPath = "C:/temp/boundaries/"
setFiletype <- "pdf"
require(mlbench)
require(ggplot2)
require(caret)
require(lattice)
require(gridExtra)

datasetNames <- c("mlbench.2dnormals(n=1000, cl=2)",
                  "mlbench.cassini(n=1000)",
                  "mlbench.hypercube(n=1000, d=2, sd=0.3)",
                  "mlbench.circle(n=1000, d=2)",
                  "mlbench.ringnorm(n=1000, d=2)",
                  "mlbench.shapes(n=1000)",
                  "mlbench.simplex(n = 1000, d = 2, sides = 1, sd = 0.1, center=TRUE)",
                  "mlbench.smiley(n=1000, sd1 = 0.1, sd2 = 0.05)",
                  "mlbench.spirals(n=1000,cycles=1,sd=0.05)",
                  "mlbench.spirals(n=1000,cycles=3,sd=0.05)",
                  "mlbench.threenorm(n=1000, d=2)",
                  "mlbench.twonorm(n=1000, d=2)",
                  "mlbench.xor(n=1000, d=2)")
# datasetNames <- c("mlbench.hypercube(n=200, d=2, sd=0.3)")
#datasetNames <- c("mlbench.simplex(n = 300, d = 2, sd = 0.3)")

#set.seed(2019)
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
  # #Neural Network: [qty neurons]
  require(nnet)
  inducerName <- "ANN"
  k = c("1","5","10", "50", "100")
  models <- c("nnet(class ~ ., data=data, size = 1, maxit = 1000)",
              "nnet(class ~ ., data=data, size = 5, maxit = 1000)",
              "nnet(class ~ ., data=data, size = 10, maxit = 1000)",
              "nnet(class ~ ., data=data, size = 50, maxit = 1000)",
              "nnet(class ~ ., data=data, size = 100, maxit = 1000)")
  boundary(models, datasetName, inducerName, k=k, setFiletype)
  #----------------------------------------------------------------
  #Deep Learning: [neurons on each hidden layer]
  require(neuralnet)
  inducerName <- "Deep Learning"
  set.seed(2019)
  k = c("[3]", "[3,7]", "[3,7,5]", "[3,7,5,2]")
  models <- c("neuralnet(class ~ ., data=data, hidden = c(5))")
              #"neuralnet(class ~ ., data=data, hidden = c(5,7))",
              #"neuralnet(class ~ ., data=data, hidden = c(5,7,7))",
              #"neuralnet(class ~ ., data=data, hidden = c(5,7,7,5))")
  boundary(models, datasetName, inducerName, k=k, setFiletype)
}

# #--------NOT-WORKING---------------------------------------------
# # Linear Discriminant Analysis
# require(MASS)
# inducerName <- "Linear Discriminant Analysis (LDA)"
# models <- "lda(class ~ ., data=data)"
# 
# boundary(models, datasetName, inducerName, k="", setFiletype)
# grid.arrange( grobs=plotList, ncol=length(plotList))
# 
