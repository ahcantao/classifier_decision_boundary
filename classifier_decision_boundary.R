#Developed by: Adriano Henrique Cantão & José Augusto Baranauskas
#First released on August, 2019

#Installing needed packages - if not installed already
install_packages <- function(packs) {
  for (pack in packs){
    if (!pack %in% installed.packages()) install.packages(pack)
  }
}
install_packages(packs=c("ggplot2","grid","gridExtra","mlbench","caret","e1071",
                        "C50","rpart","randomForest","nnet","neuralnet","mlogit"))

#function to get plot ids to arrange the plotting order in grid.arrange()
select_grobs <- function(lay) {
  id <- unique(c(t(lay))) 
  id[!is.na(id)]
} 

#function to design the plot layout
choose_layout <- function(size){
  if(size == 2){
    hlay <- rbind(c(1, 2))
  }else if(size == 3){
    hlay <- rbind(c(1, NA),
                  c(2, 3))
  }else if(size == 4){
    hlay <- rbind(c(1, 2),
                  c(3, 4))
  }else if(size == 5){
    hlay <- rbind(c( 1,2,3),
                  c(NA,4,5))
  }else if(size == 6){
    hlay <- rbind(c(1,2,3),
                  c(4,5,6))
  }else if(size == 7){
    hlay <- rbind(c( 1,2,3,4),
                  c(NA,5,6,7))
  }else if(size == 8){
    hlay <- rbind(c(1,2,3,4),
                  c(5,6,7,8))
  }else if(size == 9){
    hlay <- rbind(c( 1,2,3,4,5),
                  c(NA,6,7,8,9))
  }else if(size == 10){
    hlay <- rbind(c(1,2,3,4,5 ),
                  c(6,7,8,9,10))
  }else {
    print("It is configured only up to 10 plots in a single image. If you need more, add it right before this message.")
  }
  return(hlay)
}

boundary <- function(models, dataset_name, inducer_name, k=1){ 
  set.seed(2020)
  file_name <- gsub("\\([\\w\\d\\s\\=\\.,]*\\)", "", dataset_name, perl=TRUE, ignore.case=TRUE)
  file_name <- paste( file_name, ".", inducer_name, ".", file_extension, sep="")

  #uncomment bellow to see de details while the script is running 
#   print( paste("Dataset.: ",dataset_name, sep="") )
#   print( paste("Filename: ",file_name, sep="") )
#   cat("\n")
  
  #creating and preparing the dataset
  p <- eval(parse(text = dataset_name))
  data <- data.frame(p$x[,1], p$x[,2], p$class)
  names(data) <- c("x1","x2","class")
  
  #splittng data into train(data) and test
  sample_index <- sample(seq_len(nrow(data)), size = floor(nrow(data) / 2), replace = FALSE)
  test <- data[-sample_index, ]
  data <- data[sample_index, ]
  
  plot_title <- "Dataset"
  #replacing the text 'samples' by the variable value and save the whole string as title
  plot_subtitle <- gsub("samples", samples/2, dataset_name)
  
  #preparing background grid area
  x1_min <- min(p$x[,1])-0.2
  x1_max <- max(p$x[,1])+0.2
  x2_min <- min(p$x[,2])-0.2
  x2_max <- max(p$x[,2])+0.2
  
  #all grids will have ~150 steps. *-1 to make the number positive
  grid_step <- ((x1_min - x1_max) / 150 ) * -1
  #hs <- 0.05 #changed to 'grid_step' as a variable...
  grid <- as.matrix(expand.grid(seq(x1_min, x1_max, by = grid_step), seq(x2_min, x2_max, by = grid_step)))
  grid.df <- as.data.frame(grid)
  colnames(grid.df) <- colnames(data[,1:2])
  
  #saving the raw dataset - without any model
  plots <- list()
  plots[[length(plots)+1]] <- 
    ggplot(data) + 
    geom_point(aes(x=x1, y=x2, color = as.character(class)), size = 1) + 
    theme_bw(base_size = 15) +
    xlim(x1_min, x1_max) + 
    ylim(x2_min, x2_max) +
    labs(title = plot_title, subtitle = plot_subtitle) +
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
    model <- eval(parse(text=mod))
    #getting accuracy in train data
    prediction_train <- predict(model, data[,1:2],type = "class")
    conf_matrix_train <- table(prediction_train, data[,3])
    accuracy_train <- (sum(diag(conf_matrix_train)) / sum(conf_matrix_train)) * 100
    #getting accuracy in test data
    prediction_test <- predict(model, test[,1:2],type = "class")
    conf_matrix_test <- table(prediction_test, test[,3])
    accuracy_test <- (sum(diag(conf_matrix_test)) / sum(conf_matrix_test)) * 100
    plot_title <- paste0(inducer_name, k[i])
    plot_subtitle <- paste("training accuracy:",round(accuracy_train,2),"% - test accuracy:",round(accuracy_test,2),"%")
    Z <- predict(model, grid.df, type = "class")
      
    plots[[length(plots)+1]] <- 
      ggplot()+
      geom_raster(aes_string(x = grid[,1],y = grid[,2],fill= as.factor(Z) ), alpha = 0.3, show.legend = F)+ 
      geom_point(data = data, aes(x=x1, y=x2, color = as.character(class)), size = 1) + 
      theme_bw(base_size = 15) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      coord_fixed(ratio = 0.8) + 
      theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none', 
            plot.title  = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -1, color = "black"),
            plot.subtitle=element_text(size = 10, face ="plain", hjust = 0.1, vjust = -1, color = "brown"))
  }
  #arrange the plots according to the number of plots
  hlay <- choose_layout(length(plots))
  #arranging many plots inside a single one - the line bellow print the output 
  grid.arrange(grobs=plots[select_grobs(hlay)], layout_matrix=hlay)
  #saving to file
  if(save_plot_to_file == TRUE){
    ggsave(filename = file_name,
           plot = grid.arrange(grobs=plots[select_grobs(hlay)], layout_matrix=hlay),
           path = outputPath,
           width = 297,
           height = 210,
           units = "mm")
  }else{
     print("NOT SAVING")
  }
}

get_boundaries <- function(dataset_names,
                           knn,
                           svm,
                           trees,
                           nb,
                           ann_neu,
                           ann_its,
                           ann_fit=FALSE){
  require(mlbench)
  require(ggplot2)
  require(caret)
  require(lattice)
  require(gridExtra)
  
  for(dataset_name in dataset_names){
    if(knn == TRUE){
      require(caret)
      inducer_name <- "KNN "
      k <- c(1, 3, 5, 10, 30)  #k[i]
      models <- c( "knn3(class ~ ., data=data, k = k[1])",
                   "knn3(class ~ ., data=data, k = k[2])",
                   "knn3(class ~ ., data=data, k = k[3])",
                   "knn3(class ~ ., data=data, k = k[4])",
                   "knn3(class ~ ., data=data, k = k[5])")
      boundary(models, dataset_name, inducer_name, k)
    }
    #----------------------------------------------------------------
    if(svm == TRUE){
      require(e1071) #cart
      inducer_name <- "SVM "
      k <- c("linear", "Polynomial", "Radial", "Sigmoid")
      models <- c( "svm(class ~ ., data=data, kernel='linear')",
                   "svm(class ~ ., data=data, kernel='polynomial')",
                   "svm(class ~ ., data=data, kernel='radial')",
                   "svm(class ~ ., data=data, kernel='sigmoid')")
      boundary(models, dataset_name, inducer_name, k)
    }
    #----------------------------------------------------------------
    if(trees == TRUE){
      require(C50)
      require(rpart)
      library(randomForest)
      inducer_name <- "Trees "
      k <- c("C.50","CART", "Random Forest 3 trees", "Random Forest 128 trees")
      models <- c("C5.0(class ~ ., data=data)",
                  "rpart(class ~ ., data=data)",
                  "randomForest(class ~ ., data=data, ntree=3)",
                  "randomForest(class ~ ., data=data, ntree=128)")
      boundary(models, dataset_name, inducer_name, k)
    }
    #----------------------------------------------------------------
    if(nb == TRUE){
      require(e1071)
      inducer_name <- "Naive Bayes"
      models <- "naiveBayes(class ~ ., data=data)"
      boundary(models, dataset_name, inducer_name, k = "")
    }
    #----------------------------------------------------------------
    if(ann_neu == TRUE){ #Artificial Neural Network: [qty neurons]
      require(nnet)
      inducer_name <- "ANN_iters=100_neurons="
      k <- c(1, 10, 100, 500, 1000)
      models <- c("nnet(class ~ ., data=data, size = k[1], maxit = 100, MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = k[2], maxit = 100, MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = k[3], maxit = 100, MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = k[4], maxit = 100, MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = k[5], maxit = 100, MaxNWts = 10000)")
      boundary(models, dataset_name, inducer_name, k=k)
    }
    #----------------------------------------------------------------
    if(ann_its == TRUE){ #Artificial Neural Network: [qty iterations]
      require(nnet)
      inducer_name <- "ANN_neurons=10_iters="
      k <- c(10, 100, 500, 1000, 10000)
      models <- c("nnet(class ~ ., data=data, size = 10, maxit = k[1], MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = 10, maxit = k[2], MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = 10, maxit = k[3], MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = 10, maxit = k[4], MaxNWts = 10000)",
                  "nnet(class ~ ., data=data, size = 10, maxit = k[5], MaxNWts = 10000)")
      boundary(models, dataset_name, inducer_name, k=k)
    }
#-----------------------not-working-----------------------------------
    if(ann_fit == TRUE){ #Artificial Neural Network: [fitting]
      print("ENTREI")
#       require(nnet)
#       inducer_name <- "ANN_neurons=100_fitting="
#       k <- c("logistic", "softmax", "linout", "censored", "entropy")
#       models <- c("nnet(class ~ ., data=data, size = 100, maxit = 1000)",
#                   "nnet(class ~ ., data=data, size = 100, maxit = 1000, softmax  = TRUE)",
#                   "nnet(class ~ ., data=data, size = 100, maxit = 1000, linout   = TRUE)",
#                   "nnet(class ~ ., data=data, size = 100, maxit = 1000, censored = TRUE)",
#                   "nnet(class ~ ., data=data, size = 100, maxit = 1000, entropy  = TRUE)")
#       boundary(models, dataset_name, inducer_name, k=k)
    }
  }
}

#Setting the parameters
#use a low number of samples to have a better view of the boundaries
samples     <- 200
outputPath  <- ""
save_plot_to_file <- FALSE #if false, will plot, but not save
file_extension <- "pdf"

#High values of standard deviation (sd) leads to very mixed classes
dataset_names <- c("mlbench.2dnormals(n=samples, cl=2)",
                  "mlbench.cassini(n=samples)",
                  "mlbench.hypercube(n=samples, d=2, sd=0.3)",
                  "mlbench.circle(n=samples, d=2)",
                  "mlbench.ringnorm(n=samples, d=2)",
                  "mlbench.shapes(n=samples)",
                  "mlbench.simplex(n=samples, d=2, sd = 0.3)",
                  "mlbench.smiley(n=samples, sd1 = 0.1, sd2 = 0.3)",
                  #"mlbench.spirals(n=samples, cycles=1, sd=0.0)",
                  "mlbench.spirals(n=samples, cycles=3, sd=0.0)",
                  "mlbench.threenorm(n=samples, d=2)",
                  "mlbench.twonorm(n=samples, d=2)",
                  "mlbench.xor(n=samples, d=2)")

#double samples then split in half for testing accuracy
samples <- samples * 2

#Running the script
get_boundaries(dataset_names,
               knn     <- TRUE,  #K-nearest neighbors
               svm     <- TRUE,  #Support-vector machine
               trees   <- TRUE,  #tree-based
               nb      <- TRUE,  #Naive Bayes
               ann_neu <- TRUE,  #Artificial neural network increasing neurons
               ann_its <- TRUE,  #Artificial neural network increasing iterations
               ann_fit <- FALSE) #Artificial neural network changing functions