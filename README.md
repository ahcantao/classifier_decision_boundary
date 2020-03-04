# Machine Learning: Classifier Decision Boundary
An R script which plots the decision boundaries of the machine learning algorithms.

### Algorithms and parameters
 * K Nearest Neighbors (KNN) for k = {1, 3, 5, 10, 30}
 * Support Vector Machine (SVM) for kernels = {linear, polynomial, radial, sigmoid}
 * Naive Bayes
 * Tree-based:
   * C.50, CART and Random Forest
 * Artificial Neural Network (ANN) for neurons = {1, 10, 100, 500, 1000}
 * Artificial Neural Network (ANN) for max_iterations = {10, 100, 500, 1000, 10000}

### Datasets
The algorithms above are applied to a collection of artificial datasets (machine learning benchmark problems), available on the package '[mlbench](https://cran.r-project.org/web/packages/mlbench/mlbench.pdf)'.
 
### Packages
This script will make use of the following packages:
 * C50
 * caret
 * e1071
 * ggplot2
 * grid
 * gridExtra
 * mlbench
 * mlogit
 * neuralnet
 * nnet
 * randomForest
 * rpart

There is no need to manually pre-install the packages, as there is a function (check below) which checks if you already have the package, otherwise it intalls during the first run.

```
install_packages <- function(packs) {
  for (pack in packs){
    if (!pack %in% installed.packages()) install.packages(pack)
  }
}
install_packages(packs=c("ggplot2","grid","gridExtra","mlbench","caret","e1071",
                        "C50","rpart","randomForest","nnet","neuralnet","mlogit"))
```

## Authors
* **Adriano Henrique Cantão** - [ahCantao](https://github.com/ahcantao)
* **José Augusto Baranauskas**
