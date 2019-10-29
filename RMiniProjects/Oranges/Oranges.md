---
title: "Orange Trees"
author: "Olivia Leeson"
output: 
  html_document: 
    keep_md: TRUE
---

#### Find the mean and median of orange tree trunk circumferences for different sizes of tree.

```r
#load and attach Orange dataset
Orange <- Orange
attach(Orange)

#Find mean circumference for each Tree size
round(tapply(circumference, Tree, mean), 2)
```

```
##      3      1      5      2      4 
##  94.00  99.57 111.14 135.29 139.29
```

```r
#Find standard deviation of circumferences for each Tree size
round(tapply(circumference, Tree, sd), 2)
```

```
##     3     1     5     2     4 
## 42.98 43.29 58.86 66.32 71.90
```

#### Scatter plot of trunk circumferences by tree age.

```r
#Create plot of trunk circumferences against tree age

plot( age,circumference, pch=c(1:5)[unclass(Tree)], xlab = "Age in Days", ylab = "Circumference in mm",  main = "Circumference given Age by Tree", col=c("red","green3","blue", "pink3", "orange")[unclass(Tree)] )  # different symbol
legend("bottomright", legend=levels(Tree) , pch=c(1:5), col=c("red","green3","blue", "pink", "orange"))
```

![](Oranges_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Trunk circumferences boxplot.

```r
#Boxplot of trunk circumferences against tree

boxplot(circumference~Tree, main=toupper("Circumference by Tree type"), font.main=3, cex.main=1.2, xlab="Tree", ylab="Circumference in mm", font.lab=3, col="orange")
```

![](Oranges_files/figure-html/unnamed-chunk-3-1.png)<!-- -->