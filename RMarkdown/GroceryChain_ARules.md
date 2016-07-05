Association Rule Mining- Market Basket Analysis
================

``` r
#Import arules package
library('arules')
```

    ## Warning: package 'arules' was built under R version 3.2.5

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
#Read the dataset
setwd("/users/vsharipriya/Documents/RDirectory/RPractice")
getwd()
```

    ## [1] "/Users/vsharipriya/Documents/RDirectory/RPractice"

``` r
data.arules = read.table("D01.txt", 
                   sep=";", 
                   col.names=c("Date", "Cust_ID",   "Age","Residence","Product_Class","Product_ID", "Amount",   "Asset",    "Sales_Price"),
                   fill=FALSE, 
                   strip.white=TRUE,skip=1)
```

``` r
# Exploratory Analysis
str(data.arules)
```

    ## 'data.frame':    216864 obs. of  9 variables:
    ##  $ Date         : Factor w/ 31 levels "2001-01-01 00:00:00",..: 1 1 1 1 1 1 2 2 2 3 ...
    ##  $ Cust_ID      : int  141833 1376753 1603071 1738667 2141497 1868685 1101270 1754698 1027365 956710 ...
    ##  $ Age          : Factor w/ 11 levels "A","B","C","D",..: 6 5 5 5 1 10 4 8 6 5 ...
    ##  $ Residence    : Factor w/ 8 levels "A","B","C","D",..: 6 5 7 6 2 5 3 1 3 5 ...
    ##  $ Product_Class: int  130207 110217 100201 530105 320407 110109 730303 560402 530404 500303 ...
    ##  $ Product_ID   : num  4.71e+12 4.71e+12 4.71e+12 4.71e+12 4.71e+12 ...
    ##  $ Amount       : int  2 1 1 1 1 1 1 1 1 1 ...
    ##  $ Asset        : int  44 150 35 94 100 144 740 676 170 36 ...
    ##  $ Sales_Price  : int  52 129 39 119 159 190 969 849 219 59 ...

``` r
#Missing values
missing.values <- sapply(data.arules, function(x)(sum(is.na(x))/dim(data.arules)[1]*100 ))
print (missing.values)
```

    ##          Date       Cust_ID           Age     Residence Product_Class 
    ##             0             0             0             0             0 
    ##    Product_ID        Amount         Asset   Sales_Price 
    ##             0             0             0             0

``` r
#Check number of unique products and customers
unique.values <- rapply(data.arules, function(x) length(unique(x)))
print (unique.values)
```

    ##          Date       Cust_ID           Age     Residence Product_Class 
    ##            31         16578            11             8          1840 
    ##    Product_ID        Amount         Asset   Sales_Price 
    ##         17860            53          1301          1521

``` r
#Count of customers
customer_count <- table(data.arules$Cust_ID)
```

``` r
#Data manipulation
#stripping time from date
data.arules$Date <- strptime(data.arules$Date, "%F%t%T")
data.arules <- data.arules[order(data.arules$Cust_ID, data.arules$Date),] 
# create a variable, Transaction ID using the Date and the CustomerID
transactionID <- paste(data.arules$Date, data.arules$Cust_ID)
trans_single <- data.frame(transactionID = transactionID, productID= data.arules$Product_ID)
#To get unique transactions
trans_single<-unique(trans_single)
# converted into transaction format
transData <- as(split(trans_single[,"productID"], trans_single[,"transactionID"]),"transactions")
```

``` r
#Association rule mining
#Mine Frequent Item sets with support greater than 0.005
rules.all<-apriori(transData, parameter=list(target="frequent", supp=0.005))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport support minlen maxlen
    ##          NA    0.1    1 none FALSE            TRUE   0.005      1     10
    ##             target   ext
    ##  frequent itemsets FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 149 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[17860 item(s), 29901 transaction(s)] done [0.04s].
    ## sorting and recoding items ... [152 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.01s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [166 set(s)] done [0.00s].
    ## creating S4 object  ... done [0.01s].

``` r
#Mine Association rules with support greater than 0.005, confidence greater than 0.09
#Thresholds are set to prune the rules based on number of rules generated.
rules.all<-apriori(transData, parameter=list(target="rules", supp=0.003, conf=0.09))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport support minlen maxlen
    ##        0.09    0.1    1 none FALSE            TRUE   0.003      1     10
    ##  target   ext
    ##   rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 89 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[17860 item(s), 29901 transaction(s)] done [0.05s].
    ## sorting and recoding items ... [350 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.01s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [84 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.01s].

``` r
#Pruning redundant rules
subset.matrix<-is.subset(rules.all,rules.all)
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)
```

    ##                             {4710030346059,4710030346103} 
    ##                                                         2 
    ##                               {719859796117,719859796124} 
    ##                                                         4 
    ##                             {4710011401135,4710011401142} 
    ##                                                         6 
    ##                             {4710011401142,4710011405133} 
    ##                                                         8 
    ##                             {4710011401128,4710011401142} 
    ##                                                        10 
    ##                             {4710085120703,4710085120710} 
    ##                                                        12 
    ##                             {4710085120093,4710085172702} 
    ##                                                        14 
    ##                             {4710085172696,4710085172702} 
    ##                                                        16 
    ##                             {4710085120628,4710085172702} 
    ##                                                        18 
    ##                             {4710254049323,4710254049521} 
    ##                                                        20 
    ##                             {4713754987607,4713754987614} 
    ##                                                        22 
    ##                              {723125485032,7231254880206} 
    ##                                                        24 
    ##                             {4710011401135,4710011409056} 
    ##                                                        26 
    ##                             {4710011406123,4710011409056} 
    ##                                                        28 
    ##                             {4710011405133,4710011409056} 
    ##                                                        30 
    ##                             {4710011401128,4710011409056} 
    ##                                                        32 
    ##                             {4710018004605,4710154015206} 
    ##                                                        34 
    ##                             {4710011401135,4710011406123} 
    ##                                                        36 
    ##                             {4710011401135,4710011405133} 
    ##                                                        38 
    ##                             {4710011401128,4710011401135} 
    ##                                                        40 
    ##                             {4710085120093,4710085172696} 
    ##                                                        42 
    ##                             {4710085120093,4710085120628} 
    ##                                                        44 
    ##                             {4710085120628,4710085172696} 
    ##                                                        46 
    ##                             {4710011405133,4710011406123} 
    ##                                                        48 
    ##                             {4710011401128,4710011406123} 
    ##                                                        50 
    ##                             {4710011401128,4710011405133} 
    ##                                                        52 
    ##                             {4710018004605,4710018004704} 
    ##                                                        54 
    ##                             {4712425010255,4712425010712} 
    ##                                                        56 
    ##               {4710011401135,4710011405133,4710011409056} 
    ##                                                        57 
    ##               {4710011401135,4710011405133,4710011409056} 
    ##                                                        58 
    ##               {4710011401135,4710011405133,4710011409056} 
    ##                                                        59 
    ##               {4710011401128,4710011401135,4710011409056} 
    ##                                                        60 
    ##               {4710011401128,4710011401135,4710011409056} 
    ##                                                        61 
    ##               {4710011401128,4710011401135,4710011409056} 
    ##                                                        62 
    ##               {4710011401128,4710011406123,4710011409056} 
    ##                                                        63 
    ##               {4710011401128,4710011406123,4710011409056} 
    ##                                                        64 
    ##               {4710011401128,4710011406123,4710011409056} 
    ##                                                        65 
    ##               {4710011401128,4710011405133,4710011409056} 
    ##                                                        66 
    ##               {4710011401128,4710011405133,4710011409056} 
    ##                                                        67 
    ##               {4710011401128,4710011405133,4710011409056} 
    ##                                                        68 
    ##               {4710011401128,4710011401135,4710011406123} 
    ##                                                        69 
    ##               {4710011401128,4710011401135,4710011406123} 
    ##                                                        70 
    ##               {4710011401128,4710011401135,4710011406123} 
    ##                                                        71 
    ##               {4710011401128,4710011401135,4710011405133} 
    ##                                                        72 
    ##               {4710011401128,4710011401135,4710011405133} 
    ##                                                        73 
    ##               {4710011401128,4710011401135,4710011405133} 
    ##                                                        74 
    ##               {4710085120093,4710085120628,4710085172696} 
    ##                                                        75 
    ##               {4710085120093,4710085120628,4710085172696} 
    ##                                                        76 
    ##               {4710085120093,4710085120628,4710085172696} 
    ##                                                        77 
    ##               {4710011401128,4710011405133,4710011406123} 
    ##                                                        78 
    ##               {4710011401128,4710011405133,4710011406123} 
    ##                                                        79 
    ##               {4710011401128,4710011405133,4710011406123} 
    ##                                                        80 
    ## {4710011401128,4710011401135,4710011405133,4710011409056} 
    ##                                                        81 
    ## {4710011401128,4710011401135,4710011405133,4710011409056} 
    ##                                                        82 
    ## {4710011401128,4710011401135,4710011405133,4710011409056} 
    ##                                                        83 
    ## {4710011401128,4710011401135,4710011405133,4710011409056} 
    ##                                                        84

``` r
rules.pruned<-rules.all[!redundant]
rules.pruned<-sort(rules.pruned , by="lift")
#Identifying top 30 rules , ordered by lift
rules.pruned= head(sort(rules.pruned, by="lift"),30)
inspect(rules.pruned)
```

    ##    lhs                rhs             support     confidence lift      
    ## 3  {719859796124}  => {719859796117}  0.003377813 0.8080000  104.588779
    ## 1  {4710030346103} => {4710030346059} 0.003545032 0.6272189   91.485236
    ## 11 {4710085120710} => {4710085120703} 0.004581787 0.6313364   71.236188
    ## 23 {7231254880206} => {723125485032}  0.003812582 0.4789916   52.271269
    ## 25 {4710011409056} => {4710011401135} 0.006019866 0.5787781   46.773095
    ## 5  {4710011401142} => {4710011401135} 0.003444701 0.5786517   46.762876
    ## 7  {4710011401142} => {4710011405133} 0.003244039 0.5449438   40.634327
    ## 29 {4710011409056} => {4710011405133} 0.005651985 0.5434084   40.519834
    ## 37 {4710011401135} => {4710011405133} 0.006588408 0.5324324   39.701402
    ## 39 {4710011401135} => {4710011401128} 0.009564897 0.7729730   37.520560
    ## 13 {4710085172702} => {4710085120093} 0.003578476 0.4908257   36.782403
    ## 15 {4710085172702} => {4710085172696} 0.003645363 0.5000000   36.643382
    ## 31 {4710011409056} => {4710011401128} 0.007558276 0.7266881   35.273865
    ## 27 {4710011409056} => {4710011406123} 0.004782449 0.4598071   35.253055
    ## 21 {4713754987614} => {4713754987607} 0.003110264 0.3357401   35.101272
    ## 19 {4710254049323} => {4710254049521} 0.004648674 0.5245283   34.470156
    ## 41 {4710085120093} => {4710085172696} 0.006187084 0.4636591   33.980079
    ## 51 {4710011405133} => {4710011401128} 0.009297348 0.6932668   33.651577
    ## 47 {4710011406123} => {4710011405133} 0.005785760 0.4435897   33.076750
    ## 35 {4710011401135} => {4710011406123} 0.005250661 0.4243243   32.532620
    ## 17 {4710085172702} => {4710085120628} 0.004615230 0.6330275   32.190741
    ## 9  {4710011401142} => {4710011401128} 0.003912913 0.6573034   31.905890
    ## 53 {4710018004704} => {4710018004605} 0.008294037 0.6184539   30.515493
    ## 43 {4710085120093} => {4710085120628} 0.007758938 0.5814536   29.568104
    ## 49 {4710011406123} => {4710011401128} 0.007892713 0.6051282   29.373277
    ## 45 {4710085172696} => {4710085120628} 0.007725494 0.5661765   28.791229
    ## 33 {4710154015206} => {4710018004605} 0.003177151 0.2567568   12.668785
    ## 55 {4712425010255} => {4712425010712} 0.003244039 0.1567044    5.532015

    ## Warning: package 'arulesViz' was built under R version 3.2.5

    ## Loading required package: grid

![](GroceryChain_ARules_files/figure-markdown_github/unnamed-chunk-5-1.png)![](GroceryChain_ARules_files/figure-markdown_github/unnamed-chunk-5-2.png)
