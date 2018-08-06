
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- using Rscript -e 'library(rmarkdown); rmarkdown::render("README.Rmd")' -->





`pilsner` : **P**atient **I**ndividual **L**evel **S**imulation, **N**on-interacting but **E**fficient, using **R**cpp

This uses `Rcpp` to provide a single function in R:

-   `simulator` : take an cohort and return the final state after a DES run (optionally, data on all events)

This implements a time-to-event discrete event simulation, assuming completely independent individuals. The user specifies the DES event logic in the simple file `src/eventlogic.cpp`, but need not worry about the simulation engine. More details are explained below.

### Installation

Because the simulation is specified in C++, the `Rcpp` R package must be installed:

``` {.r}
install.packages("ggplot2") # needed for the graph
install.packages("Rcpp") #needed to run the models
```

This may require reading the relevant package installation guidance pages and downloading a compiler. This will be system-dependent. On Mac or Linux, a working compiler probably already exists.

### Logic

The function `simulator` takes 6 arguments:

-   `AM` - a column-named matrix of inital age-like quantities (eg age, time-since-event), which will advance with time. 1 row per person
-   `CM` - a column-named matrix of initial continuous person characteristics. 1 row per person
-   `DM` - a column-named matrix of initial integer person characteristics (eg for sex etc). 1 row per person
-   `endtime` - when to stop the simulation
-   `parmzl` - a list of named vectors specifying parameters (either length 1, or length the cohort size)
-   `recording` - a boolean (default `FALSE`) specifying whether to record and return event data (will substantially slow the model down)

The `simulator` function acts by side-effect for speed, i.e. it will alter the supplied data. **NB** this can be dangerous if unwary, eg with repeated simulation. However, it does facilitate onward simulation.

The person objects created in the engine, with have accessible data accessible via the column names (see below). The nature of `parmzl` allows either PSA or individualized simulation parameters (again & see below, accessible by name in writing the logic).

### Usage

Having specified a model, we can load up the `Rcpp` package and compile it:

``` {.r}
library(Rcpp) # assuming in dir above src
sourceCpp('src/pilsner.cpp') #compiles: only needed once per session
```

Then specify some input data:

``` {.r}
NN <- 1e4 #cohort size
AM <- matrix(15,ncol=1,nrow=NN); colnames(AM) <- "age" # age data <0 will not be advanced
CM <- matrix(0,ncol=2,nrow=NN); colnames(CM) <- c("timeofdeath","timeofinfection")
DM <- matrix(1,ncol=2,nrow=NN); colnames(DM) <- c("alive","infected")
DM[,2] <- 0 #start uninfected
```

Here are some parameters, specified in a round about way, to illustrate both single and individualized parameter input data

``` {.r}
## parameter examples
PP <- data.frame(mu=rep(.05,NN),lambda=rep(0.01,NN),nu=rep(0.1,NN))
df2l <- function(x) lapply(split(x,1:nrow(x)),unlist) #for converting dataframe
PP0 <- list(unlist(PP[1,])) # take the top row as a parameter input
PPL <- df2l(PP) # split the whole dataframe into a list with separate parameter for everyone
```

Now we can run the model and examine the output:

``` {.r}
out <- simulator(AM,CM,DM,100,PPL,recording=FALSE) #returns by side effect
## 36248 events took place...in 0.072824 seconds.
summary(out)
##    recording     turned       off   
##  Min.   :0   Min.   :0   Min.   :0  
##  1st Qu.:0   1st Qu.:0   1st Qu.:0  
##  Median :0   Median :0   Median :0  
##  Mean   :0   Mean   :0   Mean   :0  
##  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
##  Max.   :0   Max.   :0   Max.   :0
head(DM)
##      alive infected
## [1,]     0        0
## [2,]     0        1
## [3,]     0        0
## [4,]     0        1
## [5,]     0        0
## [6,]     0        0
head(CM)
##      timeofdeath timeofinfection
## [1,]   37.285297         0.00000
## [2,]   33.966684        15.63514
## [3,]    3.829991         0.00000
## [4,]   46.982539        20.26034
## [5,]    3.589370         0.00000
## [6,]   23.992239         0.00000
head(AM)
##           age
## [1,] 52.28530
## [2,] 48.96668
## [3,] 18.82999
## [4,] 61.98254
## [5,] 18.58937
## [6,] 38.99224
summary(DM)
##      alive          infected     
##  Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:0.0000  
##  Median :0.000   Median :0.0000  
##  Mean   :0.007   Mean   :0.1608  
##  3rd Qu.:0.000   3rd Qu.:0.0000  
##  Max.   :1.000   Max.   :1.0000
```

Note again how this acts by side-effect. `out` contains a reminder that it is empty unless `recording=TRUE` (which slows things down). For this simple model, it ran at around half a million events per second on my desktop. Further speed-ups could probably be achieved by using GSL PRNGs for example.

Check by plotting:

``` {.r}
library(ggplot2)  # loading plotting library (once)
P <- ggplot(as.data.frame(cbind(AM,DM)),
      aes(x=age,fill=factor(infected))) +
  geom_histogram() + theme_classic()
print(P)  
```

<img src="README_figs/README-unnamed-chunk-8-1.png" width="912" />

### Model specification

TODO also \<0 advance bug? & CR

### Warning

There are indubitably bugs -- this isn't fully tested. Feedback and corrections most welcome!
