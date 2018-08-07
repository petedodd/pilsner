
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

Then either download or clone this repo: the source files are in the `src` directory.

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

Let's set the PRNG seed:

``` {.r}
set.seed(123)
```

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
PP <- data.frame(mu=rep(.05,NN),lambda=rep(0.01,NN))
df2l <- function(x) lapply(split(x,1:nrow(x)),unlist) #fn for converting dataframe
PP0 <- list(unlist(PP[1,])) # take the top row as a parameter input
PPL <- df2l(PP) # split the whole dataframe into a list with separate parameter for everyone
```

Now we can run the model and examine the output:

``` {.r}
out <- simulator(AM,CM,DM,100,PPL,recording=FALSE) #returns by side effect
## 36239 events took place...in 0.068372 seconds.
out
##   recording turned off
## 1         0      0   0
summary(DM)
##      alive           infected     
##  Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000  
##  Mean   :0.0064   Mean   :0.1602  
##  3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000
head(CM)
##      timeofdeath timeofinfection
## [1,]   24.925256        23.78700
## [2,]    7.946165         0.00000
## [3,]   17.881931        12.44104
## [4,]    6.909767         0.00000
## [5,]    1.227568         0.00000
## [6,]   11.441846         0.00000
head(AM)
##           age
## [1,] 39.92526
## [2,] 22.94616
## [3,] 32.88193
## [4,] 21.90977
## [5,] 16.22757
## [6,] 26.44185
```

Note again how this acts by side-effect. `out` contains a reminder that it is empty unless `recording=TRUE` (which slows things down). For this simple model, it ran at around half a million events per second on my desktop. Further speed-ups could probably be achieved by using GSL PRNGs for example.

Check by plotting:

``` {.r}
library(ggplot2)  # plotting library 
P <- ggplot(as.data.frame(cbind(AM,DM)),
      aes(x=age,fill=factor(infected))) +
  geom_histogram() + theme_classic()
print(P)  
```

<img src="README_figs/README-unnamed-chunk-9-1.png" width="768" />

The `recording=TRUE` option is only really intended for debugging. The idea is that any data that is actually wanted from the run for analysis (eg event counts, cumulative costs etc) should be recorded 'by the people', ie encoded in their data and updated during events. Before running again - this time with `recording` turned on - we need to reset the data:

``` {.r}
AM[,1] <- 15 # resetting data 
CM[,1] <- CM[,2] <- 0
DM[,1] <-1; DM[,2] <- 0 
out <- simulator(AM,CM,DM,100,PPL,recording=TRUE) #
## 36196 events took place...in 0.123335 seconds.
out[(NN-5):(NN+5),]
##              time      event  who
## 9995  0.000000000 initialize 8188
## 9996  0.000000000 initialize 8189
## 9997  0.000000000 initialize 2047
## 9998  0.000000000 initialize 8190
## 9999  0.000000000 initialize 4095
## 10000 0.000000000 initialize 8191
## 10001 0.001707960        die 7610
## 10002 0.002127827        die 7449
## 10003 0.005683208     infect 2018
## 10004 0.007554103     infect 9163
## 10005 0.009211681        die 2901
```

The object returned now contains details of the events to check behaviour is as expected. This should probably be changed to include internal data from people also (which is currently gathered in this mode but not formatted for output).

### Model specification

To code your own model, you need to edit a simple C++ function in `eventlogic.cpp` which looks like this:

``` {.cpp}
int eventdefn( person& P,       // person to be acted on
               double& now,     // reference to current time
               event & ev,      // event to be enacted
               eventq& EQ,      // event queue to add consequences to
               NumericVector& Z // input parameters
               ){
  now = ev.t;                   // advance time
  if(P.D["alive"]==1){          // RIP
    updateages(P.A,ev.t-P.tle);    // update all age-like things
    P.tle = ev.t;       // doesn't matter if no change; last event time used for aging
    event todo = {.t=now, .who=P.who, .what=""}; // event to be determined

// ...STUFF TO EDIT HERE...
   
  }
  return 0;
}
```

This much should probably be left as it is, unless you want to edit the guard that means the dead are left alone. This means `alive` should be one of the columns in the matrix `D` passed to `simulator`, with 1 meaning alive. The `updateages` call does what it says.

There are a few other basic features one needs to understand to be able to work with this:

-   events are C structs and comprise a triple of when, who, and what. Writing an event should be as easy as setting the time of the event labeled `todo` and assing a string to say what it is
-   events are added to an event queue by writing `EQ.push(todo);` (see below)
-   the data for a person (**A**ges, **D**iscrete and **C**ontinuous) are accessed as in the `P.D["alive"]` example here. The programme knows about these names from the column names passed to `simulator`
-   the parameter input data for the relevant person are in `Z` accessed by a string set to the parameter name
-   a basic understanding of R's random deviates from distributions is needed to assign time-to-events
-   any additional events spawned must be defined and added onto the event queue
-   special events called "initialize" and "finalize" are called for everyone at the start and end. NB do not try to use these as names of other events
-   NB all eligibility/guard logic for events is the responsibility of the user! Nothing is assumed.

For the simple exponential time-to-death or time-to-infection model we consider, the logic would be defined for the events as follows:

``` {.cpp}
    if(ev.what=="initialize"){                                             // set up death/infection
      todo.what="die"; todo.t = now - log(Rf_runif(0,1))/Z["mu"];          // time-to-death
      EQ.push(todo);                                                       // add to queue
      todo.what = "infect"; todo.t = now - log(Rf_runif(0,1))/Z["lambda"]; // t-to-infection
      EQ.push(todo);                                                       // add to queue
    }
    if(ev.what=="infect"){              // infection event
      if(P.D["infected"]==0){           // eligible for event
        P.D["infected"] = 1;            // infect!
        P.C["timeofinfection"] = ev.t;  // record
      }
    }
    if(ev.what=="die"){//death event
      P.D["alive"] = 0;                     // recorded as dead
      P.C["timeofdeath"] = ev.t;            // recorded as dead
    }
    if(ev.what=="finalize"){
      // no other changes needed: just for updating ages if alive
    }
```

### TODO list

-   more thorough checking for bugs
    -   think bug if initial ages are ==0?
    -   check event times can't be past end time
-   inclusion of more output for `recording=TRUE`
-   get GSL PRNGs working and benchmark
-   openmp: split into different event queues, 1 per core

### Warning

There are indubitably bugs -- this isn't fully tested. Feedback and corrections most welcome!

### License

Copyright P.J. Dodd (2018): Distributed under CC BY 4.0 license <https://creativecommons.org/licenses/by/4.0/>
