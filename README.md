
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
## 36173 events took place...in 0.072984 seconds.
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
## [2,]     0        0
## [3,]     0        0
## [4,]     0        0
## [5,]     0        0
## [6,]     0        0
head(CM)
##      timeofdeath timeofinfection
## [1,]  35.8843475               0
## [2,]   5.7082524               0
## [3,]  41.7891155               0
## [4,]  13.2641452               0
## [5,]   6.3478396               0
## [6,]   0.4533705               0
head(AM)
##           age
## [1,] 50.88435
## [2,] 20.70825
## [3,] 56.78912
## [4,] 28.26415
## [5,] 21.34784
## [6,] 15.45337
summary(DM)
##      alive           infected     
##  Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000  
##  Mean   :0.0082   Mean   :0.1634  
##  3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000
```

Note again how this acts by side-effect. `out` contains a reminder that it is empty unless `recording=TRUE` (which slows things down). For this simple model, it ran at around half a million events per second on my desktop. Further speed-ups could probably be achieved by using GSL PRNGs for example.

Check by plotting:

``` {.r}
library(ggplot2); theme_minimal() # loading plotting library (once)
## List of 57
##  $ line                 :List of 6
##   ..$ colour       : chr "black"
##   ..$ size         : num 0.5
##   ..$ linetype     : num 1
##   ..$ lineend      : chr "butt"
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ rect                 :List of 5
##   ..$ fill         : chr "white"
##   ..$ colour       : chr "black"
##   ..$ size         : num 0.5
##   ..$ linetype     : num 1
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ text                 :List of 11
##   ..$ family       : chr ""
##   ..$ face         : chr "plain"
##   ..$ colour       : chr "black"
##   ..$ size         : num 11
##   ..$ hjust        : num 0.5
##   ..$ vjust        : num 0.5
##   ..$ angle        : num 0
##   ..$ lineheight   : num 0.9
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : logi FALSE
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.x         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 5.5 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.x.top     :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 5.5 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.y         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : num 90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 5.5 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.y.right   :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : num -90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 5.5
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text            :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : chr "grey30"
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.x          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 2.2 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.x.top      :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 2.2 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.y          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 1
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 2.2 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.y.right    :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 0
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 2.2
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.ticks           : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ axis.ticks.length    :Class 'unit'  atomic [1:1] 2.75
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ axis.line            : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ axis.line.x          : NULL
##  $ axis.line.y          : NULL
##  $ legend.background    : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ legend.margin        :Classes 'margin', 'unit'  atomic [1:4] 0.2 0.2 0.2 0.2
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.spacing       :Class 'unit'  atomic [1:1] 0.4
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.spacing.x     : NULL
##  $ legend.spacing.y     : NULL
##  $ legend.key           : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ legend.key.size      :Class 'unit'  atomic [1:1] 1.2
##   .. ..- attr(*, "valid.unit")= int 3
##   .. ..- attr(*, "unit")= chr "lines"
##  $ legend.key.height    : NULL
##  $ legend.key.width     : NULL
##  $ legend.text          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ legend.text.align    : NULL
##  $ legend.title         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 0
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ legend.title.align   : NULL
##  $ legend.position      : chr "right"
##  $ legend.direction     : NULL
##  $ legend.justification : chr "center"
##  $ legend.box           : NULL
##  $ legend.box.margin    :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 0
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.box.background: list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ legend.box.spacing   :Class 'unit'  atomic [1:1] 0.4
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ panel.background     : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ panel.border         : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ panel.spacing        :Class 'unit'  atomic [1:1] 5.5
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ panel.spacing.x      : NULL
##  $ panel.spacing.y      : NULL
##  $ panel.grid.major     :List of 6
##   ..$ colour       : chr "grey92"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ panel.grid.minor     :List of 6
##   ..$ colour       : chr "grey92"
##   ..$ size         : num 0.25
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ panel.ontop          : logi FALSE
##  $ plot.background      : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ plot.title           :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 1.2
##   ..$ hjust        : num 0
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 6.6 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.subtitle        :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.9
##   ..$ hjust        : num 0
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 4.95 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.caption         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.9
##   ..$ hjust        : num 1
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 4.95 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.margin          :Classes 'margin', 'unit'  atomic [1:4] 5.5 5.5 5.5 5.5
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ strip.background     : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ strip.placement      : chr "inside"
##  $ strip.text           :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : chr "grey10"
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.text.x         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 5.5 0 5.5 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.text.y         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : num -90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 5.5 0 5.5
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.switch.pad.grid:Class 'unit'  atomic [1:1] 0.1
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ strip.switch.pad.wrap:Class 'unit'  atomic [1:1] 0.1
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  - attr(*, "class")= chr [1:2] "theme" "gg"
##  - attr(*, "complete")= logi TRUE
##  - attr(*, "validate")= logi TRUE
P <- ggplot(as.data.frame(cbind(AM,DM)),
      aes(x=age,fill=factor(infected))) +
  geom_histogram()
print(P)  
```

<img src="README_figs/README-unnamed-chunk-8-1.png" width="912" />

### Model specification

TODO also \<0 advance bug? & CR

### Warning

There are indubitably bugs -- this isn't fully tested. Feedback and corrections most welcome!
