# plsdepot

The R package `"plsdepot"` provides a general framework for Partial Least Squares (PLS) Data Analysis Methods. `"plsdepot"` comes with a set of functions for PLS analysis of one or two data tables such as Tucker's Inter-Battery, NIPALS, SIMPLS, SIMPLS-CA, PLS Regression, as well as PLS Canonical Analysis.


## Donation

As a Data Science and Statistics educator, I love to share the work I do. 
Each month I spend dozens of hours curating learning materials and computational
tools like this R package. If you find any value and usefulness in `plsdepot`, 
please consider making a 
<a href="https://www.paypal.com/donate?business=ZF6U7K5MW25W2&currency_code=USD" target="_blank">one-time donation---via paypal---in any amount</a> 
(e.g. the amount you would spend inviting me a coffee or any other drink). 
Your support really matters.

<a href="https://www.paypal.com/donate?business=ZF6U7K5MW25W2&currency_code=USD" target="_blank"><img src="https://www.gastonsanchez.com/images/donate.png" width="140" height="60"/></a>


## Installation

To install the stable version of `"plsdepot"` from CRAN, run in your R console:

```r
install.packages("plsdepot")
```

To install the development version of `"plsdepot"` from github (using the package "devtools"), simply run in your R console:

```r
# install.packages("devtools") 
library(devtools)
install_github('plsdepot',  username='gastonstat')
```


## Motivation

It's been more than 3 years since I first launched the R package __[plspm](https://github.com/gastonstat/plspm)__. From the feedback of several colleagues, analysts and practitioners, I believe that I did the right thing in releasing and sharing the work of my doctoral adventure. I've received a lot of emails with nice comments, compliments, and congratulations. But I've also received constructive criticism, proposals, improvement suggestions... and bug reports.

If there is something in particular that I've learned over the last 3 years, is that being the maintainer of a package can be a really tough task. And this is the main reason for me to launch `"plsdepot"`. 

I've decided to split `"plspm"` in order to separate those methods specifically designed for PLS Path Modeling from the rest of the PLS methods. This makes much easier the maintainance of the functions, and it also allows me to kill the bugs in a more efficient way.

I know this decision may affect the work of some people, but in the long run is the only feasible solution that I can offer to keep my altruistic work alive.


