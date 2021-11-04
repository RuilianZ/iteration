writing\_functions.Rmd
================
Ruilian Zhang
11/4/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.4544413  0.9332982  1.1544398 -0.2030042  1.3306107  1.5764862
    ##  [7] -0.2572499  0.9111245  1.0515646 -0.3388672 -0.9646912 -0.4896306
    ## [13] -0.6153589  0.4723981 -0.4599356  1.0347018 -0.3364055 -2.2348024
    ## [19] -1.2033896  0.9145038 -1.4919226 -0.8695298 -0.1450303  0.8104640
    ## [25] -1.0342151

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.4544413  0.9332982  1.1544398 -0.2030042  1.3306107  1.5764862
    ##  [7] -0.2572499  0.9111245  1.0515646 -0.3388672 -0.9646912 -0.4896306
    ## [13] -0.6153589  0.4723981 -0.4599356  1.0347018 -0.3364055 -2.2348024
    ## [19] -1.2033896  0.9145038 -1.4919226 -0.8695298 -0.1450303  0.8104640
    ## [25] -1.0342151

``` r
y_vec = rnorm(40, 12, .3)

z_scores(y_vec)
```

    ##  [1] -0.57268140 -0.75212104 -1.34746422 -1.53260491 -0.11258484 -0.45236199
    ##  [7]  0.35283089 -0.99554602  1.37583914  1.31105397  1.21677701 -0.37483505
    ## [13]  0.36440821 -1.05525175 -1.37252854  0.50657049  0.87450524  0.93937970
    ## [19]  1.25815953 -0.23060296 -0.15417154  1.54530818 -1.56128781  2.37028745
    ## [25] -0.15479280  0.48920274 -2.23298496  0.91018832 -0.34629891  0.01387925
    ## [31] -0.45152714  0.22557266  1.19605412 -0.70483422  0.61431699 -0.82941297
    ## [37]  0.24556739 -0.83457807  0.09339294  0.16517691

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores("Roxy")
```

    ## Error in z_scores("Roxy"): x needs to be numeric

``` r
z_scores(y_vec)
```

    ##  [1] -0.57268140 -0.75212104 -1.34746422 -1.53260491 -0.11258484 -0.45236199
    ##  [7]  0.35283089 -0.99554602  1.37583914  1.31105397  1.21677701 -0.37483505
    ## [13]  0.36440821 -1.05525175 -1.37252854  0.50657049  0.87450524  0.93937970
    ## [19]  1.25815953 -0.23060296 -0.15417154  1.54530818 -1.56128781  2.37028745
    ## [25] -0.15479280  0.48920274 -2.23298496  0.91018832 -0.34629891  0.01387925
    ## [31] -0.45152714  0.22557266  1.19605412 -0.70483422  0.61431699 -0.82941297
    ## [37]  0.24556739 -0.83457807  0.09339294  0.16517691

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.51  3.55

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.275

## Different sample siezs, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98  3.36

Let’s write function that simulates data, compute mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
    sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.56  2.91
