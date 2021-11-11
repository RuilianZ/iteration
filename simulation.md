Simulation
================
Ruilian Zhang
11/11/2021

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

``` r
set.seed(1)
```

## Let’s simulate something

I have a function.

``` r
sim_mean_sd = function(sample_size, mu = 3, sigma = 4) {
  
    sim_data = 
    tibble(
      x = rnorm(n = sample_size, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}
```

I can “simulate” by running this line.

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.33  3.70

## Lets’ simulate a lot

Let’s start with a for loop.

``` r
output = vector("list", length = 100)

for (i in 1:100) {
  
  output[[i]] = sim_mean_sd(sample_size = 30)
}

bind_rows(output)
```

    ## # A tibble: 100 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ##  1  3.53  3.18
    ##  2  3.44  3.84
    ##  3  3.45  3.53
    ##  4  1.68  3.69
    ##  5  3.95  4.22
    ##  6  3.27  4.34
    ##  7  2.05  4.05
    ##  8  3.10  3.72
    ##  9  3.55  4.11
    ## 10  3.87  3.79
    ## # … with 90 more rows

Let’s use a loop function.

``` r
sim_results = 
  rerun(100, sim_mean_sd(sample_size = 30)) %>% 
  bind_rows()
```

Let’s look at results.

``` r
sim_results %>% 
  ggplot(aes(x = mean)) + geom_density()
```

![](simulation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
4/sqrt(30) # sample sd
```

    ## [1] 0.7302967

``` r
sim_results %>% 
  summarize(
    average_sample_mean = mean(mean),
    sd_sample_mean = sd(mean)
  )
```

    ## # A tibble: 1 × 2
    ##   average_sample_mean sd_sample_mean
    ##                 <dbl>          <dbl>
    ## 1                2.98          0.756

``` r
sim_results %>% 
  ggplot(aes(x = sd)) + geom_density()
```

![](simulation_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->
