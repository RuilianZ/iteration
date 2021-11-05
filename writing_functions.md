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

    ##  [1]  0.6344882 -1.8065878  1.5684643 -1.5359300 -0.2443960 -0.8368381
    ##  [7] -0.7373880  1.2064680 -0.6574762  0.7803904 -1.4265835 -0.5031362
    ## [13]  1.7931638 -1.0388869  1.1084507 -0.1679312  0.4775040  0.4259981
    ## [19] -0.1257196  0.9694586  0.8979138  0.4955456 -0.9570625 -0.6548887
    ## [25]  0.3349791

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.6344882 -1.8065878  1.5684643 -1.5359300 -0.2443960 -0.8368381
    ##  [7] -0.7373880  1.2064680 -0.6574762  0.7803904 -1.4265835 -0.5031362
    ## [13]  1.7931638 -1.0388869  1.1084507 -0.1679312  0.4775040  0.4259981
    ## [19] -0.1257196  0.9694586  0.8979138  0.4955456 -0.9570625 -0.6548887
    ## [25]  0.3349791

``` r
y_vec = rnorm(40, 12, .3)

z_scores(y_vec)
```

    ##  [1]  0.561275839  0.373161944  0.533385417 -1.090821349 -0.201478593
    ##  [6]  0.326203872 -1.095943923 -1.486472086 -0.635173452  1.074285552
    ## [11]  2.053876324  0.597561732 -0.459878463 -0.449032291  0.222411966
    ## [16] -0.605135043 -1.619999722 -2.253957656  0.196278578  1.067279795
    ## [21]  0.386586335 -1.226606849 -0.629443715  1.713135634  1.298412453
    ## [26]  0.797370133  0.886863560  1.780892072 -0.002247607 -1.397111937
    ## [31]  0.101185030 -0.318636026  1.027111760 -0.670983540 -0.798898216
    ## [36] -0.662774607 -0.670074161  0.163026314 -0.018063398  1.132428327

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

    ##  [1]  0.561275839  0.373161944  0.533385417 -1.090821349 -0.201478593
    ##  [6]  0.326203872 -1.095943923 -1.486472086 -0.635173452  1.074285552
    ## [11]  2.053876324  0.597561732 -0.459878463 -0.449032291  0.222411966
    ## [16] -0.605135043 -1.619999722 -2.253957656  0.196278578  1.067279795
    ## [21]  0.386586335 -1.226606849 -0.629443715  1.713135634  1.298412453
    ## [26]  0.797370133  0.886863560  1.780892072 -0.002247607 -1.397111937
    ## [31]  0.101185030 -0.318636026  1.027111760 -0.670983540 -0.798898216
    ## [36] -0.662774607 -0.670074161  0.163026314 -0.018063398  1.132428327

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
    ## 1  5.15  3.78

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.315

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
    ## 1  1.56  2.88

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
    ## 1  4.48  2.46

## Napolean

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are a lot pages if reviews…

Write a fuction that gets review based in page url

``` r
get_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

dynamite_reviews = bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  2 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  3 Best quirky movie ever                                    5 You all know the…
    ##  4 Classic Film                                              5 Had to order thi…
    ##  5 hehehehe                                                  5 goodjobboys      
    ##  6 Painful                                                   1 I think I sneeze…
    ##  7 GRAND                                                     5 GRAND            
    ##  8 Hello, 90s                                                5 So nostalgic mov…
    ##  9 Cult Classic                                              5 Watched it with …
    ## 10 Format was inaccurate                                     4 There was an opt…
    ## # … with 40 more rows
