iteration\_and\_listcols
================
Ruilian Zhang
11/9/2021

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

## Lists

``` r
l = 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

``` r
l[3]
```

    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

## Define funciton

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
```

## List of normals

``` r
list_norms = 
  list(
    a = rnorm(50, 2, 1),
    b = rnorm(50, 5, 3),
    c = rnorm(50, 20, 1.2),
    d = rnorm(50, -12, 0.5)
  )

mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8  1.09

## Let’s write for loop

``` r
output = vector("list", length = 4)

# output[[1]] = mean_and_sd(list_norms[[1]])

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}
```

Let’s use map instead …

``` r
output = map(list_norms, mean_and_sd)

output = map(list_norms, median)

output = map(list_norms, IQR)

output = map(list_norms, summary)

# return just numbers
output = map_dbl(list_norms, median)
```

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )

listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>%  pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>%  pull(norms)
```

    ## $a
    ##  [1] 3.1349651 3.1119318 1.1292224 2.2107316 2.0693956 0.3373511 2.8108400
    ##  [8] 0.0876542 0.7532466 2.9981544 1.4591273 1.7836242 0.3780627 0.5490360
    ## [15] 2.3509097 1.8254531 1.4085715 0.6659727 0.9027015 4.0361036 1.6735104
    ## [22] 2.7740052 2.7850064 2.7632461 2.2948088 0.7476441 0.9904962 2.7513912
    ## [29] 0.6916465 2.5275401 1.4664604 1.6016240 1.2104305 1.7698589 2.8771848
    ## [36] 2.4537332 1.7675359 2.8700055 3.6560037 1.9936311 2.4704895 2.2782186
    ## [43] 1.0220971 1.0734139 3.9197705 2.8812778 2.7420818 2.1475734 2.4853886
    ## [50] 2.1518560
    ## 
    ## $b
    ##  [1]  5.1259963  5.6702669  1.9686047 12.2036663  7.4058854  4.2463761
    ##  [7]  8.6386681  3.1182257 10.1334755  3.8168793 -1.9644726  9.0923576
    ## [13]  8.3966874  2.6770510  0.7688751 -0.5035827  4.1929594 -0.5017857
    ## [19]  2.5565959  5.4907164  7.5665577  2.5401106  4.6291917  5.7648447
    ## [25] 10.1567790  2.1243694  0.1870692 -0.5368283  6.6672116  4.8196424
    ## [31]  7.3162589  4.5774818  6.1792818  5.6726557  5.0706260  3.1311120
    ## [37]  8.7860281  3.7826779  7.0002913  5.4939175 10.3445734  7.1336419
    ## [43]  3.9869265  4.9725531  4.6240724 -1.2725383 10.0921817  8.1916435
    ## [49]  2.7001501  6.1460227
    ## 
    ## $c
    ##  [1] 20.29028 18.64069 21.78789 19.70210 20.22030 20.48585 18.80705 18.69748
    ##  [9] 19.94175 20.69130 20.08860 20.84713 20.40198 20.65447 18.31651 20.81246
    ## [17] 19.05224 19.44113 19.87418 18.02258 19.88056 19.47217 19.13779 19.33448
    ## [25] 21.49459 18.48929 19.74154 17.03365 19.19100 19.39844 21.85079 18.84558
    ## [33] 18.95338 18.32284 20.21577 21.38491 18.56176 19.48913 21.63957 19.17884
    ## [41] 20.82261 20.46740 18.43352 21.46027 20.95421 19.41416 18.91521 19.53150
    ## [49] 20.97688 19.32290
    ## 
    ## $d
    ##  [1] -12.93710 -12.07145 -11.61405 -12.57956 -12.11896 -12.61110 -11.94160
    ##  [8] -12.06625 -12.01684 -12.31116 -12.35468 -11.56428 -11.94743 -12.09347
    ## [15] -13.60659 -12.63781 -11.61855 -12.20341 -12.60416 -12.21966 -12.18779
    ## [22] -12.25072 -11.75169 -11.23956 -11.50595 -11.37694 -12.16493 -11.57783
    ## [29] -12.49054 -12.06961 -10.90728 -12.00641 -12.15265 -12.29211 -11.61437
    ## [36] -10.94691 -11.79392 -12.13063 -10.96311 -12.38942 -11.43423 -12.21067
    ## [43] -12.51087 -11.39085 -12.89988 -12.15412 -11.99224 -12.22116 -12.81900
    ## [50] -12.32070

``` r
listcol_df$norms[[1]]
```

    ##  [1] 3.1349651 3.1119318 1.1292224 2.2107316 2.0693956 0.3373511 2.8108400
    ##  [8] 0.0876542 0.7532466 2.9981544 1.4591273 1.7836242 0.3780627 0.5490360
    ## [15] 2.3509097 1.8254531 1.4085715 0.6659727 0.9027015 4.0361036 1.6735104
    ## [22] 2.7740052 2.7850064 2.7632461 2.2948088 0.7476441 0.9904962 2.7513912
    ## [29] 0.6916465 2.5275401 1.4664604 1.6016240 1.2104305 1.7698589 2.8771848
    ## [36] 2.4537332 1.7675359 2.8700055 3.6560037 1.9936311 2.4704895 2.2782186
    ## [43] 1.0220971 1.0734139 3.9197705 2.8812778 2.7420818 2.1475734 2.4853886
    ## [50] 2.1518560

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967

``` r
listcol_df %>% 
  mutate(summaries = map(listcol_df$norms, mean_and_sd)) %>% 
  pull(summaries)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.01  3.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8  1.09
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1 0.533

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:31:00 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:31:05 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:31:08 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

weather_nested %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
unnest(weather_nested, data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Napoleon

Function to get reviews/stars

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

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I…
    ##  2 too many commercials                                      1 5 minutes into t…
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo…
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  7 Best quirky movie ever                                    5 You all know the…
    ##  8 Classic Film                                              5 Had to order thi…
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars text                                 
    ##    <chr>                             <dbl> <chr>                                
    ##  1 GRAND                                 5 GRAND                                
    ##  2 Hello, 90s                            5 So nostalgic movie                   
    ##  3 Cult Classic                          5 Watched it with my older grandchilde…
    ##  4 Format was inaccurate                 4 There was an option to choose blue R…
    ##  5 Good funny                            3 Would recommend                      
    ##  6 Not available w/in 48 hour window     1 I couldn't watch it and there is no …
    ##  7 Your mom went to college.             5 Classic funny movie. It has some of …
    ##  8 Very funny movie                      5 I watch this movie with my family. V…
    ##  9 Watch it twice! Trust me!             5 Nothing to dislike!  Cult Classic Mo…
    ## 10 A classic                             5 If you don’t enjoy this movie, we ca…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars text                       
    ##    <chr>                                       <dbl> <chr>                      
    ##  1 Can't say how many times I've seen              5 Such a great movie. Will n…
    ##  2 I pity the fool who doesn’t own this movie.     5 I love technology but not …
    ##  3 I don’t know why it’s so popular!               2 My girlfriend loves it!    
    ##  4 Okay                                            3 Okay                       
    ##  5 A WHOLESOME comedic journey                     5 Not a moment of this movie…
    ##  6 Hilarious                                       5 Funny                      
    ##  7 Love it                                         5 What of the funniest movies
    ##  8 WORTH IT!                                       5 It's the dry humor for me.…
    ##  9 Funny movie.                                    5 Great comedy               
    ## 10 Best movie ever!                                5 Got this for my sister who…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 I was stuck in the oil patch back in the day.     5 I watched this serially.…
    ##  2 Funny Dork humor                                  5 Humor that is funnier wh…
    ##  3 Still funny!                                      5 Still funny!             
    ##  4 Love it!! 💜                                      5 Love it!! 💜             
    ##  5 LOVE it                                           5 cult classic. So ugly it…
    ##  6 Perfect                                           5 Exactly what I asked for 
    ##  7 Love this movie!                                  5 Great movie and sent in …
    ##  8 Love it                                           5 Love this movie. However…
    ##  9 As described                                      3 Book is as described     
    ## 10 GOSH!!!                                           5 Just watch the movie GOS…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars text                                 
    ##    <chr>                             <dbl> <chr>                                
    ##  1 Watch it right now                    5 You need to watch this movie today. …
    ##  2 At this point it’s an addiction       5 I watch this movie way too much. Hav…
    ##  3 💕                                    5 Hands down, one of my favorite movie…
    ##  4 Good dumb movie                       5 I really wanted to show my spouse a …
    ##  5 funny                                 5 so funny and inventive, if you know …
    ##  6 Best Movie- Try to prove me wrong     5 Best movie ever                      
    ##  7 Vote For Pedro!!                      5 What is NOT to like about this movie…
    ##  8 So Funny                              5 This is such a good movie, so unders…
    ##  9 Best movie ever                       5 It's napoleon dynamite, what do you …
    ## 10 Funny                                 5 Classic

``` r
napoleon_df = 
  tibble(
    urls = urls
    )

napoleon_df %>% 
  mutate(reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I…
    ##  2 too many commercials                                      1 5 minutes into t…
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo…
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  7 Best quirky movie ever                                    5 You all know the…
    ##  8 Classic Film                                              5 Had to order thi…
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze…
    ## # … with 40 more rows
