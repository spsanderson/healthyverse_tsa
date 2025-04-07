Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
07 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 136,099
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2025-04-05 23:43:23, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5796.13
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 136099        |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     97863 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97863 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97863 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11448 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-05 | 2023-05-26 | 1595 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134272.58 | 1523568.44 | 355 | 14701 | 271098 | 2367764 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10356.82 | 18359.49 | 1 | 299 | 3064 | 11721 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-05 23:43:23 | 2023-05-26 19:00:22 | 82739 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 17S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -148.90  -35.50  -10.64   26.55  812.62 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.838e+02  7.347e+01
    ## date                                                1.119e-02  3.898e-03
    ## lag(value, 1)                                       1.081e-01  2.481e-02
    ## lag(value, 7)                                       9.804e-02  2.569e-02
    ## lag(value, 14)                                      9.590e-02  2.583e-02
    ## lag(value, 21)                                      6.561e-02  2.587e-02
    ## lag(value, 28)                                      5.996e-02  2.577e-02
    ## lag(value, 35)                                      6.875e-02  2.584e-02
    ## lag(value, 42)                                      4.820e-02  2.595e-02
    ## lag(value, 49)                                      7.628e-02  2.590e-02
    ## month(date, label = TRUE).L                        -1.023e+01  5.145e+00
    ## month(date, label = TRUE).Q                         2.752e+00  5.202e+00
    ## month(date, label = TRUE).C                        -1.233e+01  5.238e+00
    ## month(date, label = TRUE)^4                        -6.645e+00  5.197e+00
    ## month(date, label = TRUE)^5                        -1.223e+01  5.210e+00
    ## month(date, label = TRUE)^6                        -3.158e+00  5.285e+00
    ## month(date, label = TRUE)^7                        -6.160e+00  5.165e+00
    ## month(date, label = TRUE)^8                        -4.549e+00  5.186e+00
    ## month(date, label = TRUE)^9                         5.420e+00  5.234e+00
    ## month(date, label = TRUE)^10                        4.556e+00  5.296e+00
    ## month(date, label = TRUE)^11                       -5.899e+00  5.340e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.175e+01  2.392e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.079e+00  2.518e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.501 0.012479 *  
    ## date                                                 2.870 0.004166 ** 
    ## lag(value, 1)                                        4.358 1.40e-05 ***
    ## lag(value, 7)                                        3.816 0.000141 ***
    ## lag(value, 14)                                       3.713 0.000212 ***
    ## lag(value, 21)                                       2.536 0.011299 *  
    ## lag(value, 28)                                       2.326 0.020123 *  
    ## lag(value, 35)                                       2.661 0.007877 ** 
    ## lag(value, 42)                                       1.857 0.063440 .  
    ## lag(value, 49)                                       2.945 0.003283 ** 
    ## month(date, label = TRUE).L                         -1.988 0.047005 *  
    ## month(date, label = TRUE).Q                          0.529 0.596926    
    ## month(date, label = TRUE).C                         -2.354 0.018697 *  
    ## month(date, label = TRUE)^4                         -1.279 0.201162    
    ## month(date, label = TRUE)^5                         -2.346 0.019081 *  
    ## month(date, label = TRUE)^6                         -0.598 0.550172    
    ## month(date, label = TRUE)^7                         -1.193 0.233169    
    ## month(date, label = TRUE)^8                         -0.877 0.380525    
    ## month(date, label = TRUE)^9                          1.036 0.300517    
    ## month(date, label = TRUE)^10                         0.860 0.389698    
    ## month(date, label = TRUE)^11                        -1.105 0.269431    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.913 9.95e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.208 0.001365 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.68 on 1523 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2496, Adjusted R-squared:  0.2387 
    ## F-statistic: 23.02 on 22 and 1523 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

## NNS Forecasting

This is something I have been wanting to try for a while. The `NNS`
package is a great package for forecasting time series data.

[NNS GitHub](https://github.com/OVVO-Financial/NNS)

``` r
library(NNS)

data_list <- base_data |>
    select(package, value) |>
    group_split(package)

data_list |>
    imap(
        \(x, idx) {
            obj <- x
            x <- obj |> pull(value) |> tail(7*52)
            train_set_size <- length(x) - 56
            pkg <- obj |> pluck(1) |> unique()
            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            
            cat(paste0("Package: ", pkg, "\n"))
            NNS.ARMA.optim(
                variable = x,
                h = 28,
                training.set = train_set_size,
                #seasonal.factor = seq(12, 60, 7),
                seasonal.factor = sf,
                pred.int = 0.95,
                plot = TRUE
            )
            title(
                sub = paste0("\n",
                             "Package: ", pkg, " - NNS Optimization")
            )
        }
    )
```

    ## Package: healthyR
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.83995082530644"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.57809229330705"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.48169522081318"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4700490138473"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35, 21, 70, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.4700490138473"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35, 21, 70, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.94293112423768"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35, 21, 70, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.94293112423768"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35, 21, 70, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.54001267703272"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35, 21, 70, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.54001267703272"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.17238004448681"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.85937797504069"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.80827509710033"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 21, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.77389484348235"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 21, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.77389484348235"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 21, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.10065554872545"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 21, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.10065554872545"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 21, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.8520371883697"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 21, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.8520371883697"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.18300143137741"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.18300143137741"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.70784443025082"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.70784443025082"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.25720471736696"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.25720471736696"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.21277883608614"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.69969856526983"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.69969856526983"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.56608176169016"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.56608176169016"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.99766678902579"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.99766678902579"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.74974599735719"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.36963364382782"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.36963364382782"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.17109012353015"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.17109012353015"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.04162386859908"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.04162386859908"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.86782902288423"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.86782902288423"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.49819387585806"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.49819387585806"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.11580232594176"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.11580232594176"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.53503636911786"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.52712161074812"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.52712161074812"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.1770364975331"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.1770364975331"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.91452806672402"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.91452806672402"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.07067800233074"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.75829361912855"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.75829361912855"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.17293823243409"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.17293823243409"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.20749732524109"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.20749732524109"

![](man/figures/README-nns_forecasting-8.png)<!-- -->

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL
    ## 
    ## [[8]]
    ## NULL

## Pre-Processing

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,588 × 2]> <tibble [28 × 2]> <split [1560|28]>
    ## 2 healthyR      <tibble [1,581 × 2]> <tibble [28 × 2]> <split [1553|28]>
    ## 3 healthyR.ts   <tibble [1,525 × 2]> <tibble [28 × 2]> <split [1497|28]>
    ## 4 healthyverse  <tibble [1,496 × 2]> <tibble [28 × 2]> <split [1468|28]>
    ## 5 healthyR.ai   <tibble [1,320 × 2]> <tibble [28 × 2]> <split [1292|28]>
    ## 6 TidyDensity   <tibble [1,171 × 2]> <tibble [28 × 2]> <split [1143|28]>
    ## 7 tidyAML       <tibble [779 × 2]>   <tibble [28 × 2]> <split [751|28]> 
    ## 8 RandomWalker  <tibble [201 × 2]>   <tibble [28 × 2]> <split [173|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7511033 | 120.68838 | 0.6669768 | 174.56645 | 0.9471055 | 0.0008044 |
| healthyR.data | 2 | LM | Test | 0.7269830 | 161.91929 | 0.6455580 | 135.84080 | 0.8719569 | 0.0081862 |
| healthyR.data | 3 | EARTH | Test | 0.8267075 | 126.21345 | 0.7341130 | 141.86585 | 1.1149747 | 0.0081862 |
| healthyR.data | 4 | NNAR | Test | 0.7542280 | 100.09105 | 0.6697515 | 157.76290 | 1.0099061 | 0.1062609 |
| healthyR | 1 | ARIMA | Test | 0.7399819 | 102.16095 | 0.7610672 | 171.06547 | 0.9324989 | 0.0272463 |
| healthyR | 2 | LM | Test | 0.7369722 | 98.35177 | 0.7579717 | 186.72955 | 0.9162167 | 0.0092741 |
| healthyR | 3 | EARTH | Test | 0.7357433 | 97.15271 | 0.7567078 | 186.05420 | 0.9158940 | 0.0092741 |
| healthyR | 4 | NNAR | Test | 0.6808830 | 88.95049 | 0.7002844 | 142.11732 | 0.8807588 | 0.0602957 |
| healthyR.ts | 1 | ARIMA | Test | 1.0654515 | 284.16234 | 0.8281830 | 141.84212 | 1.3058019 | 0.0011038 |
| healthyR.ts | 2 | LM | Test | 1.0536145 | 271.93484 | 0.8189820 | 142.37553 | 1.2926505 | 0.0011038 |
| healthyR.ts | 3 | EARTH | Test | 1.0584040 | 276.98320 | 0.8227049 | 142.11603 | 1.2981209 | 0.0011038 |
| healthyR.ts | 4 | NNAR | Test | 0.9107277 | 97.04407 | 0.7079151 | 161.37601 | 1.1426640 | 0.0610402 |
| healthyverse | 1 | ARIMA | Test | 0.6583637 | 235.57946 | 0.7530144 | 104.41492 | 0.8118162 | 0.0562159 |
| healthyverse | 2 | LM | Test | 0.6552770 | 344.12516 | 0.7494840 | 95.40014 | 0.7769046 | 0.0141388 |
| healthyverse | 3 | EARTH | Test | 0.7107685 | 210.32062 | 0.8129532 | 115.76211 | 0.8798125 | 0.0141388 |
| healthyverse | 4 | NNAR | Test | 0.7247907 | 201.84707 | 0.8289914 | 116.84412 | 0.9097982 | 0.0339063 |
| healthyR.ai | 1 | ARIMA | Test | 0.7581178 | 99.96808 | 0.7257711 | 162.08900 | 0.9759546 | 0.0567728 |
| healthyR.ai | 2 | LM | Test | 0.7412461 | 135.31439 | 0.7096192 | 143.71997 | 0.9475369 | 0.0672382 |
| healthyR.ai | 3 | EARTH | Test | 0.8983868 | 177.67233 | 0.8600552 | 183.88304 | 1.0778832 | 0.0672382 |
| healthyR.ai | 4 | NNAR | Test | 0.6743380 | 122.93033 | 0.6455659 | 127.54809 | 0.8937213 | 0.1711469 |
| TidyDensity | 1 | ARIMA | Test | 0.6115972 | 172.42804 | 0.6324280 | 112.85682 | 0.7743115 | 0.0715465 |
| TidyDensity | 2 | LM | Test | 0.6799420 | 249.44819 | 0.7031006 | 107.75583 | 0.8667946 | 0.0016181 |
| TidyDensity | 3 | EARTH | Test | 0.6465753 | 194.26514 | 0.6685974 | 114.83355 | 0.7995605 | 0.0016181 |
| TidyDensity | 4 | NNAR | Test | 0.6123894 | 109.97554 | 0.6332472 | 140.01958 | 0.7461275 | 0.1136413 |
| tidyAML | 1 | ARIMA | Test | 0.6243962 | 319.54037 | 0.6367390 | 94.75535 | 0.7933224 | 0.0200283 |
| tidyAML | 2 | LM | Test | 0.6390536 | 321.00161 | 0.6516861 | 97.44836 | 0.7980293 | 0.0055542 |
| tidyAML | 3 | EARTH | Test | 0.6621266 | 357.36942 | 0.6752153 | 97.51233 | 0.8126083 | 0.0055542 |
| tidyAML | 4 | NNAR | Test | 0.6056777 | 313.38450 | 0.6176505 | 94.19993 | 0.7723250 | 0.0993440 |
| RandomWalker | 1 | ARIMA | Test | 1.3789621 | 153.37657 | 0.8301716 | 161.93082 | 1.6028833 | 0.0722998 |
| RandomWalker | 2 | LM | Test | 1.2900864 | 110.61910 | 0.7766661 | 193.90008 | 1.4138294 | 0.0158607 |
| RandomWalker | 3 | EARTH | Test | 1.2006940 | 90.95167 | 0.7228495 | 168.29902 | 1.3621144 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3303059 | 118.05519 | 0.8008793 | 156.54778 | 1.5674438 | 0.0022912 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 10
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         2 LM          Test  0.727 162.  0.646 136.  0.872  0.00819
    ## 2 healthyR            4 NNAR        Test  0.681  89.0 0.700 142.  0.881  0.0603 
    ## 3 healthyR.ts         4 NNAR        Test  0.911  97.0 0.708 161.  1.14   0.0610 
    ## 4 healthyver…         2 LM          Test  0.655 344.  0.749  95.4 0.777  0.0141 
    ## 5 healthyR.ai         4 NNAR        Test  0.674 123.  0.646 128.  0.894  0.171  
    ## 6 TidyDensity         4 NNAR        Test  0.612 110.  0.633 140.  0.746  0.114  
    ## 7 tidyAML             4 NNAR        Test  0.606 313.  0.618  94.2 0.772  0.0993 
    ## 8 RandomWalk…         3 EARTH       Test  1.20   91.0 0.723 168.  1.36  NA

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1560|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1553|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1497|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1468|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1292|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1143|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [751|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [173|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
