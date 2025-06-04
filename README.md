Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
04 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,470
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

The last day in the data set is 2025-06-02 21:06:50, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7185.52
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141470        |
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
| r_version     |    102041 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102041 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102041 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12015 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-02 | 2023-06-28 | 1653 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133029.05 | 1517227.08 | 355 | 14701 | 289681 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10428.64 | 18577.11 | 1 | 279 | 3039 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-02 21:06:50 | 2023-06-28 19:54:18 | 86606 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     47 |       60 |

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
    ## -147.85  -35.80  -10.90   26.78  814.47 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.597e+02  6.854e+01
    ## date                                                9.950e-03  3.632e-03
    ## lag(value, 1)                                       1.044e-01  2.437e-02
    ## lag(value, 7)                                       9.617e-02  2.525e-02
    ## lag(value, 14)                                      9.003e-02  2.524e-02
    ## lag(value, 21)                                      6.971e-02  2.532e-02
    ## lag(value, 28)                                      7.246e-02  2.524e-02
    ## lag(value, 35)                                      6.526e-02  2.533e-02
    ## lag(value, 42)                                      4.848e-02  2.545e-02
    ## lag(value, 49)                                      7.011e-02  2.532e-02
    ## month(date, label = TRUE).L                        -9.608e+00  5.109e+00
    ## month(date, label = TRUE).Q                         3.911e+00  5.130e+00
    ## month(date, label = TRUE).C                        -1.344e+01  5.132e+00
    ## month(date, label = TRUE)^4                        -7.134e+00  5.176e+00
    ## month(date, label = TRUE)^5                        -1.103e+01  5.118e+00
    ## month(date, label = TRUE)^6                        -3.392e+00  5.223e+00
    ## month(date, label = TRUE)^7                        -7.510e+00  5.098e+00
    ## month(date, label = TRUE)^8                        -3.369e+00  5.105e+00
    ## month(date, label = TRUE)^9                         5.867e+00  5.118e+00
    ## month(date, label = TRUE)^10                        2.896e+00  5.085e+00
    ## month(date, label = TRUE)^11                       -4.529e+00  5.226e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.160e+01  2.334e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.970e+00  2.457e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.331 0.019901 *  
    ## date                                                 2.740 0.006215 ** 
    ## lag(value, 1)                                        4.282 1.96e-05 ***
    ## lag(value, 7)                                        3.808 0.000145 ***
    ## lag(value, 14)                                       3.567 0.000372 ***
    ## lag(value, 21)                                       2.753 0.005971 ** 
    ## lag(value, 28)                                       2.871 0.004150 ** 
    ## lag(value, 35)                                       2.577 0.010054 *  
    ## lag(value, 42)                                       1.905 0.056995 .  
    ## lag(value, 49)                                       2.769 0.005684 ** 
    ## month(date, label = TRUE).L                         -1.881 0.060204 .  
    ## month(date, label = TRUE).Q                          0.763 0.445865    
    ## month(date, label = TRUE).C                         -2.619 0.008904 ** 
    ## month(date, label = TRUE)^4                         -1.378 0.168321    
    ## month(date, label = TRUE)^5                         -2.156 0.031242 *  
    ## month(date, label = TRUE)^6                         -0.650 0.516109    
    ## month(date, label = TRUE)^7                         -1.473 0.140904    
    ## month(date, label = TRUE)^8                         -0.660 0.509394    
    ## month(date, label = TRUE)^9                          1.146 0.251812    
    ## month(date, label = TRUE)^10                         0.569 0.569151    
    ## month(date, label = TRUE)^11                        -0.867 0.386289    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.967 7.52e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.245 0.001201 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.72 on 1581 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2401, Adjusted R-squared:  0.2295 
    ## F-statistic:  22.7 on 22 and 1581 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.88463709567801"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.35792050341567"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.35792050341567"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.41373231730272"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.41373231730272"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.55225301743072"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.55225301743072"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.84073801055338"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.84073801055338"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.02194045685669"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.02194045685669"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.74178694671208"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.74178694671208"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.49223681082483"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.12414725614837"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.1092549591734"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.1092549591734"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.44662923811917"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.44662923811917"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.10679192106332"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.10679192106332"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.46114400384241"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.95324600410906"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.85144885784515"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.85144885784515"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.76359539252261"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.76359539252261"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.30655944776651"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.30655944776651"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.34223732536725"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.34223732536725"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.28964889085494"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.28964889085494"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.96557308451182"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.96557308451182"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.28607976044058"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.28607976044058"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.48704348685876"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.48704348685876"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.2408546189786"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.2408546189786"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.533572573738"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.19838515930761"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.19838515930761"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.72694017408794"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.72694017408794"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.62954511459514"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.62954511459514"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.7690364390738"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.7690364390738"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.63410859556889"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.63410859556889"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.72249534783654"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.72249534783654"

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
    ## 1 healthyR.data <tibble [1,645 × 2]> <tibble [28 × 2]> <split [1617|28]>
    ## 2 healthyR      <tibble [1,639 × 2]> <tibble [28 × 2]> <split [1611|28]>
    ## 3 healthyR.ts   <tibble [1,583 × 2]> <tibble [28 × 2]> <split [1555|28]>
    ## 4 healthyverse  <tibble [1,553 × 2]> <tibble [28 × 2]> <split [1525|28]>
    ## 5 healthyR.ai   <tibble [1,378 × 2]> <tibble [28 × 2]> <split [1350|28]>
    ## 6 TidyDensity   <tibble [1,229 × 2]> <tibble [28 × 2]> <split [1201|28]>
    ## 7 tidyAML       <tibble [837 × 2]>   <tibble [28 × 2]> <split [809|28]> 
    ## 8 RandomWalker  <tibble [259 × 2]>   <tibble [28 × 2]> <split [231|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6931305 | 144.33459 | 0.6953019 | 121.90303 | 0.9193178 | 0.0025436 |
| healthyR.data | 2 | LM | Test | 0.7052501 | 172.80961 | 0.7074595 | 119.08231 | 0.9150544 | 0.1487860 |
| healthyR.data | 3 | EARTH | Test | 0.7201072 | 258.31099 | 0.7223630 | 105.36991 | 0.9294005 | 0.1487860 |
| healthyR.data | 4 | NNAR | Test | 0.8053078 | 96.86495 | 0.8078306 | 171.67137 | 1.0359259 | 0.0906151 |
| healthyR | 1 | ARIMA | Test | 0.7123363 | 120.31852 | 0.7329470 | 152.09028 | 0.9041553 | 0.1022590 |
| healthyR | 2 | LM | Test | 0.7128401 | 96.10820 | 0.7334653 | 164.96184 | 0.9063451 | 0.1055880 |
| healthyR | 3 | EARTH | Test | 0.6776206 | 131.38440 | 0.6972267 | 137.26671 | 0.8404438 | 0.1055880 |
| healthyR | 4 | NNAR | Test | 0.7024733 | 99.33886 | 0.7227985 | 151.10679 | 0.8929052 | 0.1026075 |
| healthyR.ts | 1 | ARIMA | Test | 0.6160349 | 124.24480 | 0.6667425 | 130.72418 | 0.8286524 | 0.1313269 |
| healthyR.ts | 2 | LM | Test | 0.8945667 | 214.48583 | 0.9682010 | 158.02362 | 1.1095031 | 0.1313269 |
| healthyR.ts | 3 | EARTH | Test | 0.6505969 | 273.98836 | 0.7041494 | 101.44281 | 0.8604683 | 0.1313269 |
| healthyR.ts | 4 | NNAR | Test | 0.6953111 | 104.25512 | 0.7525441 | 179.08332 | 0.9030616 | 0.0037468 |
| healthyverse | 1 | ARIMA | Test | 0.6245999 | 115.79147 | 1.0383392 | 78.80693 | 0.8235719 | 0.0014794 |
| healthyverse | 2 | LM | Test | 0.6199616 | 145.00669 | 1.0306286 | 76.79516 | 0.7657077 | 0.0812440 |
| healthyverse | 3 | EARTH | Test | 0.6062504 | 163.22046 | 1.0078349 | 73.80705 | 0.7278195 | 0.0812440 |
| healthyverse | 4 | NNAR | Test | 0.7202673 | 96.63274 | 1.1973773 | 97.49243 | 0.9252985 | 0.0538675 |
| healthyR.ai | 1 | ARIMA | Test | 0.5747951 | 85.46252 | 0.8405042 | 153.76614 | 0.7459937 | 0.0028626 |
| healthyR.ai | 2 | LM | Test | 0.5319113 | 89.94716 | 0.7777965 | 120.72955 | 0.7026970 | 0.1213252 |
| healthyR.ai | 3 | EARTH | Test | 3.6505401 | 1401.69143 | 5.3380656 | 152.97690 | 4.0628651 | 0.1213252 |
| healthyR.ai | 4 | NNAR | Test | 0.5545119 | 79.08962 | 0.8108447 | 129.92108 | 0.7286215 | 0.0001134 |
| TidyDensity | 1 | ARIMA | Test | 0.4524450 | 178.74009 | 0.9798372 | 111.26743 | 0.5951852 | 0.1195122 |
| TidyDensity | 2 | LM | Test | 0.6143568 | 293.79291 | 1.3304814 | 118.79351 | 0.7619753 | 0.1081495 |
| TidyDensity | 3 | EARTH | Test | 0.4363865 | 175.50069 | 0.9450602 | 107.64577 | 0.5801352 | 0.1081495 |
| TidyDensity | 4 | NNAR | Test | 0.3990662 | 122.69827 | 0.8642375 | 125.52563 | 0.5024769 | 0.0165722 |
| tidyAML | 1 | ARIMA | Test | 0.8447379 | 140.05278 | 0.8703817 | 108.61453 | 1.1168414 | 0.0009521 |
| tidyAML | 2 | LM | Test | 0.8226718 | 129.02460 | 0.8476458 | 106.15734 | 1.1116453 | 0.0503943 |
| tidyAML | 3 | EARTH | Test | 1.5382389 | 301.07002 | 1.5849354 | 127.23319 | 1.7905332 | 0.0503943 |
| tidyAML | 4 | NNAR | Test | 0.8125827 | 129.08943 | 0.8372504 | 107.68493 | 1.0865776 | 0.0225711 |
| RandomWalker | 1 | ARIMA | Test | 1.1010649 | 98.53039 | 0.5802400 | 161.65099 | 1.3410369 | 0.1322393 |
| RandomWalker | 2 | LM | Test | 1.1584380 | 100.39057 | 0.6104745 | 195.55859 | 1.4101727 | 0.0732444 |
| RandomWalker | 3 | EARTH | Test | 1.1463038 | 100.69738 | 0.6040800 | 166.95306 | 1.4234371 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2853870 | 234.99384 | 0.6773742 | 160.14151 | 1.4243427 | 0.0040605 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         2 LM          Test  0.705 173.  0.707 119.  0.915 0.149 
    ## 2 healthyR              3 EARTH       Test  0.678 131.  0.697 137.  0.840 0.106 
    ## 3 healthyR.ts           1 ARIMA       Test  0.616 124.  0.667 131.  0.829 0.131 
    ## 4 healthyverse          3 EARTH       Test  0.606 163.  1.01   73.8 0.728 0.0812
    ## 5 healthyR.ai           2 LM          Test  0.532  89.9 0.778 121.  0.703 0.121 
    ## 6 TidyDensity           4 NNAR        Test  0.399 123.  0.864 126.  0.502 0.0166
    ## 7 tidyAML               4 NNAR        Test  0.813 129.  0.837 108.  1.09  0.0226
    ## 8 RandomWalker          1 ARIMA       Test  1.10   98.5 0.580 162.  1.34  0.132

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1617|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1611|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1555|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1525|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1350|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1201|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [809|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [231|28]>  <mdl_tm_t [1 × 5]>

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
