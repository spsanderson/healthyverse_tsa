Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,898
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

The last day in the data set is 2025-03-22 22:24:18, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5458.81
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134898        |
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
| r_version     |     96920 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96920 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96920 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11353 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-22 | 2023-05-22 | 1581 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135461.34 | 1524991.1 | 355 | 14701 | 261074 | 2367777 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10368.64 | 18354.7 | 1 | 303 | 3080 | 11785 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-22 22:24:18 | 2023-05-22 22:48:44 | 81951 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      3 |       60 |

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
    ## -152.55  -35.21  -10.18   26.65  811.09 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.919e+02  7.432e+01
    ## date                                                1.156e-02  3.943e-03
    ## lag(value, 1)                                       1.106e-01  2.490e-02
    ## lag(value, 7)                                       9.262e-02  2.578e-02
    ## lag(value, 14)                                      9.651e-02  2.581e-02
    ## lag(value, 21)                                      6.496e-02  2.589e-02
    ## lag(value, 28)                                      6.123e-02  2.578e-02
    ## lag(value, 35)                                      6.965e-02  2.598e-02
    ## lag(value, 42)                                      5.247e-02  2.605e-02
    ## lag(value, 49)                                      8.490e-02  2.600e-02
    ## month(date, label = TRUE).L                        -1.103e+01  5.152e+00
    ## month(date, label = TRUE).Q                         2.536e+00  5.188e+00
    ## month(date, label = TRUE).C                        -1.186e+01  5.244e+00
    ## month(date, label = TRUE)^4                        -7.414e+00  5.203e+00
    ## month(date, label = TRUE)^5                        -1.227e+01  5.200e+00
    ## month(date, label = TRUE)^6                        -2.795e+00  5.279e+00
    ## month(date, label = TRUE)^7                        -6.778e+00  5.169e+00
    ## month(date, label = TRUE)^8                        -4.384e+00  5.200e+00
    ## month(date, label = TRUE)^9                         5.564e+00  5.256e+00
    ## month(date, label = TRUE)^10                        4.478e+00  5.298e+00
    ## month(date, label = TRUE)^11                       -5.985e+00  5.325e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.186e+01  2.393e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.931e+00  2.523e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.582 0.009904 ** 
    ## date                                                 2.932 0.003415 ** 
    ## lag(value, 1)                                        4.440 9.65e-06 ***
    ## lag(value, 7)                                        3.592 0.000338 ***
    ## lag(value, 14)                                       3.740 0.000191 ***
    ## lag(value, 21)                                       2.509 0.012197 *  
    ## lag(value, 28)                                       2.376 0.017648 *  
    ## lag(value, 35)                                       2.681 0.007411 ** 
    ## lag(value, 42)                                       2.014 0.044157 *  
    ## lag(value, 49)                                       3.266 0.001116 ** 
    ## month(date, label = TRUE).L                         -2.141 0.032398 *  
    ## month(date, label = TRUE).Q                          0.489 0.625012    
    ## month(date, label = TRUE).C                         -2.262 0.023869 *  
    ## month(date, label = TRUE)^4                         -1.425 0.154370    
    ## month(date, label = TRUE)^5                         -2.360 0.018393 *  
    ## month(date, label = TRUE)^6                         -0.530 0.596495    
    ## month(date, label = TRUE)^7                         -1.311 0.189993    
    ## month(date, label = TRUE)^8                         -0.843 0.399293    
    ## month(date, label = TRUE)^9                          1.059 0.289996    
    ## month(date, label = TRUE)^10                         0.845 0.398120    
    ## month(date, label = TRUE)^11                        -1.124 0.261182    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.956 8.00e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.144 0.001697 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.46 on 1509 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2565, Adjusted R-squared:  0.2457 
    ## F-statistic: 23.66 on 22 and 1509 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.64566868957126"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.31173763978783"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2783979827204"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.2783979827204"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.06188819104469"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.06188819104469"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.58341601499856"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.58341601499856"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.34339013430963"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16720231307007"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.11463493462976"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.04581301664912"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 49, 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.01816454692225"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77, 49, 70, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.00299723546865"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 77, 49, 70, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.00299723546865"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 77, 49, 70, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.2559839260141"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 77, 49, 70, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.2559839260141"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 77, 49, 70, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.05956673614401"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 77, 49, 70, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.05956673614401"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2607241839746"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.03928200309327"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.03928200309327"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.61997571595388"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.61997571595388"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.25072632187077"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.25072632187077"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45843465847814"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.97229035126424"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.97229035126424"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.4792897196958"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.4792897196958"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.11931150320426"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.11931150320426"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.5323356890102"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.40541592877905"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37138175640094"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.37138175640094"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.71204756657581"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.71204756657581"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.86253894164445"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.86253894164445"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.16464769007256"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.16464769007256"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.24988016828474"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.24988016828474"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.77980323955255"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.77980323955255"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.1866620039971"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.98142084363908"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.98142084363908"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.86418150338858"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.86418150338858"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.50615501447343"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.50615501447343"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.71028106359317"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.43262737694831"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.43262737694831"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.26967921418079"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.26967921418079"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.68786887977814"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.68786887977814"

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
    ## 1 healthyR.data <tibble [1,574 × 2]> <tibble [28 × 2]> <split [1546|28]>
    ## 2 healthyR      <tibble [1,567 × 2]> <tibble [28 × 2]> <split [1539|28]>
    ## 3 healthyR.ts   <tibble [1,511 × 2]> <tibble [28 × 2]> <split [1483|28]>
    ## 4 healthyverse  <tibble [1,482 × 2]> <tibble [28 × 2]> <split [1454|28]>
    ## 5 healthyR.ai   <tibble [1,306 × 2]> <tibble [28 × 2]> <split [1278|28]>
    ## 6 TidyDensity   <tibble [1,157 × 2]> <tibble [28 × 2]> <split [1129|28]>
    ## 7 tidyAML       <tibble [765 × 2]>   <tibble [28 × 2]> <split [737|28]> 
    ## 8 RandomWalker  <tibble [187 × 2]>   <tibble [28 × 2]> <split [159|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6657191 | 113.79190 | 0.6942113 | 160.03041 | 0.8689234 | 0.0893179 |
| healthyR.data | 2 | LM | Test | 0.6976380 | 203.98298 | 0.7274963 | 136.89870 | 0.8289163 | 0.0016660 |
| healthyR.data | 3 | EARTH | Test | 0.9148576 | 233.22009 | 0.9540127 | 142.44485 | 1.1557369 | 0.0016660 |
| healthyR.data | 4 | NNAR | Test | 0.6923668 | 101.39324 | 0.7219995 | 154.45081 | 0.9091037 | 0.0319770 |
| healthyR | 1 | ARIMA | Test | 0.6873944 | 181.85882 | 0.7942290 | 160.73178 | 0.8129518 | 0.0160497 |
| healthyR | 2 | LM | Test | 0.6256083 | 102.58473 | 0.7228401 | 184.81742 | 0.7847954 | 0.0145255 |
| healthyR | 3 | EARTH | Test | 1.5103912 | 589.88423 | 1.7451355 | 156.08733 | 1.7061466 | 0.0145255 |
| healthyR | 4 | NNAR | Test | 0.6328728 | 224.75363 | 0.7312336 | 156.17435 | 0.7642235 | 0.0595330 |
| healthyR.ts | 1 | ARIMA | Test | 0.9435047 | 288.02930 | 0.7509163 | 130.38321 | 1.1379356 | 0.0615046 |
| healthyR.ts | 2 | LM | Test | 0.9193995 | 254.81430 | 0.7317314 | 132.27296 | 1.1159103 | 0.0615046 |
| healthyR.ts | 3 | EARTH | Test | 0.9168190 | 250.65614 | 0.7296777 | 132.47033 | 1.1135561 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8545765 | 94.64348 | 0.6801401 | 156.93708 | 1.0801993 | 0.0392119 |
| healthyverse | 1 | ARIMA | Test | 0.6283589 | 166.98044 | 0.9379338 | 108.88945 | 0.7714902 | 0.0279581 |
| healthyverse | 2 | LM | Test | 0.6063592 | 253.02614 | 0.9050955 | 91.23140 | 0.7268579 | 0.0022115 |
| healthyverse | 3 | EARTH | Test | 0.7473056 | 92.31284 | 1.1154823 | 164.11768 | 0.9370861 | 0.0022115 |
| healthyverse | 4 | NNAR | Test | 0.6238321 | 145.86446 | 0.9311769 | 109.66222 | 0.7875151 | 0.0571952 |
| healthyR.ai | 1 | ARIMA | Test | 0.7558148 | 129.72447 | 0.8359178 | 180.22734 | 0.8736610 | 0.0947851 |
| healthyR.ai | 2 | LM | Test | 0.6992854 | 110.98568 | 0.7733973 | 144.83709 | 0.8576670 | 0.0005690 |
| healthyR.ai | 3 | EARTH | Test | 1.9185679 | 841.46021 | 2.1219023 | 157.99828 | 2.1485134 | 0.0005690 |
| healthyR.ai | 4 | NNAR | Test | 0.7048067 | 127.17289 | 0.7795038 | 147.81384 | 0.8270439 | 0.1172128 |
| TidyDensity | 1 | ARIMA | Test | 0.5995371 | 240.90179 | 0.7208420 | 105.25128 | 0.7427163 | 0.0000031 |
| TidyDensity | 2 | LM | Test | 0.6271481 | 291.90464 | 0.7540395 | 104.99987 | 0.7646248 | 0.0131573 |
| TidyDensity | 3 | EARTH | Test | 0.5912969 | 223.26254 | 0.7109345 | 108.95187 | 0.7228675 | 0.0131573 |
| TidyDensity | 4 | NNAR | Test | 0.6020937 | 156.19036 | 0.7239158 | 128.53885 | 0.7557833 | 0.0210237 |
| tidyAML | 1 | ARIMA | Test | 0.6294396 | 281.28939 | 0.7403550 | 99.27809 | 0.7584282 | 0.0005076 |
| tidyAML | 2 | LM | Test | 0.6143593 | 273.32278 | 0.7226174 | 96.79290 | 0.7290000 | 0.0322826 |
| tidyAML | 3 | EARTH | Test | 0.5997643 | 112.93182 | 0.7054505 | 116.06964 | 0.7955801 | 0.0322826 |
| tidyAML | 4 | NNAR | Test | 0.6085692 | 236.30656 | 0.7158069 | 99.74162 | 0.7362441 | 0.0009640 |
| RandomWalker | 1 | ARIMA | Test | 0.9403811 | 108.77528 | 0.5458033 | 89.82894 | 1.3242607 | 0.1611767 |
| RandomWalker | 2 | LM | Test | 1.2303373 | 110.30448 | 0.7140958 | 193.81726 | 1.3492096 | 0.0244601 |
| RandomWalker | 3 | EARTH | Test | 1.1499997 | 89.88985 | 0.6674673 | 164.13999 | 1.3133626 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2904429 | 152.95061 | 0.7489815 | 175.24262 | 1.3773245 | 0.0206017 |

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
    ## 1 healthyR.d…         2 LM          Test  0.698 204.  0.727 137.  0.829  0.00167
    ## 2 healthyR            4 NNAR        Test  0.633 225.  0.731 156.  0.764  0.0595 
    ## 3 healthyR.ts         4 NNAR        Test  0.855  94.6 0.680 157.  1.08   0.0392 
    ## 4 healthyver…         2 LM          Test  0.606 253.  0.905  91.2 0.727  0.00221
    ## 5 healthyR.ai         4 NNAR        Test  0.705 127.  0.780 148.  0.827  0.117  
    ## 6 TidyDensity         3 EARTH       Test  0.591 223.  0.711 109.  0.723  0.0132 
    ## 7 tidyAML             2 LM          Test  0.614 273.  0.723  96.8 0.729  0.0323 
    ## 8 RandomWalk…         3 EARTH       Test  1.15   89.9 0.667 164.  1.31  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1546|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1539|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1483|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1454|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1278|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1129|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [737|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [159|28]>  <mdl_tm_t [1 × 5]>

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
