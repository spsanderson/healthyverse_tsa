Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 143,517
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

The last day in the data set is 2025-06-22 23:44:22, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7668.14 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 143517        |
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
| r_version     |    103683 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    103683 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    103683 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12136 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-22 | 2023-07-10 | 1673 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1132771.22 | 1515554.55 | 355 | 14701 | 289858 | 2367703 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10439.84 | 18555.53 | 1 | 295 | 3046 | 11827 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-22 23:44:22 | 2023-07-10 14:42:53 | 87989 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 30S |       60 |

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
    ## -148.24  -35.95  -11.46   26.83  816.16 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.767e+02  6.713e+01
    ## date                                                1.087e-02  3.555e-03
    ## lag(value, 1)                                       1.050e-01  2.424e-02
    ## lag(value, 7)                                       9.542e-02  2.523e-02
    ## lag(value, 14)                                      8.508e-02  2.522e-02
    ## lag(value, 21)                                      6.619e-02  2.529e-02
    ## lag(value, 28)                                      7.114e-02  2.518e-02
    ## lag(value, 35)                                      6.809e-02  2.524e-02
    ## lag(value, 42)                                      5.514e-02  2.535e-02
    ## lag(value, 49)                                      6.276e-02  2.523e-02
    ## month(date, label = TRUE).L                        -9.722e+00  5.125e+00
    ## month(date, label = TRUE).Q                         3.173e+00  5.101e+00
    ## month(date, label = TRUE).C                        -1.322e+01  5.144e+00
    ## month(date, label = TRUE)^4                        -6.599e+00  5.155e+00
    ## month(date, label = TRUE)^5                        -1.134e+01  5.128e+00
    ## month(date, label = TRUE)^6                        -4.178e+00  5.201e+00
    ## month(date, label = TRUE)^7                        -7.046e+00  5.096e+00
    ## month(date, label = TRUE)^8                        -2.806e+00  5.090e+00
    ## month(date, label = TRUE)^9                         5.144e+00  5.091e+00
    ## month(date, label = TRUE)^10                        2.481e+00  5.081e+00
    ## month(date, label = TRUE)^11                       -3.480e+00  5.124e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.194e+01  2.324e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.312e+00  2.446e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.633 0.008553 ** 
    ## date                                                 3.057 0.002272 ** 
    ## lag(value, 1)                                        4.332 1.57e-05 ***
    ## lag(value, 7)                                        3.782 0.000161 ***
    ## lag(value, 14)                                       3.373 0.000761 ***
    ## lag(value, 21)                                       2.617 0.008958 ** 
    ## lag(value, 28)                                       2.825 0.004789 ** 
    ## lag(value, 35)                                       2.698 0.007057 ** 
    ## lag(value, 42)                                       2.175 0.029759 *  
    ## lag(value, 49)                                       2.488 0.012963 *  
    ## month(date, label = TRUE).L                         -1.897 0.058000 .  
    ## month(date, label = TRUE).Q                          0.622 0.534021    
    ## month(date, label = TRUE).C                         -2.570 0.010250 *  
    ## month(date, label = TRUE)^4                         -1.280 0.200738    
    ## month(date, label = TRUE)^5                         -2.212 0.027093 *  
    ## month(date, label = TRUE)^6                         -0.803 0.421950    
    ## month(date, label = TRUE)^7                         -1.382 0.167014    
    ## month(date, label = TRUE)^8                         -0.551 0.581588    
    ## month(date, label = TRUE)^9                          1.010 0.312430    
    ## month(date, label = TRUE)^10                         0.488 0.625383    
    ## month(date, label = TRUE)^11                        -0.679 0.497063    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.138 3.11e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.398 0.000695 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.91 on 1601 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2381, Adjusted R-squared:  0.2277 
    ## F-statistic: 22.75 on 22 and 1601 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.66994888020717"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.42533751770922"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.42533751770922"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.60727473336338"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.60727473336338"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.08974902514664"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.08974902514664"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.78922233869601"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.63208406773829"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.63208406773829"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.0540943555194"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 10.0540943555194"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.65562053307408"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.65562053307408"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.88586765967406"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.88586765967406"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.40622713743649"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.40622713743649"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.27320695963454"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.27320695963454"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.70566048410166"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.70566048410166"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.99576110735578"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.99576110735578"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.00788019031387"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.00788019031387"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.77031438682172"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.77031438682172"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.53228048080319"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.53228048080319"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.19350067990673"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.19350067990673"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93213946781724"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.83131470598425"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.83131470598425"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.00392571337365"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.00392571337365"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.75788167190801"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.75788167190801"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.16296123160219"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.8724758874435"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.8724758874435"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.07099809535809"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.07099809535809"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.69413068431427"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.69413068431427"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.81315799696289"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.81315799696289"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.85093065497493"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.85093065497493"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.69959217092362"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.69959217092362"

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
    ## 1 healthyR.data <tibble [1,665 × 2]> <tibble [28 × 2]> <split [1637|28]>
    ## 2 healthyR      <tibble [1,659 × 2]> <tibble [28 × 2]> <split [1631|28]>
    ## 3 healthyR.ts   <tibble [1,603 × 2]> <tibble [28 × 2]> <split [1575|28]>
    ## 4 healthyverse  <tibble [1,573 × 2]> <tibble [28 × 2]> <split [1545|28]>
    ## 5 healthyR.ai   <tibble [1,398 × 2]> <tibble [28 × 2]> <split [1370|28]>
    ## 6 TidyDensity   <tibble [1,249 × 2]> <tibble [28 × 2]> <split [1221|28]>
    ## 7 tidyAML       <tibble [857 × 2]>   <tibble [28 × 2]> <split [829|28]> 
    ## 8 RandomWalker  <tibble [279 × 2]>   <tibble [28 × 2]> <split [251|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7052789 | 142.42358 | 0.7655845 | 136.95958 | 0.9154539 | 0.0006375 |
| healthyR.data | 2 | LM | Test | 0.7260043 | 183.45731 | 0.7880820 | 128.62172 | 0.9078559 | 0.2422164 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7381798 | 96.92701 | 0.8012987 | 181.51944 | 0.9722586 | 0.0120428 |
| healthyR | 1 | ARIMA | Test | 0.8880766 | 103.68690 | 0.9966737 | 173.36915 | 1.0349142 | 0.0006216 |
| healthyR | 2 | LM | Test | 0.8906444 | 100.67207 | 0.9995555 | 182.96611 | 1.0427832 | 0.2537049 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.8752933 | 101.77672 | 0.9823272 | 170.58581 | 1.0174544 | 0.0327732 |
| healthyR.ts | 1 | ARIMA | Test | 0.9862476 | 102.26188 | 0.7256972 | 154.26138 | 1.2395236 | 0.0917035 |
| healthyR.ts | 2 | LM | Test | 1.0940111 | 156.41765 | 0.8049914 | 156.90103 | 1.3280513 | 0.2342400 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9968361 | 106.07049 | 0.7334884 | 187.11508 | 1.2606182 | 0.0559358 |
| healthyverse | 1 | ARIMA | Test | 0.7793799 | 136.77900 | 1.1667480 | 91.73341 | 0.9510982 | 0.0094113 |
| healthyverse | 2 | LM | Test | 0.7610268 | 155.14488 | 1.1392730 | 87.39100 | 0.9151644 | 0.1492424 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.8481788 | 106.97342 | 1.2697414 | 107.98039 | 1.0266774 | 0.0056801 |
| healthyR.ai | 1 | ARIMA | Test | 0.7283422 | 98.73096 | 0.7505573 | 155.16380 | 0.8745100 | 0.0089004 |
| healthyR.ai | 2 | LM | Test | 0.7306562 | 107.88574 | 0.7529419 | 152.92772 | 0.8683458 | 0.1336733 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.7231375 | 103.62839 | 0.7451939 | 144.81820 | 0.8621973 | 0.0039286 |
| TidyDensity | 1 | ARIMA | Test | 0.5589531 | 90.45410 | 0.8480884 | 108.51016 | 0.7924370 | 0.0206718 |
| TidyDensity | 2 | LM | Test | 0.6579365 | 149.57612 | 0.9982741 | 104.16037 | 0.8793546 | 0.0967960 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5587659 | 76.30147 | 0.8478045 | 117.16690 | 0.8021660 | 0.0020770 |
| tidyAML | 1 | ARIMA | Test | 0.8318694 | 101.05421 | 0.8594586 | 133.36335 | 1.0396479 | 0.0120230 |
| tidyAML | 2 | LM | Test | 0.6720381 | 120.80966 | 0.6943264 | 88.98040 | 0.8448809 | 0.0002641 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.7021410 | 106.96999 | 0.7254278 | 98.18299 | 0.9075174 | 0.0153075 |
| RandomWalker | 1 | ARIMA | Test | 1.2897931 | 141.77275 | 0.7301831 | 153.49932 | 1.4641992 | 0.0025947 |
| RandomWalker | 2 | LM | Test | 1.2651507 | 111.10546 | 0.7162324 | 189.83759 | 1.3863941 | 0.0083418 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.3098987 | 158.36882 | 0.7415653 | 146.07504 | 1.5007930 | 0.0224883 |

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
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         2 LM          Test  0.726 183.  0.788 129.  0.908 2.42e-1
    ## 2 healthyR             4 NNAR        Test  0.875 102.  0.982 171.  1.02  3.28e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.986 102.  0.726 154.  1.24  9.17e-2
    ## 4 healthyverse         2 LM          Test  0.761 155.  1.14   87.4 0.915 1.49e-1
    ## 5 healthyR.ai          4 NNAR        Test  0.723 104.  0.745 145.  0.862 3.93e-3
    ## 6 TidyDensity          1 ARIMA       Test  0.559  90.5 0.848 109.  0.792 2.07e-2
    ## 7 tidyAML              2 LM          Test  0.672 121.  0.694  89.0 0.845 2.64e-4
    ## 8 RandomWalker         2 LM          Test  1.27  111.  0.716 190.  1.39  8.34e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1637|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1631|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1575|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1545|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1370|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1221|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [829|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [251|28]>  <mdl_tm_t [1 × 5]>

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
