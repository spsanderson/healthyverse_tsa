Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
15 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 136,892
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

The last day in the data set is 2025-04-13 17:07:54, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5981.54
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 136892        |
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
| r_version     |     98495 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     98495 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     98495 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11535 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-13 | 2023-06-01 | 1603 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134978.35 | 1522862.9 | 355 | 14701.00 | 274584 | 2367773 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10350.07 | 18368.6 | 1 | 299.75 | 3064 | 11642 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-13 17:07:54 | 2023-06-01 05:04:06 | 83293 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   20.5 |       60 |

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
    ## -148.22  -35.56  -10.67   26.65  812.99 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.816e+02  7.292e+01
    ## date                                                1.107e-02  3.868e-03
    ## lag(value, 1)                                       1.060e-01  2.474e-02
    ## lag(value, 7)                                       1.001e-01  2.569e-02
    ## lag(value, 14)                                      9.602e-02  2.574e-02
    ## lag(value, 21)                                      6.513e-02  2.590e-02
    ## lag(value, 28)                                      6.442e-02  2.576e-02
    ## lag(value, 35)                                      6.994e-02  2.582e-02
    ## lag(value, 42)                                      4.883e-02  2.593e-02
    ## lag(value, 49)                                      7.173e-02  2.577e-02
    ## month(date, label = TRUE).L                        -1.019e+01  5.144e+00
    ## month(date, label = TRUE).Q                         2.788e+00  5.204e+00
    ## month(date, label = TRUE).C                        -1.246e+01  5.224e+00
    ## month(date, label = TRUE)^4                        -6.565e+00  5.201e+00
    ## month(date, label = TRUE)^5                        -1.222e+01  5.210e+00
    ## month(date, label = TRUE)^6                        -3.156e+00  5.275e+00
    ## month(date, label = TRUE)^7                        -6.176e+00  5.171e+00
    ## month(date, label = TRUE)^8                        -4.430e+00  5.181e+00
    ## month(date, label = TRUE)^9                         5.318e+00  5.206e+00
    ## month(date, label = TRUE)^10                        4.624e+00  5.280e+00
    ## month(date, label = TRUE)^11                       -5.929e+00  5.343e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.141e+01  2.390e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.968e+00  2.514e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.491 0.012860 *  
    ## date                                                 2.861 0.004284 ** 
    ## lag(value, 1)                                        4.285 1.94e-05 ***
    ## lag(value, 7)                                        3.897 0.000102 ***
    ## lag(value, 14)                                       3.731 0.000198 ***
    ## lag(value, 21)                                       2.515 0.012004 *  
    ## lag(value, 28)                                       2.501 0.012482 *  
    ## lag(value, 35)                                       2.708 0.006838 ** 
    ## lag(value, 42)                                       1.883 0.059887 .  
    ## lag(value, 49)                                       2.783 0.005452 ** 
    ## month(date, label = TRUE).L                         -1.981 0.047769 *  
    ## month(date, label = TRUE).Q                          0.536 0.592235    
    ## month(date, label = TRUE).C                         -2.386 0.017172 *  
    ## month(date, label = TRUE)^4                         -1.262 0.207097    
    ## month(date, label = TRUE)^5                         -2.346 0.019126 *  
    ## month(date, label = TRUE)^6                         -0.598 0.549689    
    ## month(date, label = TRUE)^7                         -1.194 0.232506    
    ## month(date, label = TRUE)^8                         -0.855 0.392604    
    ## month(date, label = TRUE)^9                          1.021 0.307241    
    ## month(date, label = TRUE)^10                         0.876 0.381351    
    ## month(date, label = TRUE)^11                        -1.110 0.267358    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.774 1.98e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.170 0.001554 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.79 on 1531 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2472, Adjusted R-squared:  0.2364 
    ## F-statistic: 22.86 on 22 and 1531 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93395801321703"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.42157696761819"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.31450058313467"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.31450058313467"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.03173548194685"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.03173548194685"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.97643336492655"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.97643336492655"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.78443708771176"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.14489882532638"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.14489882532638"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.77719572440629"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.77719572440629"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.29388697403708"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.29388697403708"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.95012082198222"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.95012082198222"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.31444369141949"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.31444369141949"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.99045900180051"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.99045900180051"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.20736933371142"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.94513274688632"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.94513274688632"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.27103403254737"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.27103403254737"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.43729574340407"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.43729574340407"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.57528525774628"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.57528525774628"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.94397283531409"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.94397283531409"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.01823892611037"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.01823892611037"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.57788100100094"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.53838765142849"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 42, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.48095012504741"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 42, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.48095012504741"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 42, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.28636743182909"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 42, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.28636743182909"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 42, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.19103697823039"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 42, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.19103697823039"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.76641717465863"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.76641717465863"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.95750103143637"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.95750103143637"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.38190478178783"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.38190478178783"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.12006146397441"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.88811425324144"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.88811425324144"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.20434499493948"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.20434499493948"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.29352287320224"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.29352287320224"

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
    ## 1 healthyR.data <tibble [1,596 × 2]> <tibble [28 × 2]> <split [1568|28]>
    ## 2 healthyR      <tibble [1,589 × 2]> <tibble [28 × 2]> <split [1561|28]>
    ## 3 healthyR.ts   <tibble [1,533 × 2]> <tibble [28 × 2]> <split [1505|28]>
    ## 4 healthyverse  <tibble [1,504 × 2]> <tibble [28 × 2]> <split [1476|28]>
    ## 5 healthyR.ai   <tibble [1,328 × 2]> <tibble [28 × 2]> <split [1300|28]>
    ## 6 TidyDensity   <tibble [1,179 × 2]> <tibble [28 × 2]> <split [1151|28]>
    ## 7 tidyAML       <tibble [787 × 2]>   <tibble [28 × 2]> <split [759|28]> 
    ## 8 RandomWalker  <tibble [209 × 2]>   <tibble [28 × 2]> <split [181|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7672012 | 155.40633 | 0.6435930 | 165.49597 | 0.9153418 | 0.1686390 |
| healthyR.data | 2 | LM | Test | 0.6745886 | 152.78849 | 0.5659017 | 126.76951 | 0.8357532 | 0.0033035 |
| healthyR.data | 3 | EARTH | Test | 0.9492362 | 344.36225 | 0.7962992 | 117.75608 | 1.1692718 | 0.0033035 |
| healthyR.data | 4 | NNAR | Test | 0.7122148 | 111.16635 | 0.5974657 | 166.76754 | 0.9212006 | 0.0000689 |
| healthyR | 1 | ARIMA | Test | 0.7880546 | 107.69766 | 0.7240703 | 175.95815 | 0.9478026 | 0.0022531 |
| healthyR | 2 | LM | Test | 0.7529560 | 97.75306 | 0.6918215 | 187.40492 | 0.9145700 | 0.0233363 |
| healthyR | 3 | EARTH | Test | 0.7549063 | 98.21036 | 0.6936134 | 188.28906 | 0.9158943 | 0.0233363 |
| healthyR | 4 | NNAR | Test | 0.7882060 | 100.89225 | 0.7242094 | 156.40928 | 0.9550006 | 0.0190017 |
| healthyR.ts | 1 | ARIMA | Test | 0.9124734 | 282.02779 | 0.7330069 | 145.06404 | 1.1513826 | 0.0001686 |
| healthyR.ts | 2 | LM | Test | 0.9387756 | 317.47257 | 0.7541359 | 142.59123 | 1.1851844 | 0.0001686 |
| healthyR.ts | 3 | EARTH | Test | 0.9364233 | 314.28532 | 0.7522463 | 142.89928 | 1.1816121 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9019335 | 157.29452 | 0.7245400 | 167.91334 | 1.1701874 | 0.0264898 |
| healthyverse | 1 | ARIMA | Test | 0.6822182 | 263.31294 | 0.6931828 | 108.23968 | 0.8555973 | 0.0142116 |
| healthyverse | 2 | LM | Test | 0.6369846 | 352.62354 | 0.6472222 | 93.82387 | 0.8044417 | 0.0023370 |
| healthyverse | 3 | EARTH | Test | 0.6632897 | 222.79385 | 0.6739501 | 109.68155 | 0.8602746 | 0.0023370 |
| healthyverse | 4 | NNAR | Test | 0.6919772 | 217.08420 | 0.7030986 | 119.52158 | 0.8898375 | 0.0005224 |
| healthyR.ai | 1 | ARIMA | Test | 0.7643883 | 146.94967 | 0.7185890 | 179.06556 | 0.9825920 | 0.0008064 |
| healthyR.ai | 2 | LM | Test | 0.6891729 | 136.92779 | 0.6478802 | 142.73456 | 0.9073332 | 0.0007387 |
| healthyR.ai | 3 | EARTH | Test | 0.7039434 | 114.17874 | 0.6617657 | 155.44906 | 0.9275832 | 0.0007387 |
| healthyR.ai | 4 | NNAR | Test | 0.7667104 | 151.81077 | 0.7207719 | 148.33147 | 0.9963715 | 0.0054306 |
| TidyDensity | 1 | ARIMA | Test | 0.5777416 | 293.90456 | 0.6510785 | 112.97032 | 0.7123243 | 0.1127060 |
| TidyDensity | 2 | LM | Test | 0.6308858 | 364.85360 | 0.7109686 | 105.28741 | 0.8128040 | 0.0047528 |
| TidyDensity | 3 | EARTH | Test | 0.5882073 | 262.14157 | 0.6628727 | 111.67862 | 0.7458623 | 0.0047528 |
| TidyDensity | 4 | NNAR | Test | 0.5978328 | 234.17680 | 0.6737200 | 136.99965 | 0.7440094 | 0.0090273 |
| tidyAML | 1 | ARIMA | Test | 0.5664788 | 308.86466 | 0.6927481 | 89.55439 | 0.7323965 | 0.0231625 |
| tidyAML | 2 | LM | Test | 0.5753912 | 301.70643 | 0.7036471 | 92.77399 | 0.7263416 | 0.0037145 |
| tidyAML | 3 | EARTH | Test | 0.6042478 | 335.93674 | 0.7389358 | 93.52553 | 0.7536612 | 0.0037145 |
| tidyAML | 4 | NNAR | Test | 0.5554130 | 279.96326 | 0.6792157 | 91.86687 | 0.7129348 | 0.0032802 |
| RandomWalker | 1 | ARIMA | Test | 1.2162155 | 118.20491 | 0.6292455 | 165.47595 | 1.4295686 | 0.0130178 |
| RandomWalker | 2 | LM | Test | 1.1905312 | 95.55415 | 0.6159570 | 177.08864 | 1.3961530 | 0.0054189 |
| RandomWalker | 3 | EARTH | Test | 1.1770470 | 94.28958 | 0.6089805 | 163.84828 | 1.3940696 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4089784 | 192.30570 | 0.7289771 | 179.75299 | 1.5519306 | 0.0023773 |

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
    ## 1 healthyR.d…         2 LM          Test  0.675 153.  0.566 127.  0.836  3.30e-3
    ## 2 healthyR            2 LM          Test  0.753  97.8 0.692 187.  0.915  2.33e-2
    ## 3 healthyR.ts         1 ARIMA       Test  0.912 282.  0.733 145.  1.15   1.69e-4
    ## 4 healthyver…         2 LM          Test  0.637 353.  0.647  93.8 0.804  2.34e-3
    ## 5 healthyR.ai         2 LM          Test  0.689 137.  0.648 143.  0.907  7.39e-4
    ## 6 TidyDensity         1 ARIMA       Test  0.578 294.  0.651 113.  0.712  1.13e-1
    ## 7 tidyAML             4 NNAR        Test  0.555 280.  0.679  91.9 0.713  3.28e-3
    ## 8 RandomWalk…         3 EARTH       Test  1.18   94.3 0.609 164.  1.39  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1568|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1561|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1505|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1476|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1300|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1151|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [759|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [181|28]>  <mdl_tm_t [1 × 5]>

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
