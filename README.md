Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,880
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

The last day in the data set is 2025-04-22 23:37:36, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6204.03
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137880        |
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
| r_version     |     99253 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99253 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99253 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11625 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-22 | 2023-06-08 | 1612 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135284.18 | 1522002.66 | 355 | 14701 | 275485 | 2367773.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10370.01 | 18405.58 | 1 | 299 | 3061 | 11684.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-22 23:37:36 | 2023-06-08 02:12:39 | 84002 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     28 |       60 |

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
    ## -148.03  -35.55  -10.54   26.69  813.78 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.869e+02  7.215e+01
    ## date                                                1.136e-02  3.827e-03
    ## lag(value, 1)                                       1.052e-01  2.471e-02
    ## lag(value, 7)                                       9.563e-02  2.558e-02
    ## lag(value, 14)                                      9.717e-02  2.569e-02
    ## lag(value, 21)                                      6.838e-02  2.575e-02
    ## lag(value, 28)                                      6.546e-02  2.573e-02
    ## lag(value, 35)                                      6.732e-02  2.577e-02
    ## lag(value, 42)                                      4.929e-02  2.587e-02
    ## lag(value, 49)                                      6.827e-02  2.572e-02
    ## month(date, label = TRUE).L                        -1.025e+01  5.135e+00
    ## month(date, label = TRUE).Q                         2.726e+00  5.197e+00
    ## month(date, label = TRUE).C                        -1.232e+01  5.199e+00
    ## month(date, label = TRUE)^4                        -6.555e+00  5.198e+00
    ## month(date, label = TRUE)^5                        -1.234e+01  5.200e+00
    ## month(date, label = TRUE)^6                        -3.011e+00  5.253e+00
    ## month(date, label = TRUE)^7                        -6.264e+00  5.168e+00
    ## month(date, label = TRUE)^8                        -4.475e+00  5.170e+00
    ## month(date, label = TRUE)^9                         5.439e+00  5.172e+00
    ## month(date, label = TRUE)^10                        4.489e+00  5.254e+00
    ## month(date, label = TRUE)^11                       -5.861e+00  5.336e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.161e+01  2.379e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.144e+00  2.507e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.590 0.009692 ** 
    ## date                                                 2.969 0.003035 ** 
    ## lag(value, 1)                                        4.258 2.19e-05 ***
    ## lag(value, 7)                                        3.739 0.000191 ***
    ## lag(value, 14)                                       3.783 0.000161 ***
    ## lag(value, 21)                                       2.655 0.008005 ** 
    ## lag(value, 28)                                       2.545 0.011037 *  
    ## lag(value, 35)                                       2.613 0.009071 ** 
    ## lag(value, 42)                                       1.905 0.056962 .  
    ## lag(value, 49)                                       2.654 0.008026 ** 
    ## month(date, label = TRUE).L                         -1.996 0.046122 *  
    ## month(date, label = TRUE).Q                          0.525 0.599934    
    ## month(date, label = TRUE).C                         -2.371 0.017884 *  
    ## month(date, label = TRUE)^4                         -1.261 0.207493    
    ## month(date, label = TRUE)^5                         -2.373 0.017788 *  
    ## month(date, label = TRUE)^6                         -0.573 0.566561    
    ## month(date, label = TRUE)^7                         -1.212 0.225675    
    ## month(date, label = TRUE)^8                         -0.866 0.386800    
    ## month(date, label = TRUE)^9                          1.051 0.293202    
    ## month(date, label = TRUE)^10                         0.854 0.392991    
    ## month(date, label = TRUE)^11                        -1.098 0.272182    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.879 1.18e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.248 0.001186 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.78 on 1540 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2467, Adjusted R-squared:  0.2359 
    ## F-statistic: 22.92 on 22 and 1540 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.79441798940373"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.33775512332432"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.25297986718596"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.25297986718596"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.97085143499951"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.97085143499951"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.99335311234231"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.99335311234231"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0998400172476"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.76849907874905"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.6401564385161"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.57666686313794"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91, 84, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55827735870893"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 91, 84, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.55827735870893"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 91, 84, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.60313465327116"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 91, 84, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.60313465327116"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 91, 84, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.99023791026328"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 91, 84, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.99023791026328"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.26387150862029"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.72341813632987"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.72341813632987"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.72830585665486"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.72830585665486"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.021180899195"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.021180899195"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.51899118046503"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.05169885388388"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.05169885388388"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.30978536074624"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.30978536074624"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.49623685861096"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.49623685861096"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.85104829246428"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.85104829246428"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.73193703139628"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.73193703139628"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.17359265309578"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.17359265309578"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.6650861536416"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.6650861536416"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.70808163729129"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.70808163729129"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.94425175003311"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.94425175003311"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.6820413993655"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.6820413993655"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.48840780794971"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.48840780794971"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.14171315699472"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.14171315699472"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.3490363754644"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.13594562235118"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.13594562235118"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.50185366422185"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.50185366422185"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.50847104643056"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.50847104643056"

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
    ## 1 healthyR.data <tibble [1,605 × 2]> <tibble [28 × 2]> <split [1577|28]>
    ## 2 healthyR      <tibble [1,598 × 2]> <tibble [28 × 2]> <split [1570|28]>
    ## 3 healthyR.ts   <tibble [1,542 × 2]> <tibble [28 × 2]> <split [1514|28]>
    ## 4 healthyverse  <tibble [1,513 × 2]> <tibble [28 × 2]> <split [1485|28]>
    ## 5 healthyR.ai   <tibble [1,337 × 2]> <tibble [28 × 2]> <split [1309|28]>
    ## 6 TidyDensity   <tibble [1,188 × 2]> <tibble [28 × 2]> <split [1160|28]>
    ## 7 tidyAML       <tibble [796 × 2]>   <tibble [28 × 2]> <split [768|28]> 
    ## 8 RandomWalker  <tibble [218 × 2]>   <tibble [28 × 2]> <split [190|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8278041 | 101.79338 | 0.5732556 | 149.15154 | 0.9877413 | 0.0375735 |
| healthyR.data | 2 | LM | Test | 0.8176922 | 129.53799 | 0.5662532 | 136.36748 | 0.9564688 | 0.0245312 |
| healthyR.data | 3 | EARTH | Test | 0.9118484 | 192.73028 | 0.6314565 | 122.69225 | 1.0869266 | 0.0245312 |
| healthyR.data | 4 | NNAR | Test | 0.8479729 | 104.76077 | 0.5872226 | 182.09335 | 1.0135179 | 0.0288861 |
| healthyR | 1 | ARIMA | Test | 0.7208778 | 98.99890 | 0.7171484 | 181.47338 | 0.8934139 | 0.0215386 |
| healthyR | 2 | LM | Test | 0.7163982 | 98.80519 | 0.7126920 | 184.05014 | 0.8712612 | 0.0156096 |
| healthyR | 3 | EARTH | Test | 0.7165662 | 98.58673 | 0.7128592 | 165.42014 | 0.8665580 | 0.0156096 |
| healthyR | 4 | NNAR | Test | 0.7206109 | 98.27597 | 0.7168830 | 165.83874 | 0.8718878 | 0.0940896 |
| healthyR.ts | 1 | ARIMA | Test | 1.0087689 | 132.08327 | 0.7001500 | 150.08247 | 1.2204787 | 0.0138044 |
| healthyR.ts | 2 | LM | Test | 1.0323971 | 158.82292 | 0.7165495 | 135.98059 | 1.2877697 | 0.0138044 |
| healthyR.ts | 3 | EARTH | Test | 1.0717332 | 211.94072 | 0.7438513 | 137.12830 | 1.2641544 | 0.0138044 |
| healthyR.ts | 4 | NNAR | Test | 0.9840982 | 96.47118 | 0.6830270 | 185.00784 | 1.1654881 | 0.0709989 |
| healthyverse | 1 | ARIMA | Test | 0.7713346 | 217.30759 | 0.7719208 | 118.76001 | 0.9339833 | 0.0016288 |
| healthyverse | 2 | LM | Test | 0.7385656 | 302.88491 | 0.7391268 | 103.56769 | 0.8779980 | 0.0485269 |
| healthyverse | 3 | EARTH | Test | 0.7544233 | 188.41046 | 0.7549966 | 118.20524 | 0.9441769 | 0.0485269 |
| healthyverse | 4 | NNAR | Test | 0.7629690 | 178.31989 | 0.7635488 | 121.25820 | 0.9632857 | 0.0230418 |
| healthyR.ai | 1 | ARIMA | Test | 0.8032483 | 112.29696 | 0.7437279 | 187.54865 | 0.9859481 | 0.0686776 |
| healthyR.ai | 2 | LM | Test | 0.7540933 | 114.98724 | 0.6982152 | 153.57499 | 0.9218976 | 0.0658209 |
| healthyR.ai | 3 | EARTH | Test | 0.7615071 | 110.24232 | 0.7050796 | 159.30189 | 0.9312975 | 0.0658209 |
| healthyR.ai | 4 | NNAR | Test | 0.7832164 | 118.83592 | 0.7251803 | 156.42458 | 0.9573842 | 0.0437084 |
| TidyDensity | 1 | ARIMA | Test | 0.6426854 | 228.11435 | 0.7570447 | 118.27535 | 0.7653944 | 0.0276015 |
| TidyDensity | 2 | LM | Test | 0.6714210 | 289.63147 | 0.7908935 | 109.23275 | 0.8300349 | 0.0025664 |
| TidyDensity | 3 | EARTH | Test | 0.6458925 | 210.54112 | 0.7608225 | 119.59661 | 0.7734755 | 0.0025664 |
| TidyDensity | 4 | NNAR | Test | 0.6320027 | 126.92711 | 0.7444610 | 143.32347 | 0.7736407 | 0.0663013 |
| tidyAML | 1 | ARIMA | Test | 0.6020159 | 224.20146 | 0.8853458 | 103.82345 | 0.7138817 | 0.0252630 |
| tidyAML | 2 | LM | Test | 0.5936596 | 265.78750 | 0.8730567 | 93.04689 | 0.7438052 | 0.0277948 |
| tidyAML | 3 | EARTH | Test | 0.5859503 | 161.89293 | 0.8617192 | 113.21937 | 0.7400446 | 0.0277948 |
| tidyAML | 4 | NNAR | Test | 0.6089435 | 213.82671 | 0.8955337 | 109.04273 | 0.7291024 | 0.0071043 |
| RandomWalker | 1 | ARIMA | Test | 1.4305710 | 135.62885 | 0.6370207 | 152.75868 | 1.7170062 | 0.0872025 |
| RandomWalker | 2 | LM | Test | 1.3048571 | 106.09712 | 0.5810414 | 186.82326 | 1.5124895 | 0.0507525 |
| RandomWalker | 3 | EARTH | Test | 1.2977653 | 113.24036 | 0.5778835 | 175.61948 | 1.5160259 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3570856 | 157.16058 | 0.6042983 | 157.31692 | 1.5281698 | 0.0276416 |

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
    ## 1 healthyR.data         2 LM          Test  0.818 130.  0.566  136. 0.956 0.0245
    ## 2 healthyR              3 EARTH       Test  0.717  98.6 0.713  165. 0.867 0.0156
    ## 3 healthyR.ts           4 NNAR        Test  0.984  96.5 0.683  185. 1.17  0.0710
    ## 4 healthyverse          2 LM          Test  0.739 303.  0.739  104. 0.878 0.0485
    ## 5 healthyR.ai           2 LM          Test  0.754 115.  0.698  154. 0.922 0.0658
    ## 6 TidyDensity           1 ARIMA       Test  0.643 228.  0.757  118. 0.765 0.0276
    ## 7 tidyAML               1 ARIMA       Test  0.602 224.  0.885  104. 0.714 0.0253
    ## 8 RandomWalker          2 LM          Test  1.30  106.  0.581  187. 1.51  0.0508

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1577|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1570|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1514|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1485|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1309|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1160|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [768|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [190|28]>  <mdl_tm_t [1 × 5]>

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
