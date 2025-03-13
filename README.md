Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 133,870
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

The last day in the data set is 2025-03-11 23:52:49, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5196.28
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 133870        |
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
| r_version     |     96123 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96123 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96123 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11280 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-11 | 2023-05-17 | 1570 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136594.1 | 1526254.8 | 355 | 14701 | 260684 | 2367791.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10388.9 | 18374.8 | 1 | 299 | 3091 | 11847.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-11 23:52:49 | 2023-05-17 09:39:19 | 81240 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     31 |       60 |

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
    ## -153.79  -35.08  -10.08   26.70  810.62 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.032e+02  7.520e+01
    ## date                                                1.213e-02  3.989e-03
    ## lag(value, 1)                                       1.119e-01  2.500e-02
    ## lag(value, 7)                                       9.034e-02  2.589e-02
    ## lag(value, 14)                                      9.396e-02  2.590e-02
    ## lag(value, 21)                                      6.734e-02  2.609e-02
    ## lag(value, 28)                                      6.480e-02  2.598e-02
    ## lag(value, 35)                                      6.821e-02  2.617e-02
    ## lag(value, 42)                                      5.442e-02  2.629e-02
    ## lag(value, 49)                                      8.660e-02  2.622e-02
    ## month(date, label = TRUE).L                        -1.174e+01  5.180e+00
    ## month(date, label = TRUE).Q                         2.454e+00  5.191e+00
    ## month(date, label = TRUE).C                        -1.137e+01  5.264e+00
    ## month(date, label = TRUE)^4                        -8.155e+00  5.233e+00
    ## month(date, label = TRUE)^5                        -1.200e+01  5.206e+00
    ## month(date, label = TRUE)^6                        -2.517e+00  5.287e+00
    ## month(date, label = TRUE)^7                        -7.574e+00  5.208e+00
    ## month(date, label = TRUE)^8                        -3.546e+00  5.243e+00
    ## month(date, label = TRUE)^9                         4.939e+00  5.282e+00
    ## month(date, label = TRUE)^10                        4.870e+00  5.309e+00
    ## month(date, label = TRUE)^11                       -6.175e+00  5.329e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.193e+01  2.402e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.940e+00  2.533e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.702 0.006968 ** 
    ## date                                                 3.041 0.002400 ** 
    ## lag(value, 1)                                        4.477 8.15e-06 ***
    ## lag(value, 7)                                        3.489 0.000499 ***
    ## lag(value, 14)                                       3.628 0.000295 ***
    ## lag(value, 21)                                       2.581 0.009939 ** 
    ## lag(value, 28)                                       2.494 0.012734 *  
    ## lag(value, 35)                                       2.606 0.009247 ** 
    ## lag(value, 42)                                       2.070 0.038597 *  
    ## lag(value, 49)                                       3.303 0.000978 ***
    ## month(date, label = TRUE).L                         -2.267 0.023538 *  
    ## month(date, label = TRUE).Q                          0.473 0.636472    
    ## month(date, label = TRUE).C                         -2.160 0.030936 *  
    ## month(date, label = TRUE)^4                         -1.558 0.119373    
    ## month(date, label = TRUE)^5                         -2.305 0.021314 *  
    ## month(date, label = TRUE)^6                         -0.476 0.634069    
    ## month(date, label = TRUE)^7                         -1.454 0.146070    
    ## month(date, label = TRUE)^8                         -0.676 0.498917    
    ## month(date, label = TRUE)^9                          0.935 0.349897    
    ## month(date, label = TRUE)^10                         0.917 0.359100    
    ## month(date, label = TRUE)^11                        -1.159 0.246736    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.968 7.55e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.135 0.001751 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.49 on 1498 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2586, Adjusted R-squared:  0.2477 
    ## F-statistic: 23.75 on 22 and 1498 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.27376732175041"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.13958524497255"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.13958524497255"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.28833460672229"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.28833460672229"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.1287772027195"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.1287772027195"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.42848049091007"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.25819449457125"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16734244956954"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.16734244956954"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.97155931280371"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.97155931280371"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.44159334277731"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.44159334277731"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45707911572264"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.20088993456068"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.20088993456068"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.62211937860916"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.62211937860916"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.35332146844613"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.35332146844613"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.4473171551605"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.19964733218407"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.14383202913382"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.14206110748075"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.14206110748075"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.31497649417829"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.31497649417829"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.18606946933071"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.18606946933071"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.54143961620786"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.41408476472874"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.34665480539311"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.34665480539311"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.36717046355738"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.36717046355738"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.71002576208296"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.71002576208296"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.66711797028887"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.66711797028887"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.19047227490169"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.19047227490169"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.1600334040161"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.1600334040161"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.56294807440325"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.56294807440325"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.36205608020887"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.36205608020887"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.86768599451172"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.86768599451172"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.4907026273635"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.20191775088147"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.20191775088147"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.18599973267544"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.18599973267544"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.53983517486156"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.53983517486156"

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
    ## 1 healthyR.data <tibble [1,563 × 2]> <tibble [28 × 2]> <split [1535|28]>
    ## 2 healthyR      <tibble [1,556 × 2]> <tibble [28 × 2]> <split [1528|28]>
    ## 3 healthyR.ts   <tibble [1,500 × 2]> <tibble [28 × 2]> <split [1472|28]>
    ## 4 healthyverse  <tibble [1,471 × 2]> <tibble [28 × 2]> <split [1443|28]>
    ## 5 healthyR.ai   <tibble [1,295 × 2]> <tibble [28 × 2]> <split [1267|28]>
    ## 6 TidyDensity   <tibble [1,146 × 2]> <tibble [28 × 2]> <split [1118|28]>
    ## 7 tidyAML       <tibble [754 × 2]>   <tibble [28 × 2]> <split [726|28]> 
    ## 8 RandomWalker  <tibble [176 × 2]>   <tibble [28 × 2]> <split [148|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6958038 | 125.95796 | 0.6636464 | 154.81811 | 0.8267472 | 0.2421676 |
| healthyR.data | 2 | LM | Test | 0.8090097 | 199.03896 | 0.7716204 | 145.81333 | 0.9387505 | 0.1122283 |
| healthyR.data | 3 | EARTH | Test | 0.8093436 | 175.83673 | 0.7719389 | 130.90110 | 1.0293778 | 0.1122283 |
| healthyR.data | 4 | NNAR | Test | 0.7651265 | 113.07133 | 0.7297653 | 172.55170 | 0.9173312 | 0.0091139 |
| healthyR | 1 | ARIMA | Test | 0.7620442 | 175.64981 | 0.7001021 | 168.48134 | 0.9390860 | 0.0356319 |
| healthyR | 2 | LM | Test | 0.7702276 | 111.81690 | 0.7076203 | 182.65554 | 0.9719711 | 0.0558526 |
| healthyR | 3 | EARTH | Test | 0.7710727 | 120.76889 | 0.7083967 | 168.81482 | 0.9777093 | 0.0558526 |
| healthyR | 4 | NNAR | Test | 0.7358383 | 147.75517 | 0.6760263 | 154.41551 | 0.9338484 | 0.0650976 |
| healthyR.ts | 1 | ARIMA | Test | 1.0295668 | 116.89431 | 0.7145977 | 141.37151 | 1.2193236 | 0.2257529 |
| healthyR.ts | 2 | LM | Test | 1.0075426 | 122.22137 | 0.6993112 | 129.49313 | 1.2044354 | 0.2257529 |
| healthyR.ts | 3 | EARTH | Test | 0.9992209 | 124.25672 | 0.6935353 | 125.43728 | 1.2012440 | 0.2257529 |
| healthyR.ts | 4 | NNAR | Test | 0.9369116 | 99.81717 | 0.6502879 | 163.02141 | 1.1393385 | 0.1767125 |
| healthyverse | 1 | ARIMA | Test | 0.6820257 | 124.89371 | 0.9068840 | 112.35115 | 0.8568890 | 0.0942282 |
| healthyverse | 2 | LM | Test | 0.7795849 | 184.07171 | 1.0366078 | 110.75420 | 0.9469752 | 0.1948556 |
| healthyverse | 3 | EARTH | Test | 0.7022004 | 134.45781 | 0.9337103 | 111.13751 | 0.8908423 | 0.1948556 |
| healthyverse | 4 | NNAR | Test | 0.6924951 | 108.11287 | 0.9208051 | 121.31823 | 0.8749009 | 0.0523639 |
| healthyR.ai | 1 | ARIMA | Test | 0.7866221 | 94.80592 | 0.7481130 | 170.90035 | 0.9150747 | 0.1712177 |
| healthyR.ai | 2 | LM | Test | 0.8356361 | 94.78599 | 0.7947275 | 149.90157 | 1.0006367 | 0.1716863 |
| healthyR.ai | 3 | EARTH | Test | 0.8318945 | 95.74172 | 0.7911690 | 144.33321 | 1.0044071 | 0.1716863 |
| healthyR.ai | 4 | NNAR | Test | 0.7301360 | 85.59842 | 0.6943921 | 138.75341 | 0.8821243 | 0.1756287 |
| TidyDensity | 1 | ARIMA | Test | 0.6869406 | 141.17194 | 0.6936156 | 114.30699 | 0.8063040 | 0.0213855 |
| TidyDensity | 2 | LM | Test | 0.6926459 | 173.71929 | 0.6993764 | 105.61945 | 0.8110988 | 0.0970181 |
| TidyDensity | 3 | EARTH | Test | 0.6910332 | 128.40776 | 0.6977480 | 117.36054 | 0.8301540 | 0.0970181 |
| TidyDensity | 4 | NNAR | Test | 0.6801951 | 94.92433 | 0.6868046 | 135.92082 | 0.8621098 | 0.0892479 |
| tidyAML | 1 | ARIMA | Test | 0.6396431 | 128.88203 | 0.6338035 | 99.95209 | 0.7658077 | 0.0625880 |
| tidyAML | 2 | LM | Test | 0.6660614 | 143.68301 | 0.6599806 | 97.20246 | 0.7956125 | 0.0000427 |
| tidyAML | 3 | EARTH | Test | 0.7235329 | 88.68054 | 0.7169274 | 138.65609 | 0.8999047 | 0.0000427 |
| tidyAML | 4 | NNAR | Test | 0.6415378 | 129.20844 | 0.6356808 | 100.68276 | 0.7676838 | 0.0685805 |
| RandomWalker | 1 | ARIMA | Test | 1.0733340 | 105.81061 | 0.5062327 | 132.86333 | 1.1978775 | 0.3831489 |
| RandomWalker | 2 | LM | Test | 1.3388125 | 115.47902 | 0.6314443 | 192.06841 | 1.4660079 | 0.0105792 |
| RandomWalker | 3 | EARTH | Test | 1.2351358 | 89.19294 | 0.5825457 | 163.49201 | 1.4314954 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3528641 | 138.44124 | 0.6380717 | 144.70666 | 1.5932691 | 0.0031540 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.696 126.  0.664  155. 0.827 0.242 
    ## 2 healthyR              4 NNAR        Test  0.736 148.  0.676  154. 0.934 0.0651
    ## 3 healthyR.ts           4 NNAR        Test  0.937  99.8 0.650  163. 1.14  0.177 
    ## 4 healthyverse          1 ARIMA       Test  0.682 125.  0.907  112. 0.857 0.0942
    ## 5 healthyR.ai           4 NNAR        Test  0.730  85.6 0.694  139. 0.882 0.176 
    ## 6 TidyDensity           1 ARIMA       Test  0.687 141.  0.694  114. 0.806 0.0214
    ## 7 tidyAML               1 ARIMA       Test  0.640 129.  0.634  100. 0.766 0.0626
    ## 8 RandomWalker          1 ARIMA       Test  1.07  106.  0.506  133. 1.20  0.383

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1535|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1528|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1472|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1443|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1267|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1118|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [726|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [148|28]>  <mdl_tm_t [1 × 5]>

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
