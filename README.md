Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
04 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,836
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

The last day in the data set is 2025-04-02 21:11:36, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5721.6
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135836        |
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
| r_version     |     97671 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97671 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97671 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11412 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-02 | 2023-05-25 | 1592 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133952.87 | 1523618.85 | 355 | 14701 | 263400 | 2367762 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10354.87 | 18343.58 | 1 | 303 | 3075 | 11729 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-02 21:11:36 | 2023-05-25 10:56:06 | 82549 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   24.5 |       60 |

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
    ## -149.35  -35.56  -10.58   26.66  813.02 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.874e+02  7.377e+01
    ## date                                                1.138e-02  3.913e-03
    ## lag(value, 1)                                       1.068e-01  2.486e-02
    ## lag(value, 7)                                       9.743e-02  2.581e-02
    ## lag(value, 14)                                      9.547e-02  2.584e-02
    ## lag(value, 21)                                      6.439e-02  2.593e-02
    ## lag(value, 28)                                      5.850e-02  2.581e-02
    ## lag(value, 35)                                      6.921e-02  2.587e-02
    ## lag(value, 42)                                      5.147e-02  2.608e-02
    ## lag(value, 49)                                      7.628e-02  2.592e-02
    ## month(date, label = TRUE).L                        -1.038e+01  5.152e+00
    ## month(date, label = TRUE).Q                         2.659e+00  5.207e+00
    ## month(date, label = TRUE).C                        -1.220e+01  5.248e+00
    ## month(date, label = TRUE)^4                        -6.783e+00  5.200e+00
    ## month(date, label = TRUE)^5                        -1.236e+01  5.217e+00
    ## month(date, label = TRUE)^6                        -3.086e+00  5.294e+00
    ## month(date, label = TRUE)^7                        -6.198e+00  5.167e+00
    ## month(date, label = TRUE)^8                        -4.726e+00  5.194e+00
    ## month(date, label = TRUE)^9                         5.645e+00  5.251e+00
    ## month(date, label = TRUE)^10                        4.429e+00  5.307e+00
    ## month(date, label = TRUE)^11                       -5.881e+00  5.344e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.178e+01  2.395e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.236e+00  2.523e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.541 0.011166 *  
    ## date                                                 2.908 0.003689 ** 
    ## lag(value, 1)                                        4.294 1.86e-05 ***
    ## lag(value, 7)                                        3.775 0.000166 ***
    ## lag(value, 14)                                       3.695 0.000228 ***
    ## lag(value, 21)                                       2.484 0.013112 *  
    ## lag(value, 28)                                       2.266 0.023582 *  
    ## lag(value, 35)                                       2.675 0.007543 ** 
    ## lag(value, 42)                                       1.973 0.048624 *  
    ## lag(value, 49)                                       2.944 0.003294 ** 
    ## month(date, label = TRUE).L                         -2.014 0.044186 *  
    ## month(date, label = TRUE).Q                          0.511 0.609610    
    ## month(date, label = TRUE).C                         -2.326 0.020169 *  
    ## month(date, label = TRUE)^4                         -1.304 0.192296    
    ## month(date, label = TRUE)^5                         -2.368 0.017985 *  
    ## month(date, label = TRUE)^6                         -0.583 0.560085    
    ## month(date, label = TRUE)^7                         -1.199 0.230559    
    ## month(date, label = TRUE)^8                         -0.910 0.362952    
    ## month(date, label = TRUE)^9                          1.075 0.282520    
    ## month(date, label = TRUE)^10                         0.834 0.404154    
    ## month(date, label = TRUE)^11                        -1.101 0.271243    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.918 9.69e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.264 0.001123 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.71 on 1520 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2503, Adjusted R-squared:  0.2394 
    ## F-statistic: 23.06 on 22 and 1520 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.84933902675167"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.62707358899218"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 70, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.59504127416153"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 70, 49, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.56504280654543"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 70, 49, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.56504280654543"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 70, 49, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.77217268823604"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 70, 49, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.77217268823604"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 70, 49, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.60617746750355"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 70, 49, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.60617746750355"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.08291974829446"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.82051732145417"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.7474589830569"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98, 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.71829633151055"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 98, 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.71829633151055"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 98, 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.94160334708476"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 98, 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.94160334708476"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 98, 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.75553320074411"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 98, 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.75553320074411"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.91728770886599"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.91728770886599"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.26112734747528"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.26112734747528"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.93422471048871"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.93422471048871"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.93650038873876"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.62150977348591"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.62150977348591"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.42402438600279"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.42402438600279"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.88683829400427"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.88683829400427"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.81900469775017"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4906883083262"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.4906883083262"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.35766497985021"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.35766497985021"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.18909275653117"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.18909275653117"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.0887371349012"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.0887371349012"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.54583313391189"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.54583313391189"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.29905872278558"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.29905872278558"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.60389884407652"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.60389884407652"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.99028508790215"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.99028508790215"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.86669776383258"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.86669776383258"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.93803594618406"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.70580283751198"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.70580283751198"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.05157078520911"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.05157078520911"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.12391078659888"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.12391078659888"

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
    ## 1 healthyR.data <tibble [1,585 × 2]> <tibble [28 × 2]> <split [1557|28]>
    ## 2 healthyR      <tibble [1,578 × 2]> <tibble [28 × 2]> <split [1550|28]>
    ## 3 healthyR.ts   <tibble [1,522 × 2]> <tibble [28 × 2]> <split [1494|28]>
    ## 4 healthyverse  <tibble [1,493 × 2]> <tibble [28 × 2]> <split [1465|28]>
    ## 5 healthyR.ai   <tibble [1,317 × 2]> <tibble [28 × 2]> <split [1289|28]>
    ## 6 TidyDensity   <tibble [1,168 × 2]> <tibble [28 × 2]> <split [1140|28]>
    ## 7 tidyAML       <tibble [776 × 2]>   <tibble [28 × 2]> <split [748|28]> 
    ## 8 RandomWalker  <tibble [198 × 2]>   <tibble [28 × 2]> <split [170|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8002751 | 116.98644 | 0.7418976 | 167.38956 | 1.0351523 | 0.0105668 |
| healthyR.data | 2 | LM | Test | 0.6996819 | 151.42784 | 0.6486423 | 126.34437 | 0.8590720 | 0.0015686 |
| healthyR.data | 3 | EARTH | Test | 0.9516762 | 169.13265 | 0.8822544 | 156.86120 | 1.2113171 | 0.0015686 |
| healthyR.data | 4 | NNAR | Test | 0.7814608 | 103.98691 | 0.7244557 | 160.84048 | 1.0346265 | 0.0571063 |
| healthyR | 1 | ARIMA | Test | 0.7651914 | 110.97962 | 0.8094372 | 164.85121 | 0.9446007 | 0.0361436 |
| healthyR | 2 | LM | Test | 0.7368737 | 98.38758 | 0.7794821 | 187.58494 | 0.9162089 | 0.0257667 |
| healthyR | 3 | EARTH | Test | 0.8359132 | 127.65592 | 0.8842484 | 164.95377 | 1.0105316 | 0.0257667 |
| healthyR | 4 | NNAR | Test | 0.6978305 | 97.03141 | 0.7381814 | 149.15825 | 0.8880256 | 0.0552151 |
| healthyR.ts | 1 | ARIMA | Test | 1.1823173 | 379.06838 | 0.9408571 | 153.34046 | 1.3977109 | 0.0023315 |
| healthyR.ts | 2 | LM | Test | 1.0963770 | 307.34640 | 0.8724681 | 153.18693 | 1.3189551 | 0.0023315 |
| healthyR.ts | 3 | EARTH | Test | 1.0897827 | 301.24701 | 0.8672206 | 153.31642 | 1.3125048 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9057983 | 104.87794 | 0.7208106 | 163.56652 | 1.1528954 | 0.0541231 |
| healthyverse | 1 | ARIMA | Test | 0.6780970 | 180.11009 | 0.8498835 | 107.60314 | 0.8395215 | 0.0148543 |
| healthyverse | 2 | LM | Test | 0.6272014 | 250.91582 | 0.7860942 | 88.80424 | 0.7630739 | 0.0005843 |
| healthyverse | 3 | EARTH | Test | 0.6845324 | 155.33615 | 0.8579493 | 108.79451 | 0.8734472 | 0.0005843 |
| healthyverse | 4 | NNAR | Test | 0.6883803 | 141.10076 | 0.8627720 | 112.11020 | 0.8931225 | 0.0080809 |
| healthyR.ai | 1 | ARIMA | Test | 0.7104831 | 110.95549 | 0.7041469 | 156.19486 | 0.9103472 | 0.1363381 |
| healthyR.ai | 2 | LM | Test | 0.6856507 | 133.67728 | 0.6795360 | 139.28310 | 0.8976736 | 0.0121401 |
| healthyR.ai | 3 | EARTH | Test | 0.8058545 | 155.32582 | 0.7986677 | 184.71990 | 0.9899843 | 0.0121401 |
| healthyR.ai | 4 | NNAR | Test | 0.6466563 | 139.82025 | 0.6408893 | 130.88237 | 0.8584017 | 0.1473069 |
| TidyDensity | 1 | ARIMA | Test | 0.6733130 | 247.67817 | 0.6994457 | 119.03725 | 0.8273439 | 0.0301128 |
| TidyDensity | 2 | LM | Test | 0.7274284 | 289.84579 | 0.7556615 | 117.67352 | 0.8951435 | 0.0026000 |
| TidyDensity | 3 | EARTH | Test | 0.6761875 | 232.69139 | 0.7024318 | 120.80274 | 0.8263905 | 0.0026000 |
| TidyDensity | 4 | NNAR | Test | 0.6163409 | 137.46832 | 0.6402624 | 142.40753 | 0.7603884 | 0.0412900 |
| tidyAML | 1 | ARIMA | Test | 0.6684546 | 278.24429 | 0.7141867 | 102.54309 | 0.8412820 | 0.0369952 |
| tidyAML | 2 | LM | Test | 0.6580888 | 317.20884 | 0.7031117 | 97.47308 | 0.8091894 | 0.0111038 |
| tidyAML | 3 | EARTH | Test | 0.6931476 | 161.08225 | 0.7405690 | 127.48511 | 0.8877393 | 0.0111038 |
| tidyAML | 4 | NNAR | Test | 0.6344602 | 286.92900 | 0.6778665 | 95.73293 | 0.8065082 | 0.0051758 |
| RandomWalker | 1 | ARIMA | Test | 1.3991374 | 138.18747 | 0.8025634 | 121.28146 | 1.7727564 | 0.0275528 |
| RandomWalker | 2 | LM | Test | 1.2574854 | 110.36682 | 0.7213099 | 194.30642 | 1.3879274 | 0.0000418 |
| RandomWalker | 3 | EARTH | Test | 1.1692156 | 90.13430 | 0.6706773 | 165.83984 | 1.3372901 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3809771 | 128.90754 | 0.7921464 | 192.65996 | 1.5099272 | 0.1413299 |

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
    ## 1 healthyR.d…         2 LM          Test  0.700 151.  0.649 126.  0.859  1.57e-3
    ## 2 healthyR            4 NNAR        Test  0.698  97.0 0.738 149.  0.888  5.52e-2
    ## 3 healthyR.ts         4 NNAR        Test  0.906 105.  0.721 164.  1.15   5.41e-2
    ## 4 healthyver…         2 LM          Test  0.627 251.  0.786  88.8 0.763  5.84e-4
    ## 5 healthyR.ai         4 NNAR        Test  0.647 140.  0.641 131.  0.858  1.47e-1
    ## 6 TidyDensity         4 NNAR        Test  0.616 137.  0.640 142.  0.760  4.13e-2
    ## 7 tidyAML             4 NNAR        Test  0.634 287.  0.678  95.7 0.807  5.18e-3
    ## 8 RandomWalk…         3 EARTH       Test  1.17   90.1 0.671 166.  1.34  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1557|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1550|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1494|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1465|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1289|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1140|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [748|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [170|28]>  <mdl_tm_t [1 × 5]>

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
