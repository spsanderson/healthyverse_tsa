Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
07 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 139,042
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

The last day in the data set is 2025-05-05 23:54:20, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6516.31
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 139042        |
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
| r_version     |    100177 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100177 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100177 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11811 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-05 | 2023-06-15 | 1625 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133813.47 | 1519853.97 | 355 | 14701 | 280112 | 2367746.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10380.47 | 18454.94 | 1 | 292 | 3058 | 11650.75 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-05 23:54:20 | 2023-06-15 06:08:28 | 84821 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     14 |       60 |

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
    ## -146.67  -35.86  -11.02   26.65  814.67 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.805e+02  7.100e+01
    ## date                                                1.105e-02  3.765e-03
    ## lag(value, 1)                                       1.047e-01  2.460e-02
    ## lag(value, 7)                                       9.784e-02  2.548e-02
    ## lag(value, 14)                                      9.185e-02  2.553e-02
    ## lag(value, 21)                                      6.757e-02  2.562e-02
    ## lag(value, 28)                                      6.711e-02  2.559e-02
    ## lag(value, 35)                                      6.728e-02  2.564e-02
    ## lag(value, 42)                                      4.874e-02  2.582e-02
    ## lag(value, 49)                                      6.722e-02  2.563e-02
    ## month(date, label = TRUE).L                        -9.983e+00  5.128e+00
    ## month(date, label = TRUE).Q                         3.075e+00  5.183e+00
    ## month(date, label = TRUE).C                        -1.264e+01  5.173e+00
    ## month(date, label = TRUE)^4                        -6.699e+00  5.196e+00
    ## month(date, label = TRUE)^5                        -1.193e+01  5.182e+00
    ## month(date, label = TRUE)^6                        -3.241e+00  5.237e+00
    ## month(date, label = TRUE)^7                        -6.545e+00  5.156e+00
    ## month(date, label = TRUE)^8                        -4.122e+00  5.154e+00
    ## month(date, label = TRUE)^9                         5.373e+00  5.145e+00
    ## month(date, label = TRUE)^10                        4.234e+00  5.208e+00
    ## month(date, label = TRUE)^11                       -5.575e+00  5.317e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.156e+01  2.367e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.125e+00  2.489e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.543 0.011090 *  
    ## date                                                 2.936 0.003379 ** 
    ## lag(value, 1)                                        4.257 2.20e-05 ***
    ## lag(value, 7)                                        3.840 0.000128 ***
    ## lag(value, 14)                                       3.598 0.000331 ***
    ## lag(value, 21)                                       2.638 0.008433 ** 
    ## lag(value, 28)                                       2.623 0.008808 ** 
    ## lag(value, 35)                                       2.624 0.008773 ** 
    ## lag(value, 42)                                       1.888 0.059220 .  
    ## lag(value, 49)                                       2.623 0.008794 ** 
    ## month(date, label = TRUE).L                         -1.947 0.051730 .  
    ## month(date, label = TRUE).Q                          0.593 0.553142    
    ## month(date, label = TRUE).C                         -2.443 0.014676 *  
    ## month(date, label = TRUE)^4                         -1.289 0.197551    
    ## month(date, label = TRUE)^5                         -2.302 0.021441 *  
    ## month(date, label = TRUE)^6                         -0.619 0.536080    
    ## month(date, label = TRUE)^7                         -1.269 0.204473    
    ## month(date, label = TRUE)^8                         -0.800 0.423994    
    ## month(date, label = TRUE)^9                          1.044 0.296517    
    ## month(date, label = TRUE)^10                         0.813 0.416395    
    ## month(date, label = TRUE)^11                        -1.049 0.294561    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.881 1.16e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.264 0.001124 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.81 on 1553 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2429, Adjusted R-squared:  0.2321 
    ## F-statistic: 22.64 on 22 and 1553 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.31968519398258"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.18326867357131"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.9514450833027"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.9514450833027"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.36271713234049"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.36271713234049"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.9643826647023"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.9643826647023"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.1619805691834"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.1619805691834"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.05500486420795"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.05500486420795"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.51448608339207"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.51448608339207"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.14408478945861"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.35919688674323"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.35919688674323"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.88578867731008"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.88578867731008"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.36300115592244"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.36300115592244"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.30129945996527"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.55560010662585"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.29871614970738"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.29871614970738"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.53654096346111"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.53654096346111"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.23259191239231"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.23259191239231"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.59001054994572"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.59001054994572"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.412799864668"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.412799864668"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.1566304126903"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.1566304126903"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.76080406772811"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.76080406772811"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.27269866601532"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.27269866601532"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.30818077193231"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.30818077193231"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.84551693894502"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.84551693894502"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65826309095942"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65826309095942"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.87362791562433"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.87362791562433"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.00880850341093"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.39578721925788"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.39578721925788"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.09819804992716"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.09819804992716"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.57305880331548"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.57305880331548"

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
    ## 1 healthyR.data <tibble [1,618 × 2]> <tibble [28 × 2]> <split [1590|28]>
    ## 2 healthyR      <tibble [1,611 × 2]> <tibble [28 × 2]> <split [1583|28]>
    ## 3 healthyR.ts   <tibble [1,555 × 2]> <tibble [28 × 2]> <split [1527|28]>
    ## 4 healthyverse  <tibble [1,525 × 2]> <tibble [28 × 2]> <split [1497|28]>
    ## 5 healthyR.ai   <tibble [1,350 × 2]> <tibble [28 × 2]> <split [1322|28]>
    ## 6 TidyDensity   <tibble [1,201 × 2]> <tibble [28 × 2]> <split [1173|28]>
    ## 7 tidyAML       <tibble [809 × 2]>   <tibble [28 × 2]> <split [781|28]> 
    ## 8 RandomWalker  <tibble [231 × 2]>   <tibble [28 × 2]> <split [203|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6882929 | 125.37551 | 0.6309922 | 139.75813 | 0.8482545 | 0.0000272 |
| healthyR.data | 2 | LM | Test | 0.6857827 | 166.37022 | 0.6286910 | 124.05174 | 0.8287399 | 0.0551131 |
| healthyR.data | 3 | EARTH | Test | 0.6973852 | 218.44429 | 0.6393275 | 115.03018 | 0.8496239 | 0.0551131 |
| healthyR.data | 4 | NNAR | Test | 0.7203510 | 96.39229 | 0.6603814 | 171.92331 | 0.8866663 | 0.0242040 |
| healthyR | 1 | ARIMA | Test | 0.6420254 | 100.29100 | 0.7306023 | 180.91813 | 0.7969158 | 0.0116339 |
| healthyR | 2 | LM | Test | 0.6428029 | 95.25889 | 0.7314870 | 173.94947 | 0.7926039 | 0.1246753 |
| healthyR | 3 | EARTH | Test | 0.6200071 | 95.05215 | 0.7055462 | 142.77431 | 0.7653826 | 0.1246753 |
| healthyR | 4 | NNAR | Test | 0.6315434 | 90.74297 | 0.7186742 | 149.86676 | 0.7805513 | 0.0033439 |
| healthyR.ts | 1 | ARIMA | Test | 0.8056511 | 105.39694 | 0.7004689 | 171.91048 | 0.9851384 | 0.0870225 |
| healthyR.ts | 2 | LM | Test | 0.9205441 | 223.46255 | 0.8003619 | 144.37023 | 1.1457012 | 0.0870225 |
| healthyR.ts | 3 | EARTH | Test | 0.8377133 | 312.13581 | 0.7283452 | 123.41950 | 1.0197186 | 0.0870225 |
| healthyR.ts | 4 | NNAR | Test | 0.8144752 | 132.61085 | 0.7081409 | 179.70741 | 0.9843241 | 0.0007617 |
| healthyverse | 1 | ARIMA | Test | 0.6058718 | 336.84376 | 0.8471461 | 97.75827 | 0.7476257 | 0.0100118 |
| healthyverse | 2 | LM | Test | 0.6201922 | 372.28280 | 0.8671693 | 97.43831 | 0.7569627 | 0.0053504 |
| healthyverse | 3 | EARTH | Test | 0.5907656 | 235.84211 | 0.8260243 | 104.01924 | 0.7622860 | 0.0053504 |
| healthyverse | 4 | NNAR | Test | 0.5766711 | 201.54397 | 0.8063169 | 105.97470 | 0.7445952 | 0.0928220 |
| healthyR.ai | 1 | ARIMA | Test | 0.7723152 | 123.29139 | 0.8106727 | 167.73504 | 1.0214993 | 0.0392716 |
| healthyR.ai | 2 | LM | Test | 0.7744811 | 126.37818 | 0.8129462 | 160.47976 | 1.0391789 | 0.0188166 |
| healthyR.ai | 3 | EARTH | Test | 0.7950000 | 145.49603 | 0.8344842 | 149.77193 | 1.0725238 | 0.0188166 |
| healthyR.ai | 4 | NNAR | Test | 0.7707932 | 135.44672 | 0.8090752 | 157.16061 | 1.0376807 | 0.0273807 |
| TidyDensity | 1 | ARIMA | Test | 0.5538695 | 267.46596 | 0.8162263 | 116.23499 | 0.6772682 | 0.0095032 |
| TidyDensity | 2 | LM | Test | 0.6045367 | 392.98131 | 0.8908935 | 107.47933 | 0.7523435 | 0.0836312 |
| TidyDensity | 3 | EARTH | Test | 0.5511720 | 254.69020 | 0.8122510 | 114.43976 | 0.6805287 | 0.0836312 |
| TidyDensity | 4 | NNAR | Test | 0.5423209 | 140.86754 | 0.7992073 | 135.70334 | 0.6893196 | 0.0633944 |
| tidyAML | 1 | ARIMA | Test | 0.6627182 | 165.88422 | 0.9731616 | 102.15187 | 0.7936627 | 0.0020696 |
| tidyAML | 2 | LM | Test | 0.6614061 | 181.54082 | 0.9712349 | 96.51978 | 0.7981091 | 0.1120359 |
| tidyAML | 3 | EARTH | Test | 0.6678325 | 127.99251 | 0.9806716 | 109.40616 | 0.8192452 | 0.1120359 |
| tidyAML | 4 | NNAR | Test | 0.6554262 | 167.52412 | 0.9624537 | 99.38267 | 0.7881851 | 0.0084778 |
| RandomWalker | 1 | ARIMA | Test | 1.4793064 | 115.14123 | 0.6655381 | 168.87967 | 1.8037133 | 0.3864663 |
| RandomWalker | 2 | LM | Test | 1.2778139 | 111.10143 | 0.5748869 | 172.09436 | 1.4994183 | 0.0118719 |
| RandomWalker | 3 | EARTH | Test | 1.2769688 | 111.55021 | 0.5745067 | 171.01107 | 1.5001539 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3921598 | 166.99499 | 0.6263310 | 166.03515 | 1.5639406 | 0.0018257 |

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
    ## 1 healthyR.da…         2 LM          Test  0.686 166.  0.629 124.  0.829 5.51e-2
    ## 2 healthyR             3 EARTH       Test  0.620  95.1 0.706 143.  0.765 1.25e-1
    ## 3 healthyR.ts          4 NNAR        Test  0.814 133.  0.708 180.  0.984 7.62e-4
    ## 4 healthyverse         4 NNAR        Test  0.577 202.  0.806 106.  0.745 9.28e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.772 123.  0.811 168.  1.02  3.93e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.554 267.  0.816 116.  0.677 9.50e-3
    ## 7 tidyAML              4 NNAR        Test  0.655 168.  0.962  99.4 0.788 8.48e-3
    ## 8 RandomWalker         2 LM          Test  1.28  111.  0.575 172.  1.50  1.19e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1590|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1583|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1527|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1497|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1322|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1173|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [781|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [203|28]>  <mdl_tm_t [1 × 5]>

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
