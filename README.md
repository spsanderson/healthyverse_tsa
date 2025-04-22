Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,708
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

The last day in the data set is 2025-04-20 23:54:11, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6156.31
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137708        |
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
| r_version     |     99124 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99124 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99124 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11604 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-20 | 2023-06-06 | 1610 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135467.91 | 1522230.77 | 355 | 14701 | 275170 | 2367773 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10367.84 | 18401.97 | 1 | 298 | 3064 | 11663 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-20 23:54:11 | 2023-06-06 09:57:55 | 83903 |

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
    ## -148.02  -35.60  -10.50   26.57  813.70 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.891e+02  7.231e+01
    ## date                                                1.148e-02  3.836e-03
    ## lag(value, 1)                                       1.055e-01  2.472e-02
    ## lag(value, 7)                                       9.656e-02  2.560e-02
    ## lag(value, 14)                                      9.599e-02  2.570e-02
    ## lag(value, 21)                                      6.910e-02  2.576e-02
    ## lag(value, 28)                                      6.439e-02  2.574e-02
    ## lag(value, 35)                                      6.713e-02  2.578e-02
    ## lag(value, 42)                                      4.936e-02  2.588e-02
    ## lag(value, 49)                                      6.888e-02  2.572e-02
    ## month(date, label = TRUE).L                        -1.033e+01  5.137e+00
    ## month(date, label = TRUE).Q                         2.655e+00  5.198e+00
    ## month(date, label = TRUE).C                        -1.219e+01  5.204e+00
    ## month(date, label = TRUE)^4                        -6.602e+00  5.199e+00
    ## month(date, label = TRUE)^5                        -1.240e+01  5.202e+00
    ## month(date, label = TRUE)^6                        -2.890e+00  5.258e+00
    ## month(date, label = TRUE)^7                        -6.286e+00  5.169e+00
    ## month(date, label = TRUE)^8                        -4.573e+00  5.172e+00
    ## month(date, label = TRUE)^9                         5.599e+00  5.180e+00
    ## month(date, label = TRUE)^10                        4.350e+00  5.260e+00
    ## month(date, label = TRUE)^11                       -5.787e+00  5.338e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.169e+01  2.382e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.144e+00  2.508e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.615 0.009001 ** 
    ## date                                                 2.993 0.002809 ** 
    ## lag(value, 1)                                        4.269 2.08e-05 ***
    ## lag(value, 7)                                        3.772 0.000168 ***
    ## lag(value, 14)                                       3.734 0.000195 ***
    ## lag(value, 21)                                       2.682 0.007398 ** 
    ## lag(value, 28)                                       2.502 0.012470 *  
    ## lag(value, 35)                                       2.604 0.009296 ** 
    ## lag(value, 42)                                       1.907 0.056703 .  
    ## lag(value, 49)                                       2.678 0.007492 ** 
    ## month(date, label = TRUE).L                         -2.011 0.044549 *  
    ## month(date, label = TRUE).Q                          0.511 0.609658    
    ## month(date, label = TRUE).C                         -2.342 0.019313 *  
    ## month(date, label = TRUE)^4                         -1.270 0.204320    
    ## month(date, label = TRUE)^5                         -2.384 0.017247 *  
    ## month(date, label = TRUE)^6                         -0.550 0.582556    
    ## month(date, label = TRUE)^7                         -1.216 0.224172    
    ## month(date, label = TRUE)^8                         -0.884 0.376722    
    ## month(date, label = TRUE)^9                          1.081 0.279922    
    ## month(date, label = TRUE)^10                         0.827 0.408338    
    ## month(date, label = TRUE)^11                        -1.084 0.278500    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.910 1.01e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.248 0.001189 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.79 on 1538 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2474, Adjusted R-squared:  0.2366 
    ## F-statistic: 22.98 on 22 and 1538 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.4953706482459"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.12254717450526"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.11973521876182"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.11973521876182"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.75862152255792"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.75862152255792"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.81263066011668"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.81263066011668"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.11899837616993"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.75745276210471"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.71113920405602"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.62473392873764"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 91, 70, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.60996543048662"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 91, 70, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.60996543048662"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 91, 70, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.33122494069242"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 91, 70, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.33122494069242"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 91, 70, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.89134247511721"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 91, 70, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.89134247511721"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.02132906198513"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55684428784322"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.55684428784322"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.35885344273781"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.35885344273781"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.73567269446901"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.73567269446901"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.50329233689893"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.01520482770726"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.01520482770726"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.16377842784451"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.16377842784451"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.39890255707212"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.39890255707212"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.72121361864848"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.65537001360946"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.65537001360946"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.98683094423122"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.98683094423122"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.04915868668539"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.04915868668539"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.43725265776426"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.43725265776426"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.25379621249964"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.25379621249964"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.59400813713548"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.59400813713548"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.83361806385869"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.83361806385869"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.44701375388449"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.44701375388449"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.18172114593128"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.18172114593128"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.40784309959943"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.16048498140134"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.10179126193546"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77, 35, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.07305036206526"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77, 35, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.07305036206526"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77, 35, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.43224156792624"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77, 35, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.43224156792624"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77, 35, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.866753963521"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77, 35, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.866753963521"

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
    ## 1 healthyR.data <tibble [1,603 × 2]> <tibble [28 × 2]> <split [1575|28]>
    ## 2 healthyR      <tibble [1,596 × 2]> <tibble [28 × 2]> <split [1568|28]>
    ## 3 healthyR.ts   <tibble [1,540 × 2]> <tibble [28 × 2]> <split [1512|28]>
    ## 4 healthyverse  <tibble [1,511 × 2]> <tibble [28 × 2]> <split [1483|28]>
    ## 5 healthyR.ai   <tibble [1,335 × 2]> <tibble [28 × 2]> <split [1307|28]>
    ## 6 TidyDensity   <tibble [1,186 × 2]> <tibble [28 × 2]> <split [1158|28]>
    ## 7 tidyAML       <tibble [794 × 2]>   <tibble [28 × 2]> <split [766|28]> 
    ## 8 RandomWalker  <tibble [216 × 2]>   <tibble [28 × 2]> <split [188|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8872888 | 118.05050 | 0.6263153 | 166.77689 | 1.0231918 | 0.1215583 |
| healthyR.data | 2 | LM | Test | 0.8496714 | 130.71444 | 0.5997620 | 139.56661 | 0.9759175 | 0.0334515 |
| healthyR.data | 3 | EARTH | Test | 0.9314382 | 191.92262 | 0.6574793 | 121.83291 | 1.1216596 | 0.0334515 |
| healthyR.data | 4 | NNAR | Test | 0.8647894 | 100.35950 | 0.6104335 | 184.42361 | 1.0316796 | 0.0032995 |
| healthyR | 1 | ARIMA | Test | 0.7964504 | 99.62754 | 0.7516429 | 183.63565 | 0.9566926 | 0.0070013 |
| healthyR | 2 | LM | Test | 0.7929123 | 99.43847 | 0.7483039 | 188.57401 | 0.9392469 | 0.0200525 |
| healthyR | 3 | EARTH | Test | 0.7925528 | 99.28781 | 0.7479646 | 175.80088 | 0.9341276 | 0.0200525 |
| healthyR | 4 | NNAR | Test | 0.7989463 | 99.62303 | 0.7539984 | 168.97443 | 0.9456069 | 0.1246575 |
| healthyR.ts | 1 | ARIMA | Test | 1.0058314 | 133.45792 | 0.7029039 | 146.85801 | 1.2235332 | 0.0145521 |
| healthyR.ts | 2 | LM | Test | 1.0158970 | 152.54550 | 0.7099380 | 132.25147 | 1.2817715 | 0.0145521 |
| healthyR.ts | 3 | EARTH | Test | 1.0156578 | 151.40135 | 0.7097709 | 133.01857 | 1.2782770 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9864764 | 97.20658 | 0.6893781 | 188.09100 | 1.1568012 | 0.0441460 |
| healthyverse | 1 | ARIMA | Test | 0.7534502 | 238.52258 | 0.7217805 | 117.19561 | 0.9140623 | 0.0058848 |
| healthyverse | 2 | LM | Test | 0.7434157 | 313.14391 | 0.7121678 | 107.12589 | 0.8881596 | 0.0303497 |
| healthyverse | 3 | EARTH | Test | 0.7479283 | 199.03499 | 0.7164907 | 121.36791 | 0.9311894 | 0.0303497 |
| healthyverse | 4 | NNAR | Test | 0.7498552 | 183.94760 | 0.7183367 | 123.84863 | 0.9438307 | 0.0002185 |
| healthyR.ai | 1 | ARIMA | Test | 0.7893842 | 112.79886 | 0.7261473 | 181.38659 | 0.9807191 | 0.0178508 |
| healthyR.ai | 2 | LM | Test | 0.7571580 | 134.72848 | 0.6965028 | 159.25913 | 0.9221513 | 0.0464662 |
| healthyR.ai | 3 | EARTH | Test | 0.7636754 | 125.13629 | 0.7024980 | 165.16684 | 0.9311908 | 0.0464662 |
| healthyR.ai | 4 | NNAR | Test | 0.7903398 | 152.24310 | 0.7270263 | 159.62828 | 0.9637148 | 0.0462436 |
| TidyDensity | 1 | ARIMA | Test | 0.6291294 | 205.92505 | 0.7536932 | 118.28447 | 0.7464154 | 0.0246044 |
| TidyDensity | 2 | LM | Test | 0.6313422 | 280.30820 | 0.7563443 | 101.48233 | 0.7976330 | 0.0085988 |
| TidyDensity | 3 | EARTH | Test | 0.6223822 | 203.19216 | 0.7456102 | 113.30199 | 0.7563209 | 0.0085988 |
| TidyDensity | 4 | NNAR | Test | 0.6485264 | 129.72132 | 0.7769308 | 142.82890 | 0.7811954 | 0.0314575 |
| tidyAML | 1 | ARIMA | Test | 0.6129990 | 259.44460 | 0.8373420 | 102.24977 | 0.7313926 | 0.0941761 |
| tidyAML | 2 | LM | Test | 0.6285849 | 279.55992 | 0.8586318 | 99.40473 | 0.7793258 | 0.0177538 |
| tidyAML | 3 | EARTH | Test | 0.5742118 | 175.09659 | 0.7843596 | 108.64705 | 0.7351257 | 0.0177538 |
| tidyAML | 4 | NNAR | Test | 0.6224639 | 231.17944 | 0.8502707 | 106.79412 | 0.7576899 | 0.0132992 |
| RandomWalker | 1 | ARIMA | Test | 1.2987514 | 116.25739 | 0.6094100 | 160.37923 | 1.5285901 | 0.0000582 |
| RandomWalker | 2 | LM | Test | 1.3067692 | 108.45338 | 0.6131722 | 183.27233 | 1.5129326 | 0.0242224 |
| RandomWalker | 3 | EARTH | Test | 1.3012630 | 114.13354 | 0.6105885 | 174.99390 | 1.5152642 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3544512 | 116.27696 | 0.6355459 | 150.97186 | 1.6373428 | 0.0014309 |

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
    ## 1 healthyR.data         2 LM          Test  0.850 131.  0.600  140. 0.976 0.0335
    ## 2 healthyR              3 EARTH       Test  0.793  99.3 0.748  176. 0.934 0.0201
    ## 3 healthyR.ts           4 NNAR        Test  0.986  97.2 0.689  188. 1.16  0.0441
    ## 4 healthyverse          2 LM          Test  0.743 313.  0.712  107. 0.888 0.0303
    ## 5 healthyR.ai           2 LM          Test  0.757 135.  0.697  159. 0.922 0.0465
    ## 6 TidyDensity           1 ARIMA       Test  0.629 206.  0.754  118. 0.746 0.0246
    ## 7 tidyAML               1 ARIMA       Test  0.613 259.  0.837  102. 0.731 0.0942
    ## 8 RandomWalker          2 LM          Test  1.31  108.  0.613  183. 1.51  0.0242

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1575|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1568|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1512|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1483|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1307|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1158|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [766|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [188|28]>  <mdl_tm_t [1 × 5]>

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
