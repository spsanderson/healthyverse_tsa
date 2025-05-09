Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 139,405
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

The last day in the data set is 2025-05-07 23:39:22, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6564.06
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 139405        |
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
| r_version     |    100472 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100472 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100472 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11841 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-07 | 2023-06-17 | 1627 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134829.71 | 1520310.0 | 355 | 14701 | 289680 | 2367756 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10388.69 | 18485.4 | 1 | 295 | 3051 | 11641 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-07 23:39:22 | 2023-06-17 10:55:45 | 85119 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 24S |       60 |

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
    ## -146.37  -35.76  -11.21   26.54  814.88 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.918e+02  7.083e+01
    ## date                                                1.167e-02  3.755e-03
    ## lag(value, 1)                                       1.041e-01  2.463e-02
    ## lag(value, 7)                                       9.551e-02  2.549e-02
    ## lag(value, 14)                                      9.072e-02  2.555e-02
    ## lag(value, 21)                                      6.787e-02  2.564e-02
    ## lag(value, 28)                                      6.791e-02  2.561e-02
    ## lag(value, 35)                                      6.536e-02  2.567e-02
    ## lag(value, 42)                                      4.672e-02  2.584e-02
    ## lag(value, 49)                                      6.701e-02  2.567e-02
    ## month(date, label = TRUE).L                        -1.012e+01  5.135e+00
    ## month(date, label = TRUE).Q                         2.740e+00  5.187e+00
    ## month(date, label = TRUE).C                        -1.228e+01  5.177e+00
    ## month(date, label = TRUE)^4                        -6.550e+00  5.204e+00
    ## month(date, label = TRUE)^5                        -1.233e+01  5.185e+00
    ## month(date, label = TRUE)^6                        -3.223e+00  5.245e+00
    ## month(date, label = TRUE)^7                        -6.206e+00  5.160e+00
    ## month(date, label = TRUE)^8                        -4.431e+00  5.159e+00
    ## month(date, label = TRUE)^9                         5.162e+00  5.152e+00
    ## month(date, label = TRUE)^10                        4.793e+00  5.206e+00
    ## month(date, label = TRUE)^11                       -5.999e+00  5.318e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.184e+01  2.366e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.422e+00  2.490e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.708 0.006844 ** 
    ## date                                                 3.107 0.001923 ** 
    ## lag(value, 1)                                        4.226 2.51e-05 ***
    ## lag(value, 7)                                        3.746 0.000186 ***
    ## lag(value, 14)                                       3.551 0.000396 ***
    ## lag(value, 21)                                       2.647 0.008211 ** 
    ## lag(value, 28)                                       2.652 0.008094 ** 
    ## lag(value, 35)                                       2.547 0.010972 *  
    ## lag(value, 42)                                       1.808 0.070749 .  
    ## lag(value, 49)                                       2.611 0.009118 ** 
    ## month(date, label = TRUE).L                         -1.971 0.048933 *  
    ## month(date, label = TRUE).Q                          0.528 0.597361    
    ## month(date, label = TRUE).C                         -2.372 0.017797 *  
    ## month(date, label = TRUE)^4                         -1.259 0.208336    
    ## month(date, label = TRUE)^5                         -2.378 0.017525 *  
    ## month(date, label = TRUE)^6                         -0.614 0.539006    
    ## month(date, label = TRUE)^7                         -1.203 0.229229    
    ## month(date, label = TRUE)^8                         -0.859 0.390572    
    ## month(date, label = TRUE)^9                          1.002 0.316539    
    ## month(date, label = TRUE)^10                         0.921 0.357295    
    ## month(date, label = TRUE)^11                        -1.128 0.259534    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.003 6.29e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.382 0.000737 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.9 on 1555 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2429, Adjusted R-squared:  0.2322 
    ## F-statistic: 22.68 on 22 and 1555 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.68289330984692"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.75637490277449"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.75637490277449"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.92384869314633"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.92384869314633"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.19646187472118"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.19646187472118"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.23012030055204"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.12364297966083"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.12364297966083"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.28350964725846"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.28350964725846"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.12468099795393"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.12468099795393"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.85651942427558"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.74991266073846"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.74491280431788"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.74491280431788"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.43833110059483"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.43833110059483"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.25178502477681"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.25178502477681"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.34899338935038"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.76132512400939"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.76132512400939"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.19339415196967"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.19339415196967"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.3457558488422"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.3457558488422"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.68667794571941"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.68667794571941"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.55762369246147"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.55762369246147"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.63706994712131"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.63706994712131"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.82230750321162"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.81257970731934"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.81257970731934"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.93583532716426"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.93583532716426"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.67227911748465"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.67227911748465"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8624709196113"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.8624709196113"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.51144561670414"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.51144561670414"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.86473904492883"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.86473904492883"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.13163124738511"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.72119195865454"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.72119195865454"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.85781234697775"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.85781234697775"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.08260868906089"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.08260868906089"

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
    ## 1 healthyR.data <tibble [1,620 × 2]> <tibble [28 × 2]> <split [1592|28]>
    ## 2 healthyR      <tibble [1,613 × 2]> <tibble [28 × 2]> <split [1585|28]>
    ## 3 healthyR.ts   <tibble [1,557 × 2]> <tibble [28 × 2]> <split [1529|28]>
    ## 4 healthyverse  <tibble [1,527 × 2]> <tibble [28 × 2]> <split [1499|28]>
    ## 5 healthyR.ai   <tibble [1,352 × 2]> <tibble [28 × 2]> <split [1324|28]>
    ## 6 TidyDensity   <tibble [1,203 × 2]> <tibble [28 × 2]> <split [1175|28]>
    ## 7 tidyAML       <tibble [811 × 2]>   <tibble [28 × 2]> <split [783|28]> 
    ## 8 RandomWalker  <tibble [233 × 2]>   <tibble [28 × 2]> <split [205|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7718041 | 121.30209 | 0.7314959 | 139.93075 | 0.9696681 | 0.0000298 |
| healthyR.data | 2 | LM | Test | 0.7762892 | 167.45596 | 0.7357466 | 127.49497 | 0.9707282 | 0.0007696 |
| healthyR.data | 3 | EARTH | Test | 0.9312600 | 301.03253 | 0.8826240 | 121.11619 | 1.1538521 | 0.0007696 |
| healthyR.data | 4 | NNAR | Test | 0.7682321 | 95.40567 | 0.7281104 | 181.49887 | 0.9461369 | 0.0753033 |
| healthyR | 1 | ARIMA | Test | 0.6794369 | 104.40595 | 0.7908109 | 183.00798 | 0.8358351 | 0.0237736 |
| healthyR | 2 | LM | Test | 0.6777356 | 95.01608 | 0.7888307 | 173.58091 | 0.8334556 | 0.0278305 |
| healthyR | 3 | EARTH | Test | 0.6548254 | 93.91229 | 0.7621651 | 141.71263 | 0.8136713 | 0.0278305 |
| healthyR | 4 | NNAR | Test | 0.6675195 | 90.96417 | 0.7769399 | 148.93180 | 0.8239607 | 0.0095162 |
| healthyR.ts | 1 | ARIMA | Test | 0.8451992 | 110.37981 | 0.7431289 | 172.24584 | 1.0429245 | 0.0084981 |
| healthyR.ts | 2 | LM | Test | 0.9578806 | 241.07882 | 0.8422023 | 147.64979 | 1.1755849 | 0.0084981 |
| healthyR.ts | 3 | EARTH | Test | 0.8881130 | 287.15252 | 0.7808602 | 134.01083 | 1.0874488 | 0.0084981 |
| healthyR.ts | 4 | NNAR | Test | 0.8470834 | 135.95734 | 0.7447855 | 182.46875 | 1.0387471 | 0.0000084 |
| healthyverse | 1 | ARIMA | Test | 0.6212015 | 363.66387 | 0.8486599 | 102.80380 | 0.7506635 | 0.0129313 |
| healthyverse | 2 | LM | Test | 0.6367631 | 402.31609 | 0.8699196 | 102.20325 | 0.7654020 | 0.0053646 |
| healthyverse | 3 | EARTH | Test | 0.8031307 | 604.20563 | 1.0972041 | 108.14110 | 0.9035512 | 0.0053646 |
| healthyverse | 4 | NNAR | Test | 0.5904363 | 230.60490 | 0.8066298 | 110.16362 | 0.7433499 | 0.0710925 |
| healthyR.ai | 1 | ARIMA | Test | 0.8112502 | 106.93530 | 0.8532300 | 168.69640 | 1.0579119 | 0.0506351 |
| healthyR.ai | 2 | LM | Test | 0.8280552 | 124.00485 | 0.8709046 | 161.30783 | 1.0846196 | 0.0184430 |
| healthyR.ai | 3 | EARTH | Test | 0.8480234 | 141.46718 | 0.8919061 | 150.32443 | 1.1207124 | 0.0184430 |
| healthyR.ai | 4 | NNAR | Test | 0.8206057 | 132.46706 | 0.8630696 | 153.35174 | 1.0888721 | 0.0201858 |
| TidyDensity | 1 | ARIMA | Test | 0.5540018 | 240.23372 | 0.8164274 | 115.40252 | 0.6813310 | 0.0277460 |
| TidyDensity | 2 | LM | Test | 0.6152711 | 359.73391 | 0.9067195 | 109.09238 | 0.7566971 | 0.0745603 |
| TidyDensity | 3 | EARTH | Test | 0.5663431 | 233.70784 | 0.8346147 | 117.41574 | 0.6883732 | 0.0745603 |
| TidyDensity | 4 | NNAR | Test | 0.5530570 | 129.92413 | 0.8150351 | 135.12424 | 0.6986450 | 0.0800097 |
| tidyAML | 1 | ARIMA | Test | 0.6695941 | 207.03732 | 0.8994561 | 99.02136 | 0.8221620 | 0.0031798 |
| tidyAML | 2 | LM | Test | 0.6772963 | 192.09816 | 0.9098024 | 100.83402 | 0.8177264 | 0.1699453 |
| tidyAML | 3 | EARTH | Test | 0.6644271 | 142.75317 | 0.8925153 | 110.32543 | 0.8092843 | 0.1699453 |
| tidyAML | 4 | NNAR | Test | 0.6722577 | 185.65836 | 0.9030341 | 100.77316 | 0.8229866 | 0.0065733 |
| RandomWalker | 1 | ARIMA | Test | 1.2939805 | 168.04481 | 0.6163368 | 155.85566 | 1.5566482 | 0.0028345 |
| RandomWalker | 2 | LM | Test | 1.2526378 | 112.71503 | 0.5966448 | 172.11418 | 1.4912882 | 0.0003070 |
| RandomWalker | 3 | EARTH | Test | 1.2523751 | 113.03485 | 0.5965197 | 171.45411 | 1.4920869 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4079301 | 262.67445 | 0.6706122 | 158.32815 | 1.6218195 | 0.0106676 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.768  95.4 0.728  181. 0.946 7.53e-2
    ## 2 healthyR             3 EARTH       Test  0.655  93.9 0.762  142. 0.814 2.78e-2
    ## 3 healthyR.ts          4 NNAR        Test  0.847 136.  0.745  182. 1.04  8.44e-6
    ## 4 healthyverse         4 NNAR        Test  0.590 231.  0.807  110. 0.743 7.11e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.811 107.  0.853  169. 1.06  5.06e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.554 240.  0.816  115. 0.681 2.77e-2
    ## 7 tidyAML              3 EARTH       Test  0.664 143.  0.893  110. 0.809 1.70e-1
    ## 8 RandomWalker         2 LM          Test  1.25  113.  0.597  172. 1.49  3.07e-4

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1592|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1585|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1529|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1499|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1324|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1175|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [783|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [205|28]>  <mdl_tm_t [1 × 5]>

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
