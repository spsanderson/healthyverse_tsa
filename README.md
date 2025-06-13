Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 142,214
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

The last day in the data set is 2025-06-11 23:40:05, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7404.07 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 142214        |
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
| r_version     |    102620 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102620 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102620 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12072 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-11 | 2023-07-03 | 1662 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134110.05 | 1516881.8 | 355 | 14701 | 289852 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10432.65 | 18585.2 | 1 | 280 | 3030 | 11655 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-11 23:40:05 | 2023-07-03 08:23:47 | 87176 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     32 |       60 |

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
    ## -147.08  -35.66  -11.13   26.93  815.02 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.582e+02  6.766e+01
    ## date                                                9.876e-03  3.584e-03
    ## lag(value, 1)                                       1.033e-01  2.430e-02
    ## lag(value, 7)                                       9.667e-02  2.519e-02
    ## lag(value, 14)                                      8.970e-02  2.519e-02
    ## lag(value, 21)                                      6.754e-02  2.527e-02
    ## lag(value, 28)                                      6.929e-02  2.514e-02
    ## lag(value, 35)                                      6.825e-02  2.520e-02
    ## lag(value, 42)                                      4.982e-02  2.534e-02
    ## lag(value, 49)                                      7.003e-02  2.523e-02
    ## month(date, label = TRUE).L                        -9.597e+00  5.106e+00
    ## month(date, label = TRUE).Q                         4.051e+00  5.105e+00
    ## month(date, label = TRUE).C                        -1.347e+01  5.128e+00
    ## month(date, label = TRUE)^4                        -7.345e+00  5.156e+00
    ## month(date, label = TRUE)^5                        -1.098e+01  5.112e+00
    ## month(date, label = TRUE)^6                        -3.342e+00  5.201e+00
    ## month(date, label = TRUE)^7                        -7.593e+00  5.087e+00
    ## month(date, label = TRUE)^8                        -3.523e+00  5.087e+00
    ## month(date, label = TRUE)^9                         5.995e+00  5.094e+00
    ## month(date, label = TRUE)^10                        3.030e+00  5.073e+00
    ## month(date, label = TRUE)^11                       -4.806e+00  5.166e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.152e+01  2.324e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.016e+00  2.446e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.338 0.019533 *  
    ## date                                                 2.756 0.005919 ** 
    ## lag(value, 1)                                        4.252 2.24e-05 ***
    ## lag(value, 7)                                        3.838 0.000129 ***
    ## lag(value, 14)                                       3.562 0.000380 ***
    ## lag(value, 21)                                       2.673 0.007594 ** 
    ## lag(value, 28)                                       2.756 0.005909 ** 
    ## lag(value, 35)                                       2.708 0.006847 ** 
    ## lag(value, 42)                                       1.966 0.049440 *  
    ## lag(value, 49)                                       2.775 0.005581 ** 
    ## month(date, label = TRUE).L                         -1.879 0.060379 .  
    ## month(date, label = TRUE).Q                          0.793 0.427651    
    ## month(date, label = TRUE).C                         -2.627 0.008693 ** 
    ## month(date, label = TRUE)^4                         -1.425 0.154450    
    ## month(date, label = TRUE)^5                         -2.149 0.031812 *  
    ## month(date, label = TRUE)^6                         -0.643 0.520640    
    ## month(date, label = TRUE)^7                         -1.493 0.135741    
    ## month(date, label = TRUE)^8                         -0.693 0.488692    
    ## month(date, label = TRUE)^9                          1.177 0.239441    
    ## month(date, label = TRUE)^10                         0.597 0.550347    
    ## month(date, label = TRUE)^11                        -0.930 0.352372    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.957 7.92e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.277 0.001071 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.69 on 1590 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2386, Adjusted R-squared:  0.2281 
    ## F-statistic: 22.65 on 22 and 1590 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.24869910880825"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.80052205931593"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.62093858881777"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 91, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.6092993396382"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 91, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.6092993396382"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 91, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.68516548870093"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 91, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.68516548870093"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 91, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.27334510924467"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 91, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.27334510924467"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.95262328714603"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.95262328714603"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.31315363718415"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.31315363718415"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.01216251820149"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.01216251820149"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.98640944669942"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.48251795827986"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.26744312885382"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.26194552482611"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 91, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.26194552482611"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 91, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.71965960478927"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 91, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.71965960478927"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 91, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.30648287807644"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 91, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.30648287807644"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.99780459288683"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.61550948636956"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4766870172458"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 42, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.4766870172458"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.3469644492739"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 42, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 12.3469644492739"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 42, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.81239189898346"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 42, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.81239189898346"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.66386806320049"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.98061862470746"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.98061862470746"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.73134606535983"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.73134606535983"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.12243904029387"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.12243904029387"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.81808673964272"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.45772230311898"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.45772230311898"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.15372429114321"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.15372429114321"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.1570367788037"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.1570367788037"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.69124634374656"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.69124634374656"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.88375801208258"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.88375801208258"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.4568851581972"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.4568851581972"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.92565285605052"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.04233065649835"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.04233065649835"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.47067642885308"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.47067642885308"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.8367118992513"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.8367118992513"

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
    ## 1 healthyR.data <tibble [1,654 × 2]> <tibble [28 × 2]> <split [1626|28]>
    ## 2 healthyR      <tibble [1,648 × 2]> <tibble [28 × 2]> <split [1620|28]>
    ## 3 healthyR.ts   <tibble [1,592 × 2]> <tibble [28 × 2]> <split [1564|28]>
    ## 4 healthyverse  <tibble [1,562 × 2]> <tibble [28 × 2]> <split [1534|28]>
    ## 5 healthyR.ai   <tibble [1,387 × 2]> <tibble [28 × 2]> <split [1359|28]>
    ## 6 TidyDensity   <tibble [1,238 × 2]> <tibble [28 × 2]> <split [1210|28]>
    ## 7 tidyAML       <tibble [846 × 2]>   <tibble [28 × 2]> <split [818|28]> 
    ## 8 RandomWalker  <tibble [268 × 2]>   <tibble [28 × 2]> <split [240|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5536624 | 111.10263 | 0.6325289 | 120.48232 | 0.7497787 | 0.0958438 |
| healthyR.data | 2 | LM | Test | 0.5985022 | 170.88920 | 0.6837559 | 116.52596 | 0.7544078 | 0.0002363 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6647887 | 97.69035 | 0.7594846 | 185.84282 | 0.8964588 | 0.0000010 |
| healthyR | 1 | ARIMA | Test | 0.7082713 | 128.48902 | 0.8144576 | 160.64798 | 0.8429445 | 0.0570088 |
| healthyR | 2 | LM | Test | 0.6973646 | 96.50904 | 0.8019158 | 171.41342 | 0.8594124 | 0.0431175 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.6855031 | 98.11470 | 0.7882759 | 159.45240 | 0.8392401 | 0.0534675 |
| healthyR.ts | 1 | ARIMA | Test | 0.8262993 | 103.05604 | 0.8082756 | 176.40329 | 1.0786450 | 0.0000384 |
| healthyR.ts | 2 | LM | Test | 0.9869319 | 157.84748 | 0.9654044 | 155.25109 | 1.2370907 | 0.0081206 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8227763 | 99.32450 | 0.8048295 | 181.98832 | 1.0744881 | 0.0000018 |
| healthyverse | 1 | ARIMA | Test | 0.5943964 | 117.52311 | 1.1036172 | 72.63241 | 0.7443677 | 0.0223226 |
| healthyverse | 2 | LM | Test | 0.5843258 | 120.92629 | 1.0849192 | 72.25844 | 0.7128625 | 0.0819879 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6551241 | 93.13821 | 1.2163706 | 85.01251 | 0.8499811 | 0.0201545 |
| healthyR.ai | 1 | ARIMA | Test | 0.6225187 | 89.17266 | 0.7975759 | 148.83864 | 0.7971468 | 0.0025397 |
| healthyR.ai | 2 | LM | Test | 0.6099132 | 96.83274 | 0.7814256 | 135.79082 | 0.7601473 | 0.0003592 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.5989178 | 96.08131 | 0.7673382 | 130.13239 | 0.7543286 | 0.0047529 |
| TidyDensity | 1 | ARIMA | Test | 0.4810022 | 123.79753 | 0.9019095 | 110.00156 | 0.6302344 | 0.0026945 |
| TidyDensity | 2 | LM | Test | 0.6300411 | 212.69981 | 1.1813669 | 113.79368 | 0.8040310 | 0.0004645 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.4513391 | 87.96122 | 0.8462893 | 123.06412 | 0.5698410 | 0.0066585 |
| tidyAML | 1 | ARIMA | Test | 0.7449247 | 114.41136 | 0.8035075 | 123.88718 | 0.9215536 | 0.0097784 |
| tidyAML | 2 | LM | Test | 0.6748820 | 128.50228 | 0.7279564 | 93.91439 | 0.8633856 | 0.3404947 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6687902 | 117.12160 | 0.7213856 | 104.52495 | 0.8278084 | 0.0916808 |
| RandomWalker | 1 | ARIMA | Test | 1.2681640 | 142.86638 | 0.6665822 | 134.68352 | 1.5261294 | 0.0020465 |
| RandomWalker | 2 | LM | Test | 1.3041487 | 116.01415 | 0.6854968 | 190.93553 | 1.4533646 | 0.0275919 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.3333424 | 131.07721 | 0.7008418 | 166.12897 | 1.4928601 | 0.0087199 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.554 111.  0.633 120.  0.750 9.58e-2
    ## 2 healthyR             4 NNAR        Test  0.686  98.1 0.788 159.  0.839 5.35e-2
    ## 3 healthyR.ts          4 NNAR        Test  0.823  99.3 0.805 182.  1.07  1.82e-6
    ## 4 healthyverse         2 LM          Test  0.584 121.  1.08   72.3 0.713 8.20e-2
    ## 5 healthyR.ai          4 NNAR        Test  0.599  96.1 0.767 130.  0.754 4.75e-3
    ## 6 TidyDensity          4 NNAR        Test  0.451  88.0 0.846 123.  0.570 6.66e-3
    ## 7 tidyAML              4 NNAR        Test  0.669 117.  0.721 105.  0.828 9.17e-2
    ## 8 RandomWalker         2 LM          Test  1.30  116.  0.685 191.  1.45  2.76e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1626|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1620|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1564|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1534|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1359|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1210|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [818|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [240|28]>  <mdl_tm_t [1 × 5]>

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
