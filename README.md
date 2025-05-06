Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,976
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

The last day in the data set is 2025-05-04 22:42:34, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6491.11
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138976        |
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
| r_version     |    100136 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100136 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100136 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11787 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-04 | 2023-06-15 | 1624 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133771.23 | 1519941.93 | 355 | 14701 | 278455 | 2367746.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10380.44 | 18455.29 | 1 | 292 | 3058 | 11643.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-04 22:42:34 | 2023-06-15 05:27:32 | 84759 |

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
    ## -146.68  -35.83  -11.03   26.67  814.72 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.825e+02  7.109e+01
    ## date                                                1.115e-02  3.769e-03
    ## lag(value, 1)                                       1.049e-01  2.460e-02
    ## lag(value, 7)                                       9.743e-02  2.549e-02
    ## lag(value, 14)                                      9.192e-02  2.554e-02
    ## lag(value, 21)                                      6.776e-02  2.563e-02
    ## lag(value, 28)                                      6.717e-02  2.559e-02
    ## lag(value, 35)                                      6.689e-02  2.566e-02
    ## lag(value, 42)                                      4.908e-02  2.583e-02
    ## lag(value, 49)                                      6.696e-02  2.564e-02
    ## month(date, label = TRUE).L                        -1.002e+01  5.129e+00
    ## month(date, label = TRUE).Q                         2.991e+00  5.186e+00
    ## month(date, label = TRUE).C                        -1.257e+01  5.176e+00
    ## month(date, label = TRUE)^4                        -6.654e+00  5.198e+00
    ## month(date, label = TRUE)^5                        -1.202e+01  5.186e+00
    ## month(date, label = TRUE)^6                        -3.230e+00  5.238e+00
    ## month(date, label = TRUE)^7                        -6.451e+00  5.160e+00
    ## month(date, label = TRUE)^8                        -4.200e+00  5.157e+00
    ## month(date, label = TRUE)^9                         5.325e+00  5.147e+00
    ## month(date, label = TRUE)^10                        4.368e+00  5.214e+00
    ## month(date, label = TRUE)^11                       -5.681e+00  5.321e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.159e+01  2.368e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.089e+00  2.491e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.567 0.010359 *  
    ## date                                                 2.959 0.003136 ** 
    ## lag(value, 1)                                        4.265 2.12e-05 ***
    ## lag(value, 7)                                        3.822 0.000138 ***
    ## lag(value, 14)                                       3.599 0.000329 ***
    ## lag(value, 21)                                       2.644 0.008278 ** 
    ## lag(value, 28)                                       2.624 0.008769 ** 
    ## lag(value, 35)                                       2.607 0.009217 ** 
    ## lag(value, 42)                                       1.900 0.057573 .  
    ## lag(value, 49)                                       2.612 0.009087 ** 
    ## month(date, label = TRUE).L                         -1.954 0.050871 .  
    ## month(date, label = TRUE).Q                          0.577 0.564179    
    ## month(date, label = TRUE).C                         -2.428 0.015308 *  
    ## month(date, label = TRUE)^4                         -1.280 0.200679    
    ## month(date, label = TRUE)^5                         -2.319 0.020539 *  
    ## month(date, label = TRUE)^6                         -0.617 0.537527    
    ## month(date, label = TRUE)^7                         -1.250 0.211380    
    ## month(date, label = TRUE)^8                         -0.814 0.415523    
    ## month(date, label = TRUE)^9                          1.035 0.301008    
    ## month(date, label = TRUE)^10                         0.838 0.402383    
    ## month(date, label = TRUE)^11                        -1.068 0.285854    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.891 1.11e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.248 0.001189 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.82 on 1552 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.243,  Adjusted R-squared:  0.2323 
    ## F-statistic: 22.64 on 22 and 1552 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.05460535104967"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.91049440527198"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.8333621024445"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.8333621024445"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.18300491367617"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.18300491367617"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.82687057563127"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.82687057563127"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.93280921733044"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.93280921733044"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.68524923500258"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.68524923500258"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.22550556237763"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.22550556237763"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.15114102451886"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.49676440027167"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.49676440027167"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.02247786559462"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.02247786559462"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.500975618038"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.500975618038"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.00103295207217"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.30119916423494"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.30119916423494"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.2025471320075"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.2025471320075"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.61645043601383"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.61645043601383"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.48115577703638"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.48115577703638"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.702839025282"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 14.702839025282"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.1830387553084"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.1830387553084"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.44721884129796"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.44721884129796"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.79275574661588"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.79275574661588"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.28419792599634"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.28419792599634"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.36558050229902"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.36558050229902"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.43194687021707"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.43194687021707"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.14355253182091"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.14355253182091"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.78300145716886"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.19191050506486"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.19191050506486"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.60669742806282"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.60669742806282"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.23795055851347"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.23795055851347"

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
    ## 1 healthyR.data <tibble [1,617 × 2]> <tibble [28 × 2]> <split [1589|28]>
    ## 2 healthyR      <tibble [1,610 × 2]> <tibble [28 × 2]> <split [1582|28]>
    ## 3 healthyR.ts   <tibble [1,554 × 2]> <tibble [28 × 2]> <split [1526|28]>
    ## 4 healthyverse  <tibble [1,524 × 2]> <tibble [28 × 2]> <split [1496|28]>
    ## 5 healthyR.ai   <tibble [1,349 × 2]> <tibble [28 × 2]> <split [1321|28]>
    ## 6 TidyDensity   <tibble [1,200 × 2]> <tibble [28 × 2]> <split [1172|28]>
    ## 7 tidyAML       <tibble [808 × 2]>   <tibble [28 × 2]> <split [780|28]> 
    ## 8 RandomWalker  <tibble [230 × 2]>   <tibble [28 × 2]> <split [202|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6897071 | 136.31366 | 0.6176452 | 138.88323 | 0.8425241 | 0.0042391 |
| healthyR.data | 2 | LM | Test | 0.7063823 | 172.10382 | 0.6325782 | 129.02257 | 0.8431934 | 0.0689914 |
| healthyR.data | 3 | EARTH | Test | 0.8929369 | 358.15378 | 0.7996412 | 121.45211 | 1.0400334 | 0.0689914 |
| healthyR.data | 4 | NNAR | Test | 0.7462710 | 98.64134 | 0.6682992 | 176.67119 | 0.9150898 | 0.0661592 |
| healthyR | 1 | ARIMA | Test | 0.6411724 | 96.43891 | 0.7181118 | 171.03626 | 0.7988841 | 0.0006913 |
| healthyR | 2 | LM | Test | 0.6385635 | 95.53470 | 0.7151898 | 173.81467 | 0.7892635 | 0.1398678 |
| healthyR | 3 | EARTH | Test | 0.6231449 | 97.99132 | 0.6979210 | 144.18950 | 0.7666498 | 0.1398678 |
| healthyR | 4 | NNAR | Test | 0.6520723 | 97.17328 | 0.7303196 | 155.83006 | 0.7944664 | 0.0303218 |
| healthyR.ts | 1 | ARIMA | Test | 0.7772362 | 98.31935 | 0.6806989 | 173.86157 | 0.9578920 | 0.0811851 |
| healthyR.ts | 2 | LM | Test | 0.8696079 | 220.19632 | 0.7615976 | 138.31296 | 1.1067880 | 0.0811851 |
| healthyR.ts | 3 | EARTH | Test | 0.9238407 | 366.10740 | 0.8090943 | 129.95711 | 1.0935304 | 0.0811851 |
| healthyR.ts | 4 | NNAR | Test | 0.7904478 | 131.34971 | 0.6922696 | 180.99971 | 0.9602636 | 0.0000106 |
| healthyverse | 1 | ARIMA | Test | 0.6120380 | 333.53465 | 0.8385758 | 99.14934 | 0.7464965 | 0.0275924 |
| healthyverse | 2 | LM | Test | 0.6284811 | 370.29211 | 0.8611051 | 98.94555 | 0.7645975 | 0.0301730 |
| healthyverse | 3 | EARTH | Test | 0.8221185 | 611.51905 | 1.1264149 | 103.78361 | 0.9572841 | 0.0301730 |
| healthyverse | 4 | NNAR | Test | 0.5943996 | 224.48449 | 0.8144088 | 105.82692 | 0.7517132 | 0.0370062 |
| healthyR.ai | 1 | ARIMA | Test | 0.7918057 | 132.96030 | 0.8334935 | 165.36767 | 1.0344838 | 0.0458351 |
| healthyR.ai | 2 | LM | Test | 0.7829073 | 129.51101 | 0.8241266 | 163.64247 | 1.0431598 | 0.0281300 |
| healthyR.ai | 3 | EARTH | Test | 1.3666750 | 419.99474 | 1.4386290 | 151.38317 | 1.6440547 | 0.0281300 |
| healthyR.ai | 4 | NNAR | Test | 0.8077790 | 147.27325 | 0.8503077 | 161.19605 | 1.0638738 | 0.0023536 |
| TidyDensity | 1 | ARIMA | Test | 0.5628191 | 353.11056 | 0.8524070 | 118.88207 | 0.6816549 | 0.0151924 |
| TidyDensity | 2 | LM | Test | 0.6249402 | 469.08090 | 0.9464913 | 113.65867 | 0.7621450 | 0.0909762 |
| TidyDensity | 3 | EARTH | Test | 0.5543265 | 302.80026 | 0.8395446 | 118.44261 | 0.6823512 | 0.0909762 |
| TidyDensity | 4 | NNAR | Test | 0.5300514 | 187.44692 | 0.8027793 | 134.43658 | 0.6801537 | 0.0185573 |
| tidyAML | 1 | ARIMA | Test | 0.6594784 | 206.32202 | 0.9438974 | 103.94982 | 0.7910916 | 0.0135644 |
| tidyAML | 2 | LM | Test | 0.6696215 | 256.12062 | 0.9584150 | 99.78051 | 0.8044196 | 0.1627218 |
| tidyAML | 3 | EARTH | Test | 0.6785527 | 185.17974 | 0.9711980 | 113.71382 | 0.8199817 | 0.1627218 |
| tidyAML | 4 | NNAR | Test | 0.6655400 | 248.14065 | 0.9525733 | 99.63242 | 0.8014352 | 0.0260582 |
| RandomWalker | 1 | ARIMA | Test | 1.4418391 | 133.91224 | 0.6488542 | 163.01942 | 1.7537009 | 0.3391873 |
| RandomWalker | 2 | LM | Test | 1.2712327 | 113.97826 | 0.5720782 | 171.13788 | 1.4991335 | 0.0069610 |
| RandomWalker | 3 | EARTH | Test | 1.2717000 | 113.46399 | 0.5722885 | 172.12822 | 1.4981879 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2998380 | 143.21678 | 0.5849511 | 164.80102 | 1.4835730 | 0.0128753 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.690 136.  0.618 139.  0.843 0.00424
    ## 2 healthyR             3 EARTH       Test  0.623  98.0 0.698 144.  0.767 0.140  
    ## 3 healthyR.ts          1 ARIMA       Test  0.777  98.3 0.681 174.  0.958 0.0812 
    ## 4 healthyverse         1 ARIMA       Test  0.612 334.  0.839  99.1 0.746 0.0276 
    ## 5 healthyR.ai          1 ARIMA       Test  0.792 133.  0.833 165.  1.03  0.0458 
    ## 6 TidyDensity          4 NNAR        Test  0.530 187.  0.803 134.  0.680 0.0186 
    ## 7 tidyAML              1 ARIMA       Test  0.659 206.  0.944 104.  0.791 0.0136 
    ## 8 RandomWalker         4 NNAR        Test  1.30  143.  0.585 165.  1.48  0.0129

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1589|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1582|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1526|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1496|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1321|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1172|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [780|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [202|28]>  <mdl_tm_t [1 × 5]>

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
