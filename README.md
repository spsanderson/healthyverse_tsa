Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 133,663
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

The last day in the data set is 2025-03-08 19:51:04, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.351188^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 133663        |
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
| r_version     |     95990 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     95990 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     95990 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11261 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-08 | 2023-05-16 | 1567 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136243.25 | 1526413.44 | 355 | 14701 | 260603 | 2367791 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10389.21 | 18372.22 | 1 | 300 | 3091 | 11871 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-08 19:51:04 | 2023-05-16 12:53:46 | 81039 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 8M 18S |       60 |

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
    ## -154.01  -34.91  -10.27   26.76  810.59 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.086e+02  7.553e+01
    ## date                                                1.241e-02  4.007e-03
    ## lag(value, 1)                                       1.117e-01  2.502e-02
    ## lag(value, 7)                                       8.954e-02  2.592e-02
    ## lag(value, 14)                                      9.421e-02  2.595e-02
    ## lag(value, 21)                                      6.819e-02  2.612e-02
    ## lag(value, 28)                                      6.503e-02  2.601e-02
    ## lag(value, 35)                                      6.785e-02  2.622e-02
    ## lag(value, 42)                                      5.428e-02  2.633e-02
    ## lag(value, 49)                                      8.716e-02  2.624e-02
    ## month(date, label = TRUE).L                        -1.202e+01  5.189e+00
    ## month(date, label = TRUE).Q                         2.420e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.114e+01  5.272e+00
    ## month(date, label = TRUE)^4                        -8.434e+00  5.243e+00
    ## month(date, label = TRUE)^5                        -1.189e+01  5.209e+00
    ## month(date, label = TRUE)^6                        -2.387e+00  5.291e+00
    ## month(date, label = TRUE)^7                        -7.898e+00  5.221e+00
    ## month(date, label = TRUE)^8                        -3.204e+00  5.257e+00
    ## month(date, label = TRUE)^9                         4.681e+00  5.291e+00
    ## month(date, label = TRUE)^10                        5.007e+00  5.313e+00
    ## month(date, label = TRUE)^11                       -6.225e+00  5.332e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.191e+01  2.406e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.806e+00  2.536e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.761 0.005825 ** 
    ## date                                                 3.097 0.001991 ** 
    ## lag(value, 1)                                        4.463 8.67e-06 ***
    ## lag(value, 7)                                        3.455 0.000566 ***
    ## lag(value, 14)                                       3.630 0.000292 ***
    ## lag(value, 21)                                       2.611 0.009126 ** 
    ## lag(value, 28)                                       2.500 0.012515 *  
    ## lag(value, 35)                                       2.588 0.009746 ** 
    ## lag(value, 42)                                       2.062 0.039403 *  
    ## lag(value, 49)                                       3.322 0.000915 ***
    ## month(date, label = TRUE).L                         -2.316 0.020696 *  
    ## month(date, label = TRUE).Q                          0.466 0.641290    
    ## month(date, label = TRUE).C                         -2.114 0.034699 *  
    ## month(date, label = TRUE)^4                         -1.609 0.107932    
    ## month(date, label = TRUE)^5                         -2.283 0.022596 *  
    ## month(date, label = TRUE)^6                         -0.451 0.651961    
    ## month(date, label = TRUE)^7                         -1.513 0.130531    
    ## month(date, label = TRUE)^8                         -0.609 0.542302    
    ## month(date, label = TRUE)^9                          0.885 0.376478    
    ## month(date, label = TRUE)^10                         0.942 0.346103    
    ## month(date, label = TRUE)^11                        -1.168 0.243172    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.950 8.28e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.078 0.002119 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.52 on 1495 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.259,  Adjusted R-squared:  0.2481 
    ## F-statistic: 23.75 on 22 and 1495 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.32652752142154"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.11283734411229"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.11283734411229"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.22887064604039"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.22887064604039"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.0808371364507"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.0808371364507"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.37082477202484"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.20277512471357"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16709526485828"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.16709526485828"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.86248323229053"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.86248323229053"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.40075855240982"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.40075855240982"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.59847745729872"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46628542116208"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.46628542116208"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.05004340579222"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.05004340579222"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.70623741360271"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.70623741360271"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.46367697812714"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.2282463293256"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.17285792759821"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.17254566312021"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 98, 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.17254566312021"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.29664420372374"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.29664420372374"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 98, 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.19040133421232"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 98, 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.19040133421232"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.44436857794727"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.36212500194688"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.35256393338232"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.35256393338232"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.32280736600664"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.32280736600664"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.69172336681084"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.69172336681084"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.98882713186769"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.98882713186769"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.21968515776871"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.21968515776871"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.79403030209085"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.79403030209085"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.12923821703387"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.11674491707574"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 49 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.11674491707574"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.0287934406637"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 49 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.0287934406637"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 49 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.0653659876406"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 49 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.0653659876406"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.47985358741797"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.11798818915202"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.11798818915202"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.16333714897218"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.16333714897218"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.49411624725024"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.49411624725024"

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
    ## 1 healthyR.data <tibble [1,560 × 2]> <tibble [28 × 2]> <split [1532|28]>
    ## 2 healthyR      <tibble [1,553 × 2]> <tibble [28 × 2]> <split [1525|28]>
    ## 3 healthyR.ts   <tibble [1,497 × 2]> <tibble [28 × 2]> <split [1469|28]>
    ## 4 healthyverse  <tibble [1,468 × 2]> <tibble [28 × 2]> <split [1440|28]>
    ## 5 healthyR.ai   <tibble [1,292 × 2]> <tibble [28 × 2]> <split [1264|28]>
    ## 6 TidyDensity   <tibble [1,143 × 2]> <tibble [28 × 2]> <split [1115|28]>
    ## 7 tidyAML       <tibble [751 × 2]>   <tibble [28 × 2]> <split [723|28]> 
    ## 8 RandomWalker  <tibble [173 × 2]>   <tibble [28 × 2]> <split [145|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6669583 | 170.97772 | 0.6772493 | 150.59286 | 0.7881822 | 0.1421806 |
| healthyR.data | 2 | LM | Test | 0.7840220 | 214.69142 | 0.7961193 | 148.66005 | 0.9205432 | 0.0605364 |
| healthyR.data | 3 | EARTH | Test | 0.6917826 | 183.53582 | 0.7024566 | 125.74225 | 0.8991284 | 0.0605364 |
| healthyR.data | 4 | NNAR | Test | 0.6796011 | 114.94980 | 0.6900872 | 169.67015 | 0.8192542 | 0.0073608 |
| healthyR | 1 | ARIMA | Test | 0.8198744 | 169.73330 | 0.7261129 | 171.31434 | 0.9996576 | 0.0071818 |
| healthyR | 2 | LM | Test | 0.8288189 | 114.76444 | 0.7340346 | 182.56342 | 1.0137475 | 0.0367363 |
| healthyR | 3 | EARTH | Test | 0.8471942 | 145.97463 | 0.7503085 | 167.02179 | 1.0360136 | 0.0367363 |
| healthyR | 4 | NNAR | Test | 0.8027531 | 148.49868 | 0.7109496 | 157.76664 | 0.9779835 | 0.0588626 |
| healthyR.ts | 1 | ARIMA | Test | 0.9695902 | 114.40050 | 0.6538214 | 139.81757 | 1.1561180 | 0.0783122 |
| healthyR.ts | 2 | LM | Test | 0.9346800 | 120.11191 | 0.6302805 | 124.73829 | 1.1216329 | 0.0783122 |
| healthyR.ts | 3 | EARTH | Test | 0.9234248 | 121.97951 | 0.6226908 | 120.42608 | 1.1132523 | 0.0783122 |
| healthyR.ts | 4 | NNAR | Test | 0.9215509 | 103.00303 | 0.6214272 | 167.49583 | 1.1241794 | 0.1275285 |
| healthyverse | 1 | ARIMA | Test | 0.6187711 | 129.75713 | 0.8052899 | 107.43203 | 0.8137142 | 0.0386544 |
| healthyverse | 2 | LM | Test | 0.7240425 | 188.13369 | 0.9422938 | 108.61033 | 0.9096897 | 0.1047770 |
| healthyverse | 3 | EARTH | Test | 0.6260992 | 135.12184 | 0.8148270 | 106.58183 | 0.8224783 | 0.1047770 |
| healthyverse | 4 | NNAR | Test | 0.6067654 | 121.95665 | 0.7896653 | 114.08620 | 0.7793890 | 0.0808713 |
| healthyR.ai | 1 | ARIMA | Test | 0.7485838 | 96.29836 | 0.6821222 | 162.72855 | 0.8962388 | 0.1018281 |
| healthyR.ai | 2 | LM | Test | 0.7930867 | 97.33422 | 0.7226740 | 149.39732 | 0.9727010 | 0.0990356 |
| healthyR.ai | 3 | EARTH | Test | 0.7929067 | 99.19101 | 0.7225100 | 140.70691 | 0.9889261 | 0.0990356 |
| healthyR.ai | 4 | NNAR | Test | 0.6843929 | 89.47548 | 0.6236304 | 139.13219 | 0.8412601 | 0.1794161 |
| TidyDensity | 1 | ARIMA | Test | 0.6791496 | 114.55471 | 0.6642872 | 111.00050 | 0.8160469 | 0.0904804 |
| TidyDensity | 2 | LM | Test | 0.6897918 | 164.67147 | 0.6746965 | 101.89908 | 0.8044294 | 0.0774872 |
| TidyDensity | 3 | EARTH | Test | 0.7145304 | 112.96019 | 0.6988937 | 119.11381 | 0.8677687 | 0.0774872 |
| TidyDensity | 4 | NNAR | Test | 0.7594066 | 104.51121 | 0.7427878 | 151.56232 | 0.9309760 | 0.0341232 |
| tidyAML | 1 | ARIMA | Test | 0.6662460 | 182.09598 | 0.7094099 | 100.13239 | 0.7725491 | 0.0100536 |
| tidyAML | 2 | LM | Test | 0.6380272 | 165.55657 | 0.6793629 | 98.04393 | 0.7625161 | 0.0604446 |
| tidyAML | 3 | EARTH | Test | 0.6893231 | 198.31825 | 0.7339822 | 99.19162 | 0.8010868 | 0.0604446 |
| tidyAML | 4 | NNAR | Test | 0.6342302 | 165.96926 | 0.6753199 | 99.15857 | 0.7633663 | 0.0000835 |
| RandomWalker | 1 | ARIMA | Test | 0.8977435 | 167.45921 | 0.4221469 | 91.79265 | 1.0489949 | 0.4882359 |
| RandomWalker | 2 | LM | Test | 1.2791167 | 111.82559 | 0.6014805 | 194.01633 | 1.4475052 | 0.0032509 |
| RandomWalker | 3 | EARTH | Test | 1.2100109 | 84.99721 | 0.5689848 | 153.25276 | 1.4471258 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3093063 | 127.38087 | 0.6156766 | 168.81176 | 1.4965906 | 0.0023461 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.667 171.  0.677 151.  0.788 0.142 
    ## 2 healthyR              4 NNAR        Test  0.803 148.  0.711 158.  0.978 0.0589
    ## 3 healthyR.ts           3 EARTH       Test  0.923 122.  0.623 120.  1.11  0.0783
    ## 4 healthyverse          4 NNAR        Test  0.607 122.  0.790 114.  0.779 0.0809
    ## 5 healthyR.ai           4 NNAR        Test  0.684  89.5 0.624 139.  0.841 0.179 
    ## 6 TidyDensity           2 LM          Test  0.690 165.  0.675 102.  0.804 0.0775
    ## 7 tidyAML               2 LM          Test  0.638 166.  0.679  98.0 0.763 0.0604
    ## 8 RandomWalker          1 ARIMA       Test  0.898 167.  0.422  91.8 1.05  0.488

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1532|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1525|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1469|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1440|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1264|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1115|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [723|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [145|28]>  <mdl_tm_t [1 × 5]>

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
