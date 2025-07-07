Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
07 July, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 144,707
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

The last day in the data set is 2025-07-05 22:52:15, the file was
birthed on: 2024-08-07 07:35:44.428716, and at report knit time is
-7979.28 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 144707        |
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
| r_version     |    104630 |          0.28 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    104630 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    104630 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12210 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-07-05 | 2023-07-18 | 1686 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131346.31 | 1513567.87 | 355 | 14701 | 292717 | 2367676 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10451.67 | 18582.57 | 1 | 282 | 3033 | 11827 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-07-05 22:52:15 | 2023-07-18 04:36:16 | 88845 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 12S |       60 |

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
    ## -148.77  -36.01  -11.32   26.72  816.06 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.722e+02  6.602e+01
    ## date                                                1.062e-02  3.495e-03
    ## lag(value, 1)                                       1.044e-01  2.412e-02
    ## lag(value, 7)                                       9.436e-02  2.497e-02
    ## lag(value, 14)                                      8.673e-02  2.496e-02
    ## lag(value, 21)                                      6.547e-02  2.514e-02
    ## lag(value, 28)                                      7.084e-02  2.502e-02
    ## lag(value, 35)                                      6.769e-02  2.510e-02
    ## lag(value, 42)                                      5.641e-02  2.521e-02
    ## lag(value, 49)                                      6.527e-02  2.508e-02
    ## month(date, label = TRUE).L                        -9.738e+00  5.112e+00
    ## month(date, label = TRUE).Q                         3.356e+00  5.064e+00
    ## month(date, label = TRUE).C                        -1.331e+01  5.128e+00
    ## month(date, label = TRUE)^4                        -6.801e+00  5.119e+00
    ## month(date, label = TRUE)^5                        -1.131e+01  5.110e+00
    ## month(date, label = TRUE)^6                        -4.076e+00  5.169e+00
    ## month(date, label = TRUE)^7                        -7.113e+00  5.071e+00
    ## month(date, label = TRUE)^8                        -2.998e+00  5.060e+00
    ## month(date, label = TRUE)^9                         5.284e+00  5.052e+00
    ## month(date, label = TRUE)^10                        2.619e+00  5.055e+00
    ## month(date, label = TRUE)^11                       -3.707e+00  5.039e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.182e+01  2.309e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.114e+00  2.430e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.608 0.009194 ** 
    ## date                                                 3.039 0.002413 ** 
    ## lag(value, 1)                                        4.326 1.61e-05 ***
    ## lag(value, 7)                                        3.779 0.000163 ***
    ## lag(value, 14)                                       3.475 0.000525 ***
    ## lag(value, 21)                                       2.605 0.009281 ** 
    ## lag(value, 28)                                       2.831 0.004693 ** 
    ## lag(value, 35)                                       2.696 0.007086 ** 
    ## lag(value, 42)                                       2.238 0.025378 *  
    ## lag(value, 49)                                       2.602 0.009343 ** 
    ## month(date, label = TRUE).L                         -1.905 0.056969 .  
    ## month(date, label = TRUE).Q                          0.663 0.507614    
    ## month(date, label = TRUE).C                         -2.595 0.009540 ** 
    ## month(date, label = TRUE)^4                         -1.329 0.184170    
    ## month(date, label = TRUE)^5                         -2.213 0.027063 *  
    ## month(date, label = TRUE)^6                         -0.789 0.430485    
    ## month(date, label = TRUE)^7                         -1.403 0.160883    
    ## month(date, label = TRUE)^8                         -0.593 0.553547    
    ## month(date, label = TRUE)^9                          1.046 0.295731    
    ## month(date, label = TRUE)^10                         0.518 0.604439    
    ## month(date, label = TRUE)^11                        -0.736 0.462070    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.118 3.46e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.340 0.000858 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.76 on 1614 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2379, Adjusted R-squared:  0.2275 
    ## F-statistic: 22.91 on 22 and 1614 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.79494014880398"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.763586289781"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.763586289781"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.56831032419192"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.56831032419192"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.8709055049956"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.8709055049956"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.81119048427799"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.97244851548342"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.64911715579515"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33730684712078"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29919934477378"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.29919934477378"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.75337142112892"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.75337142112892"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7, 42, 49, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.36898103399076"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7, 42, 49, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.36898103399076"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.20910094682021"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.82146516172513"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.82146516172513"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.8360807581238"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.8360807581238"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.57319885702709"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.57319885702709"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.12345258788546"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.12345258788546"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.0987440467045"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.0987440467045"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.9499529775252"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.9499529775252"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.4234742853424"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.72998725919597"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.72998725919597"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.68138121187211"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.68138121187211"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.17196484278094"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.17196484278094"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.17504024447452"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.16263276198373"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.0068977306322"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.0068977306322"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.73051438626619"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.73051438626619"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.68956918837649"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.68956918837649"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.52399095296943"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.52399095296943"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.8532715628493"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.8532715628493"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.38735085221818"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.38735085221818"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.9547684809782"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.9547684809782"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.77119338118984"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.77119338118984"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.5611974037516"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.5611974037516"

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
    ## 1 healthyR.data <tibble [1,678 × 2]> <tibble [28 × 2]> <split [1650|28]>
    ## 2 healthyR      <tibble [1,672 × 2]> <tibble [28 × 2]> <split [1644|28]>
    ## 3 healthyR.ts   <tibble [1,616 × 2]> <tibble [28 × 2]> <split [1588|28]>
    ## 4 healthyverse  <tibble [1,586 × 2]> <tibble [28 × 2]> <split [1558|28]>
    ## 5 healthyR.ai   <tibble [1,411 × 2]> <tibble [28 × 2]> <split [1383|28]>
    ## 6 TidyDensity   <tibble [1,262 × 2]> <tibble [28 × 2]> <split [1234|28]>
    ## 7 tidyAML       <tibble [870 × 2]>   <tibble [28 × 2]> <split [842|28]> 
    ## 8 RandomWalker  <tibble [292 × 2]>   <tibble [28 × 2]> <split [264|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6696444 | 142.64622 | 0.8120620 | 123.79946 | 0.8222990 | 0.0006096 |
| healthyR.data | 2 | LM | Test | 0.6645934 | 135.10359 | 0.8059367 | 125.58052 | 0.7971163 | 0.0169833 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.6807640 | 103.58245 | 0.8255464 | 193.28469 | 0.8071306 | 0.0003362 |
| healthyR | 1 | ARIMA | Test | 0.7604827 | 161.03578 | 0.7636827 | 140.89418 | 0.9789391 | 0.0157208 |
| healthyR | 2 | LM | Test | 0.7221395 | 97.92542 | 0.7251782 | 156.18356 | 0.9535844 | 0.0121643 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.7292135 | 105.75215 | 0.7322820 | 156.50083 | 0.9562339 | 0.0090045 |
| healthyR.ts | 1 | ARIMA | Test | 0.8939975 | 118.66900 | 0.6733030 | 162.15207 | 1.1656992 | 0.0145338 |
| healthyR.ts | 2 | LM | Test | 0.8867251 | 157.91313 | 0.6678259 | 141.85174 | 1.1254833 | 0.0590259 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8461026 | 96.42447 | 0.6372315 | 180.38664 | 1.1215578 | 0.0031359 |
| healthyverse | 1 | ARIMA | Test | 0.6719695 | 230.60320 | 0.7237650 | 90.63165 | 0.8515189 | 0.0043996 |
| healthyverse | 2 | LM | Test | 0.6778932 | 232.75148 | 0.7301453 | 91.39862 | 0.8501451 | 0.0129810 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6487525 | 138.19815 | 0.6987584 | 98.93304 | 0.8520009 | 0.0362540 |
| healthyR.ai | 1 | ARIMA | Test | 0.6504106 | 113.79751 | 0.7206630 | 137.28643 | 0.8002726 | 0.0002178 |
| healthyR.ai | 2 | LM | Test | 0.6479353 | 105.63925 | 0.7179204 | 149.50037 | 0.7847582 | 0.0252058 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.6631750 | 111.76763 | 0.7348061 | 144.20956 | 0.8000403 | 0.0019985 |
| TidyDensity | 1 | ARIMA | Test | 0.5499243 | 92.28827 | 0.7055951 | 113.60507 | 0.7606906 | 0.0061849 |
| TidyDensity | 2 | LM | Test | 0.5361476 | 132.68512 | 0.6879186 | 89.57911 | 0.7447402 | 0.0056158 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 0.5795628 | 89.45707 | 0.7436236 | 126.21211 | 0.7934986 | 0.0371365 |
| tidyAML | 1 | ARIMA | Test | 0.6387131 | 157.15177 | 0.7520949 | 96.46468 | 0.7894721 | 0.0026056 |
| tidyAML | 2 | LM | Test | 0.6428515 | 176.40536 | 0.7569679 | 95.63240 | 0.7824123 | 0.0466514 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.6225418 | 171.52600 | 0.7330529 | 92.71339 | 0.7615977 | 0.1218116 |
| RandomWalker | 1 | ARIMA | Test | 1.1822135 | 110.09881 | 0.6507939 | 163.57572 | 1.3354801 | 0.0200254 |
| RandomWalker | 2 | LM | Test | 1.1863822 | 98.21543 | 0.6530887 | 180.39908 | 1.3390607 | 0.0601026 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.2187334 | 145.04895 | 0.6708976 | 149.25996 | 1.3789816 | 0.0054162 |

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
    ## 1 healthyR.da…         2 LM          Test  0.665 135.  0.806 126.  0.797 0.0170 
    ## 2 healthyR             2 LM          Test  0.722  97.9 0.725 156.  0.954 0.0122 
    ## 3 healthyR.ts          4 NNAR        Test  0.846  96.4 0.637 180.  1.12  0.00314
    ## 4 healthyverse         2 LM          Test  0.678 233.  0.730  91.4 0.850 0.0130 
    ## 5 healthyR.ai          2 LM          Test  0.648 106.  0.718 150.  0.785 0.0252 
    ## 6 TidyDensity          2 LM          Test  0.536 133.  0.688  89.6 0.745 0.00562
    ## 7 tidyAML              4 NNAR        Test  0.623 172.  0.733  92.7 0.762 0.122  
    ## 8 RandomWalker         1 ARIMA       Test  1.18  110.  0.651 164.  1.34  0.0200

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1650|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1644|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1588|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1558|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1383|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1234|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [842|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [264|28]>  <mdl_tm_t [1 × 5]>

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
