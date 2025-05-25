Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,721
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

The last day in the data set is 2025-05-22 19:23:42, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.531142^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140721        |
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
| r_version     |    101436 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    101436 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101436 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11986 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-22 | 2023-06-26 | 1642 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133981.14 | 1518442.96 | 355 | 14701 | 289681 | 2367744 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10440.13 | 18591.44 | 1 | 288 | 3058 | 11715 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-22 19:23:42 | 2023-06-26 02:15:49 | 86066 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 56S |       60 |

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
    ## -147.70  -35.70  -11.11   26.61  814.71 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.749e+02  6.940e+01
    ## date                                                1.076e-02  3.678e-03
    ## lag(value, 1)                                       1.027e-01  2.449e-02
    ## lag(value, 7)                                       9.605e-02  2.532e-02
    ## lag(value, 14)                                      8.915e-02  2.532e-02
    ## lag(value, 21)                                      6.796e-02  2.544e-02
    ## lag(value, 28)                                      7.076e-02  2.536e-02
    ## lag(value, 35)                                      6.510e-02  2.546e-02
    ## lag(value, 42)                                      4.839e-02  2.563e-02
    ## lag(value, 49)                                      7.086e-02  2.546e-02
    ## month(date, label = TRUE).L                        -9.884e+00  5.117e+00
    ## month(date, label = TRUE).Q                         3.372e+00  5.151e+00
    ## month(date, label = TRUE).C                        -1.294e+01  5.145e+00
    ## month(date, label = TRUE)^4                        -6.941e+00  5.187e+00
    ## month(date, label = TRUE)^5                        -1.166e+01  5.140e+00
    ## month(date, label = TRUE)^6                        -3.382e+00  5.233e+00
    ## month(date, label = TRUE)^7                        -6.893e+00  5.119e+00
    ## month(date, label = TRUE)^8                        -3.941e+00  5.126e+00
    ## month(date, label = TRUE)^9                         5.558e+00  5.132e+00
    ## month(date, label = TRUE)^10                        3.852e+00  5.127e+00
    ## month(date, label = TRUE)^11                       -5.294e+00  5.266e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.170e+01  2.346e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.150e+00  2.469e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.520 0.011834 *  
    ## date                                                 2.926 0.003479 ** 
    ## lag(value, 1)                                        4.192 2.92e-05 ***
    ## lag(value, 7)                                        3.794 0.000154 ***
    ## lag(value, 14)                                       3.521 0.000442 ***
    ## lag(value, 21)                                       2.672 0.007627 ** 
    ## lag(value, 28)                                       2.790 0.005329 ** 
    ## lag(value, 35)                                       2.557 0.010643 *  
    ## lag(value, 42)                                       1.888 0.059202 .  
    ## lag(value, 49)                                       2.783 0.005447 ** 
    ## month(date, label = TRUE).L                         -1.931 0.053602 .  
    ## month(date, label = TRUE).Q                          0.655 0.512757    
    ## month(date, label = TRUE).C                         -2.514 0.012038 *  
    ## month(date, label = TRUE)^4                         -1.338 0.180988    
    ## month(date, label = TRUE)^5                         -2.269 0.023389 *  
    ## month(date, label = TRUE)^6                         -0.646 0.518192    
    ## month(date, label = TRUE)^7                         -1.347 0.178333    
    ## month(date, label = TRUE)^8                         -0.769 0.442168    
    ## month(date, label = TRUE)^9                          1.083 0.278924    
    ## month(date, label = TRUE)^10                         0.751 0.452571    
    ## month(date, label = TRUE)^11                        -1.005 0.314863    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.988 6.77e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.301 0.000986 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.77 on 1570 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2416, Adjusted R-squared:  0.231 
    ## F-statistic: 22.74 on 22 and 1570 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.20800583937519"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0415801194674"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.0415801194674"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.90366389091296"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.90366389091296"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.71559735539646"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.71559735539646"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.26077333643916"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.26077333643916"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.98180149637745"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.98180149637745"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.92710155951893"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.92710155951893"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.14386574662679"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.14386574662679"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.84051651478852"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.84051651478852"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.62560843953"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.62560843953"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.67718969740643"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.35667584242402"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.35667584242402"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.03922494630705"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.03922494630705"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.9121657070267"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.9121657070267"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.8373772465808"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.77846141806471"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.77846141806471"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.0794582612464"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.0794582612464"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.89605674945935"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.89605674945935"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.24483706579601"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.24483706579601"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.59392230277257"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.59392230277257"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.27521593507703"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.27521593507703"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.89359875702772"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.67087644006994"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.67087644006994"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.79270362663474"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.79270362663474"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.48744597146252"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.48744597146252"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.61228570630295"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.24126925390486"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.24126925390486"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.08137323489765"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.08137323489765"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.29596108552926"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.29596108552926"

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
    ## 1 healthyR.data <tibble [1,635 × 2]> <tibble [28 × 2]> <split [1607|28]>
    ## 2 healthyR      <tibble [1,628 × 2]> <tibble [28 × 2]> <split [1600|28]>
    ## 3 healthyR.ts   <tibble [1,572 × 2]> <tibble [28 × 2]> <split [1544|28]>
    ## 4 healthyverse  <tibble [1,542 × 2]> <tibble [28 × 2]> <split [1514|28]>
    ## 5 healthyR.ai   <tibble [1,367 × 2]> <tibble [28 × 2]> <split [1339|28]>
    ## 6 TidyDensity   <tibble [1,218 × 2]> <tibble [28 × 2]> <split [1190|28]>
    ## 7 tidyAML       <tibble [826 × 2]>   <tibble [28 × 2]> <split [798|28]> 
    ## 8 RandomWalker  <tibble [248 × 2]>   <tibble [28 × 2]> <split [220|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6094574 | 116.69346 | 0.6671413 | 131.15273 | 0.8236186 | 0.0065964 |
| healthyR.data | 2 | LM | Test | 0.6040768 | 187.67541 | 0.6612514 | 112.51991 | 0.8032826 | 0.0142054 |
| healthyR.data | 3 | EARTH | Test | 0.6081344 | 194.97817 | 0.6656932 | 110.69252 | 0.8090467 | 0.0142054 |
| healthyR.data | 4 | NNAR | Test | 0.7216462 | 136.73730 | 0.7899486 | 173.69640 | 0.9044674 | 0.0093676 |
| healthyR | 1 | ARIMA | Test | 0.6231816 | 98.65035 | 0.7462954 | 174.96922 | 0.7958791 | 0.0055449 |
| healthyR | 2 | LM | Test | 0.6021145 | 93.83296 | 0.7210663 | 163.98524 | 0.7707576 | 0.0679723 |
| healthyR | 3 | EARTH | Test | 0.5617998 | 100.46585 | 0.6727872 | 128.33923 | 0.7283414 | 0.0679723 |
| healthyR | 4 | NNAR | Test | 0.5938707 | 90.20345 | 0.7111939 | 136.00820 | 0.7683758 | 0.0007774 |
| healthyR.ts | 1 | ARIMA | Test | 0.6660585 | 102.70372 | 0.6897723 | 178.58301 | 0.8206874 | 0.0338602 |
| healthyR.ts | 2 | LM | Test | 0.8795070 | 232.76638 | 0.9108202 | 159.38858 | 1.0400042 | 0.0338602 |
| healthyR.ts | 3 | EARTH | Test | 0.5770732 | 195.24984 | 0.5976188 | 107.16207 | 0.7718866 | 0.0338602 |
| healthyR.ts | 4 | NNAR | Test | 0.6684463 | 105.40332 | 0.6922451 | 176.52478 | 0.8233208 | 0.0244860 |
| healthyverse | 1 | ARIMA | Test | 0.4946158 | 212.04777 | 0.7783340 | 79.43975 | 0.6548384 | 0.0000000 |
| healthyverse | 2 | LM | Test | 0.5024344 | 294.36478 | 0.7906374 | 76.85327 | 0.6214853 | 0.0017895 |
| healthyverse | 3 | EARTH | Test | 0.5266694 | 191.17573 | 0.8287740 | 87.75207 | 0.6962734 | 0.0017895 |
| healthyverse | 4 | NNAR | Test | 0.5256992 | 131.74661 | 0.8272472 | 92.82821 | 0.6993803 | 0.1142295 |
| healthyR.ai | 1 | ARIMA | Test | 0.6836739 | 95.57944 | 0.8559269 | 145.98264 | 1.0125746 | 0.1029909 |
| healthyR.ai | 2 | LM | Test | 0.6700295 | 106.05717 | 0.8388448 | 136.26881 | 0.9678470 | 0.2725545 |
| healthyR.ai | 3 | EARTH | Test | 0.6641089 | 122.78883 | 0.8314325 | 125.14996 | 0.9759422 | 0.2725545 |
| healthyR.ai | 4 | NNAR | Test | 0.6774452 | 122.54356 | 0.8481289 | 138.79025 | 0.9764629 | 0.0016885 |
| TidyDensity | 1 | ARIMA | Test | 0.4189751 | 307.47440 | 0.8262562 | 111.46358 | 0.5273934 | 0.0019593 |
| TidyDensity | 2 | LM | Test | 0.5583072 | 489.85690 | 1.1010315 | 118.34890 | 0.6819186 | 0.0028327 |
| TidyDensity | 3 | EARTH | Test | 0.4312278 | 317.92771 | 0.8504197 | 114.94469 | 0.5269950 | 0.0028327 |
| TidyDensity | 4 | NNAR | Test | 0.3637425 | 166.33513 | 0.7173326 | 129.40205 | 0.4421095 | 0.0141776 |
| tidyAML | 1 | ARIMA | Test | 0.8020473 | 102.07502 | 0.9404675 | 112.21655 | 1.0599674 | 0.0009121 |
| tidyAML | 2 | LM | Test | 0.8216157 | 123.69268 | 0.9634131 | 104.21401 | 1.0905751 | 0.1461729 |
| tidyAML | 3 | EARTH | Test | 0.8173477 | 92.76083 | 0.9584084 | 128.47853 | 1.0334431 | 0.1461729 |
| tidyAML | 4 | NNAR | Test | 0.8194318 | 116.39928 | 0.9608522 | 108.89567 | 1.0680548 | 0.0065377 |
| RandomWalker | 1 | ARIMA | Test | 1.3225836 | 201.57495 | 0.6947107 | 136.99286 | 1.6760845 | 0.0028020 |
| RandomWalker | 2 | LM | Test | 1.1530234 | 98.50258 | 0.6056461 | 189.55089 | 1.3695750 | 0.0134212 |
| RandomWalker | 3 | EARTH | Test | 1.1371500 | 104.38234 | 0.5973083 | 172.09858 | 1.3864958 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2035862 | 172.93729 | 0.6322051 | 153.66090 | 1.3901421 | 0.0173071 |

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
    ## 1 healthyR.da…         2 LM          Test  0.604 188.  0.661 113.  0.803 0.0142 
    ## 2 healthyR             3 EARTH       Test  0.562 100.  0.673 128.  0.728 0.0680 
    ## 3 healthyR.ts          3 EARTH       Test  0.577 195.  0.598 107.  0.772 0.0339 
    ## 4 healthyverse         2 LM          Test  0.502 294.  0.791  76.9 0.621 0.00179
    ## 5 healthyR.ai          2 LM          Test  0.670 106.  0.839 136.  0.968 0.273  
    ## 6 TidyDensity          4 NNAR        Test  0.364 166.  0.717 129.  0.442 0.0142 
    ## 7 tidyAML              3 EARTH       Test  0.817  92.8 0.958 128.  1.03  0.146  
    ## 8 RandomWalker         2 LM          Test  1.15   98.5 0.606 190.  1.37  0.0134

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1607|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1600|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1544|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1514|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1339|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1190|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [798|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [220|28]>  <mdl_tm_t [1 × 5]>

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
