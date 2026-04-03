# Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
Steven P. Sanderson II, MPH - Date:
2026-04-03

# Introduction

This analysis follows a *Nested Modeltime Workflow* from **`modeltime`**
along with using the **`NNS`** package. I use this to monitor the
downloads of all of my packages:

- [`healthyR`](https://www.spsanderson.com/healthyR/)
- [`healthyR.data`](https://www.spsanderson.com/healthyR.data/)
- [`healthyR.ts`](https://www.spsanderson.com/healthyR.ts/)
- [`healthyR.ai`](https://www.spsanderson.com/healthyR.ai/)
- [`healthyverse`](https://www.spsanderson.com/healthyverse/)
- [`TidyDensity`](https://www.spsanderson.com/TidyDensity/)
- [`tidyAML`](https://www.spsanderson.com/tidyAML/)
- [`RandomWalker`](https://www.spsanderson.com/RandomWalker/)

## Get Data

``` r
glimpse(downloads_tbl)
```

    Rows: 174,042
    Columns: 11
    $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2026-04-01 23:24:50, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
3656.61 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 174042        |
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
| r_version     |    129196 |          0.26 |   5 |   7 |     0 |       51 |          0 |
| r_arch        |    129196 |          0.26 |   1 |   7 |     0 |        6 |          0 |
| r_os          |    129196 |          0.26 |   7 |  19 |     0 |       24 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     16161 |          0.91 |   2 |   2 |     0 |      167 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-04-01 | 2024-01-10 | 1949 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1126813.35 | 1478444.58 | 355 | 43530 | 325156 | 2333727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11455.81 | 22866.32 | 1 | 199 | 2741 | 11721 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-04-01 23:24:50 | 2024-01-10 05:09:02 | 110684 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     17 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

    [[1]]

![](man/figures/README-initial_ts_plot-3.png)


    [[2]]

![](man/figures/README-initial_ts_plot-4.png)


    [[3]]

![](man/figures/README-initial_ts_plot-5.png)


    [[4]]

![](man/figures/README-initial_ts_plot-6.png)


    [[5]]

![](man/figures/README-initial_ts_plot-7.png)


    [[6]]

![](man/figures/README-initial_ts_plot-8.png)


    [[7]]

![](man/figures/README-initial_ts_plot-9.png)


    [[8]]

![](man/figures/README-initial_ts_plot-10.png)

Now lets take a look at some time series decomposition graphs.

    [[1]]

![](man/figures/README-ts_decomp_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-8.png)

    [[1]]

![](man/figures/README-ts_decomp_plt-9.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-10.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-11.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-12.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-13.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-14.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-15.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-16.png)

Seasonal Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_seasonal_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_seasonal_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_seasonal_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_seasonal_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_seasonal_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_seasonal_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_seasonal_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_seasonal_plt-8.png)

ACF and PACF Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_acf_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_acf_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_acf_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_acf_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_acf_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_acf_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_acf_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_acf_plt-8.png)

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.


    Call:
    stats::lm(formula = .formula, data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -150.38  -37.45  -11.57   27.69  826.76 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.749e+02  5.448e+01
    date                                                1.087e-02  2.882e-03
    lag(value, 1)                                       9.354e-02  2.263e-02
    lag(value, 7)                                       7.634e-02  2.348e-02
    lag(value, 14)                                      6.414e-02  2.336e-02
    lag(value, 21)                                      9.138e-02  2.344e-02
    lag(value, 28)                                      7.594e-02  2.334e-02
    lag(value, 35)                                      4.589e-02  2.340e-02
    lag(value, 42)                                      6.389e-02  2.354e-02
    lag(value, 49)                                      7.568e-02  2.345e-02
    month(date, label = TRUE).L                        -9.300e+00  4.756e+00
    month(date, label = TRUE).Q                        -1.020e+00  4.776e+00
    month(date, label = TRUE).C                        -1.463e+01  4.790e+00
    month(date, label = TRUE)^4                        -8.247e+00  4.788e+00
    month(date, label = TRUE)^5                        -5.221e+00  4.793e+00
    month(date, label = TRUE)^6                        -3.400e-01  4.832e+00
    month(date, label = TRUE)^7                        -3.574e+00  4.757e+00
    month(date, label = TRUE)^8                        -5.005e+00  4.757e+00
    month(date, label = TRUE)^9                         3.230e+00  4.822e+00
    month(date, label = TRUE)^10                        1.046e+00  4.881e+00
    month(date, label = TRUE)^11                       -4.344e+00  4.883e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.133e+01  2.152e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  7.386e+00  2.225e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -3.210 0.001348 ** 
    date                                                 3.771 0.000168 ***
    lag(value, 1)                                        4.134 3.73e-05 ***
    lag(value, 7)                                        3.251 0.001169 ** 
    lag(value, 14)                                       2.746 0.006095 ** 
    lag(value, 21)                                       3.898 0.000100 ***
    lag(value, 28)                                       3.253 0.001161 ** 
    lag(value, 35)                                       1.960 0.050093 .  
    lag(value, 42)                                       2.714 0.006700 ** 
    lag(value, 49)                                       3.228 0.001270 ** 
    month(date, label = TRUE).L                         -1.955 0.050705 .  
    month(date, label = TRUE).Q                         -0.214 0.830848    
    month(date, label = TRUE).C                         -3.054 0.002290 ** 
    month(date, label = TRUE)^4                         -1.723 0.085119 .  
    month(date, label = TRUE)^5                         -1.089 0.276212    
    month(date, label = TRUE)^6                         -0.070 0.943912    
    month(date, label = TRUE)^7                         -0.751 0.452486    
    month(date, label = TRUE)^8                         -1.052 0.292868    
    month(date, label = TRUE)^9                          0.670 0.502971    
    month(date, label = TRUE)^10                         0.214 0.830286    
    month(date, label = TRUE)^11                        -0.890 0.373759    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.266 1.55e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.319 0.000921 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 59.94 on 1877 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.215, Adjusted R-squared:  0.2058 
    F-statistic: 23.37 on 22 and 1877 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)

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
#            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            seas <- t(
                sapply(
                    1:25, 
                    function(i) c(
                        i,
                        sqrt(
                            mean((
                                NNS.ARMA(x, 
                                         h = 28, 
                                         training.set = train_set_size, 
                                         method = "lin", 
                                         seasonal.factor = i, 
                                         plot=FALSE
                                         ) - tail(x, 28)) ^ 2)))
                    )
                )
            colnames(seas) <- c("Period", "RMSE")
            sf <- seas[which.min(seas[, 2]), 1]
            
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

    Package: healthyR
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 49.4161511080069"
    [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 49.4161511080069"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.06837966657089"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 8.06837966657089"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 12.0257366658761"
    [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    [1] "BEST both OBJECTIVE FUNCTION = 12.0257366658761"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 10.9495849147152"
    [1] "BEST method = 'lin' PATH MEMBER = c( 4 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 10.9495849147152"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 19.5399310985138"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 4 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 19.5399310985138"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 29.9278931568024"
    [1] "BEST method = 'both' PATH MEMBER = c( 4 )"
    [1] "BEST both OBJECTIVE FUNCTION = 29.9278931568024"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 11.3366668224515"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 11.3366668224515"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.6153402278014"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 12.6153402278014"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.1585031460308"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.1585031460308"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 22.9338928771366"
    [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 22.9338928771366"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.3894163987613"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 15.3894163987613"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 13.3834672437266"
    [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    [1] "BEST both OBJECTIVE FUNCTION = 13.3834672437266"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 72.5270370117118"
    [1] "BEST method = 'lin' PATH MEMBER = c( 3 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 72.5270370117118"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 40.715170547556"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 3 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 40.715170547556"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 30.1735408390841"
    [1] "BEST method = 'both' PATH MEMBER = c( 3 )"
    [1] "BEST both OBJECTIVE FUNCTION = 30.1735408390841"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 6.49616822458263"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 6.49616822458263"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.3596880201861"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.3596880201861"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.53157588661384"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.53157588661384"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 10.8828299247299"
    [1] "BEST method = 'lin' PATH MEMBER = c( 14 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 10.8828299247299"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 21.7415509624376"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 21.7415509624376"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 17.0455268231883"
    [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    [1] "BEST both OBJECTIVE FUNCTION = 17.0455268231883"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 12 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 27.30996456205"
    [1] "BEST method = 'lin' PATH MEMBER = c( 12 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 27.30996456205"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 12 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.19615487231713"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 12 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.19615487231713"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 12 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.63176462278477"
    [1] "BEST method = 'both' PATH MEMBER = c( 12 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.63176462278477"

![](man/figures/README-nns_forecasting-8.png)

    [[1]]
    NULL

    [[2]]
    NULL

    [[3]]
    NULL

    [[4]]
    NULL

    [[5]]
    NULL

    [[6]]
    NULL

    [[7]]
    NULL

    [[8]]
    NULL

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
  group_by(package) %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  tk_augment_fourier(
    .date_var = date,
    .periods  = c(7, 14, 30, 90, 180),
    .K        = 2
  ) %>%
  tk_augment_timeseries_signature(
    .date_var = date
  ) %>%
  ungroup() %>%
  select(-c(value, -year.iso))
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

    # 0. Filter out column where package is NA
    filter(!is.na(package)) %>%
    
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

    # A tibble: 8 × 4
      package       .actual_data          .future_data       .splits          
      <fct>         <list>                <list>             <list>           
    1 healthyR.data <tibble [1,938 × 50]> <tibble [28 × 50]> <split [1910|28]>
    2 healthyR      <tibble [1,932 × 50]> <tibble [28 × 50]> <split [1904|28]>
    3 healthyR.ts   <tibble [1,868 × 50]> <tibble [28 × 50]> <split [1840|28]>
    4 healthyverse  <tibble [1,814 × 50]> <tibble [28 × 50]> <split [1786|28]>
    5 healthyR.ai   <tibble [1,674 × 50]> <tibble [28 × 50]> <split [1646|28]>
    6 TidyDensity   <tibble [1,525 × 50]> <tibble [28 × 50]> <split [1497|28]>
    7 tidyAML       <tibble [1,131 × 50]> <tibble [28 × 50]> <split [1103|28]>
    8 RandomWalker  <tibble [555 × 50]>   <tibble [28 × 50]> <split [527|28]> 

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
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
  add_recipe(recipe = recipe_date) %>%
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
| healthyR.data | 1 | ARIMA | Test | 0.8242392 | 121.30399 | 0.7044495 | 174.33072 | 0.9988770 | 0.0032335 |
| healthyR.data | 2 | LM | Test | 0.7825210 | 148.37654 | 0.6687944 | 140.18645 | 1.0175742 | 0.0003613 |
| healthyR.data | 3 | EARTH | Test | 0.8310477 | 175.73553 | 0.7102685 | 132.35598 | 1.0863959 | 0.1921224 |
| healthyR.data | 4 | NNAR | Test | 0.7991947 | 179.82609 | 0.6830448 | 140.17863 | 1.0204628 | 0.0026202 |
| healthyR | 1 | ARIMA | Test | 0.8302436 | 1387.80159 | 0.7973388 | 135.36780 | 1.1398173 | 0.0061143 |
| healthyR | 2 | LM | Test | 0.8915471 | 536.45841 | 0.8562126 | 152.07271 | 1.1834100 | 0.0561822 |
| healthyR | 3 | EARTH | Test | 0.7960861 | 2243.63534 | 0.7645350 | 122.35384 | 1.1093113 | 0.1603213 |
| healthyR | 4 | NNAR | Test | 0.8396737 | 948.22791 | 0.8063951 | 139.17705 | 1.1826287 | 0.0068016 |
| healthyR.ts | 1 | ARIMA | Test | 0.5017220 | 284.13160 | 0.5420679 | 126.06488 | 0.7363862 | 0.1781458 |
| healthyR.ts | 2 | LM | Test | 0.6534916 | 422.93963 | 0.7060420 | 149.62237 | 0.8375889 | 0.0196666 |
| healthyR.ts | 3 | EARTH | Test | 0.7039487 | 338.47427 | 0.7605566 | 145.21175 | 0.8974595 | 0.0757851 |
| healthyR.ts | 4 | NNAR | Test | 0.6798702 | 366.93462 | 0.7345418 | 146.74808 | 0.8631650 | 0.0111622 |
| healthyverse | 1 | ARIMA | Test | 0.7509300 | 156.73037 | 0.7956564 | 69.22935 | 0.9180230 | 0.1020942 |
| healthyverse | 2 | LM | Test | 1.2788147 | 269.53868 | 1.3549827 | 158.50920 | 1.4206565 | 0.0000088 |
| healthyverse | 3 | EARTH | Test | 0.8323226 | 364.91823 | 0.8818969 | 61.94034 | 1.0362736 | 0.0360843 |
| healthyverse | 4 | NNAR | Test | 1.0951601 | 300.17317 | 1.1603893 | 136.17314 | 1.2417999 | 0.0038155 |
| healthyR.ai | 1 | ARIMA | Test | 0.4838008 | 191.85899 | 0.6408506 | 103.28068 | 0.7780767 | 0.0591838 |
| healthyR.ai | 2 | LM | Test | 0.5139797 | 163.51725 | 0.6808262 | 122.34592 | 0.7644093 | 0.0651694 |
| healthyR.ai | 3 | EARTH | Test | 0.5466512 | 116.88490 | 0.7241033 | 163.61297 | 0.7510209 | 0.0898351 |
| healthyR.ai | 4 | NNAR | Test | 0.4579734 | 148.30895 | 0.6066393 | 99.20578 | 0.7359135 | 0.0822008 |
| TidyDensity | 1 | ARIMA | Test | 1.2365426 | 146.77995 | 0.6910491 | 149.66380 | 1.3205353 | 0.0099386 |
| TidyDensity | 2 | LM | Test | 1.2155897 | 175.45443 | 0.6793394 | 149.43935 | 1.3075702 | 0.0349318 |
| TidyDensity | 3 | EARTH | Test | 1.2571415 | 169.35012 | 0.7025609 | 141.87097 | 1.3471470 | 0.0281517 |
| TidyDensity | 4 | NNAR | Test | 1.1120456 | 130.96852 | 0.6214732 | 149.50056 | 1.2537058 | 0.0641856 |
| tidyAML | 1 | ARIMA | Test | 0.8386531 | 150.01323 | 0.6811630 | 153.63869 | 1.1068609 | 0.0003378 |
| tidyAML | 2 | LM | Test | 0.8503077 | 247.42513 | 0.6906291 | 156.54244 | 1.0451118 | 0.1321412 |
| tidyAML | 3 | EARTH | Test | 0.9744772 | 241.96285 | 0.7914808 | 148.57084 | 1.2318668 | 0.0986567 |
| tidyAML | 4 | NNAR | Test | 0.8227772 | 237.27805 | 0.6682685 | 137.42697 | 1.0121226 | 0.2015502 |
| RandomWalker | 1 | ARIMA | Test | 0.8423489 | 87.00537 | 0.5085867 | 140.02538 | 0.9685627 | 0.5038078 |
| RandomWalker | 2 | LM | Test | 1.0508404 | 103.68502 | 0.6344681 | 146.24341 | 1.2774097 | 0.0099320 |
| RandomWalker | 3 | EARTH | Test | 1.0189601 | 95.21790 | 0.6152196 | 173.34315 | 1.1879625 | 0.0103766 |
| RandomWalker | 4 | NNAR | Test | 1.0324474 | 113.23750 | 0.6233629 | 142.13449 | 1.2265583 | 0.0040233 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  facet_wrap(~ package, nrow = 3) +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 10
      package     .model_id .model_desc .type   mae   mape  mase smape  rmse     rsq
      <fct>           <int> <chr>       <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.d…         1 ARIMA       Test  0.824  121.  0.704 174.  0.999 0.00323
    2 healthyR            3 EARTH       Test  0.796 2244.  0.765 122.  1.11  0.160  
    3 healthyR.ts         1 ARIMA       Test  0.502  284.  0.542 126.  0.736 0.178  
    4 healthyver…         1 ARIMA       Test  0.751  157.  0.796  69.2 0.918 0.102  
    5 healthyR.ai         4 NNAR        Test  0.458  148.  0.607  99.2 0.736 0.0822 
    6 TidyDensity         4 NNAR        Test  1.11   131.  0.621 150.  1.25  0.0642 
    7 tidyAML             4 NNAR        Test  0.823  237.  0.668 137.  1.01  0.202  
    8 RandomWalk…         1 ARIMA       Test  0.842   87.0 0.509 140.  0.969 0.504  

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 5
      package       .actual_data .future_data .splits           .modeltime_tables 
      <fct>         <list>       <list>       <list>            <list>            
    1 healthyR.data <tibble>     <tibble>     <split [1910|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [1904|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1840|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1786|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1646|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1497|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1103|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [527|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(package) %>%
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
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)
