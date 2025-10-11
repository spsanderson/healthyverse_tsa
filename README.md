# Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
Steven P. Sanderson II, MPH - Date:
2025-10-11

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

    Rows: 156,328
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

The last day in the data set is 2025-10-09 23:37:11, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
2.867565^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 156328        |
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
| r_version     |    114229 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    114229 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    114229 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     14715 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-10-09 | 2023-09-27 | 1775 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1124529.06 | 1495512.45 | 355 | 16873 | 308031 | 2365059 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11301.62 | 21880.19 | 1 | 198 | 2934 | 11961 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-10-09 23:37:11 | 2023-09-27 20:13:29 | 98137 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   42.5 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

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
    -147.66  -36.29  -10.63   27.01  814.63 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -2.166e+02  6.240e+01
    date                                                1.287e-02  3.306e-03
    lag(value, 1)                                       1.150e-01  2.348e-02
    lag(value, 7)                                       9.345e-02  2.434e-02
    lag(value, 14)                                      8.367e-02  2.438e-02
    lag(value, 21)                                      6.334e-02  2.444e-02
    lag(value, 28)                                      7.945e-02  2.446e-02
    lag(value, 35)                                      7.068e-02  2.454e-02
    lag(value, 42)                                      5.658e-02  2.468e-02
    lag(value, 49)                                      6.498e-02  2.459e-02
    month(date, label = TRUE).L                        -7.779e+00  5.111e+00
    month(date, label = TRUE).Q                         2.255e+00  5.015e+00
    month(date, label = TRUE).C                        -1.672e+01  5.048e+00
    month(date, label = TRUE)^4                        -8.731e+00  5.074e+00
    month(date, label = TRUE)^5                        -9.778e+00  5.034e+00
    month(date, label = TRUE)^6                        -1.022e+00  5.075e+00
    month(date, label = TRUE)^7                        -5.522e+00  4.979e+00
    month(date, label = TRUE)^8                        -3.958e+00  4.942e+00
    month(date, label = TRUE)^9                         3.194e+00  4.881e+00
    month(date, label = TRUE)^10                        1.253e+00  4.851e+00
    month(date, label = TRUE)^11                       -4.068e+00  4.830e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.127e+01  2.260e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  6.625e+00  2.349e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -3.472 0.000530 ***
    date                                                 3.894 0.000103 ***
    lag(value, 1)                                        4.899 1.05e-06 ***
    lag(value, 7)                                        3.839 0.000128 ***
    lag(value, 14)                                       3.433 0.000612 ***
    lag(value, 21)                                       2.592 0.009636 ** 
    lag(value, 28)                                       3.248 0.001185 ** 
    lag(value, 35)                                       2.880 0.004029 ** 
    lag(value, 42)                                       2.293 0.021984 *  
    lag(value, 49)                                       2.643 0.008300 ** 
    month(date, label = TRUE).L                         -1.522 0.128190    
    month(date, label = TRUE).Q                          0.450 0.653055    
    month(date, label = TRUE).C                         -3.312 0.000944 ***
    month(date, label = TRUE)^4                         -1.721 0.085482 .  
    month(date, label = TRUE)^5                         -1.942 0.052266 .  
    month(date, label = TRUE)^6                         -0.201 0.840352    
    month(date, label = TRUE)^7                         -1.109 0.267593    
    month(date, label = TRUE)^8                         -0.801 0.423284    
    month(date, label = TRUE)^9                          0.654 0.512988    
    month(date, label = TRUE)^10                         0.258 0.796241    
    month(date, label = TRUE)^11                        -0.842 0.399783    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -4.986 6.79e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   2.820 0.004857 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 59.19 on 1703 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.2405,    Adjusted R-squared:  0.2307 
    F-statistic: 24.52 on 22 and 1703 DF,  p-value: < 2.2e-16

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
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 185.17345218637"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 185.17345218637"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 18.2231372989492"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 18.2231372989492"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 32.603262309868"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 32.603262309868"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 55.7968331237009"
    [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 55.7968331237009"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 31.110379677055"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 31.110379677055"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 44.6924119475142"
    [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    [1] "BEST both OBJECTIVE FUNCTION = 44.6924119475142"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 8.62218653334933"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 8.62218653334933"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.64258107321674"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 9.64258107321674"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.74912705452325"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.74912705452325"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 18.5835808921461"
    [1] "BEST method = 'lin' PATH MEMBER = c( 4 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 18.5835808921461"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.00178176879383"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 4 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.00178176879383"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.36746661612806"
    [1] "BEST method = 'both' PATH MEMBER = c( 4 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.36746661612806"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 17.8824085766292"
    [1] "BEST method = 'lin' PATH MEMBER = c( 3 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 17.8824085766292"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.8446992330909"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 3 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 15.8446992330909"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 13.351167414668"
    [1] "BEST method = 'both' PATH MEMBER = c( 3 )"
    [1] "BEST both OBJECTIVE FUNCTION = 13.351167414668"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 25 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 3.08041565952606"
    [1] "BEST method = 'lin' PATH MEMBER = c( 25 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 3.08041565952606"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 25 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.7240576551704"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 25 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.7240576551704"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 25 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 3.80788690189099"
    [1] "BEST method = 'both' PATH MEMBER = c( 25 )"
    [1] "BEST both OBJECTIVE FUNCTION = 3.80788690189099"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 23.6961761863263"
    [1] "BEST method = 'lin' PATH MEMBER = c( 18 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 23.6961761863263"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.2619231391755"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 18 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.2619231391755"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 21.6272089635927"
    [1] "BEST method = 'both' PATH MEMBER = c( 18 )"
    [1] "BEST both OBJECTIVE FUNCTION = 21.6272089635927"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 40.7943200744149"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 40.7943200744149"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.45262657930145"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.45262657930145"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 12.3602283574031"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 12.3602283574031"

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
    1 healthyR.data <tibble [1,767 × 50]> <tibble [28 × 50]> <split [1739|28]>
    2 healthyR      <tibble [1,758 × 50]> <tibble [28 × 50]> <split [1730|28]>
    3 healthyR.ts   <tibble [1,704 × 50]> <tibble [28 × 50]> <split [1676|28]>
    4 healthyverse  <tibble [1,675 × 50]> <tibble [28 × 50]> <split [1647|28]>
    5 healthyR.ai   <tibble [1,500 × 50]> <tibble [28 × 50]> <split [1472|28]>
    6 TidyDensity   <tibble [1,351 × 50]> <tibble [28 × 50]> <split [1323|28]>
    7 tidyAML       <tibble [958 × 50]>   <tibble [28 × 50]> <split [930|28]> 
    8 RandomWalker  <tibble [381 × 50]>   <tibble [28 × 50]> <split [353|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 0.6384143 | 124.0511 | 0.9032452 | 161.93084 | 0.7626951 | 0.0000061 |
| healthyR.data | 2 | LM | Test | 0.8469077 | 211.3391 | 1.1982272 | 161.79876 | 1.0057613 | 0.0062821 |
| healthyR.data | 3 | EARTH | Test | 0.7385903 | 176.7985 | 1.0449768 | 166.43035 | 0.8468415 | 0.0006796 |
| healthyR.data | 4 | NNAR | Test | 0.8495616 | 221.5492 | 1.2019820 | 171.13059 | 1.0002739 | 0.0000060 |
| healthyR | 1 | ARIMA | Test | 0.5723793 | 225.5472 | 0.9488879 | 174.48968 | 0.6969747 | 0.0038168 |
| healthyR | 2 | LM | Test | 0.7703444 | 382.6098 | 1.2770734 | 164.47988 | 0.9577758 | 0.0157548 |
| healthyR | 3 | EARTH | Test | 0.4698236 | 377.2019 | 0.7788714 | 124.72264 | 0.5514670 | 0.0387621 |
| healthyR | 4 | NNAR | Test | 0.5434428 | 261.9101 | 0.9009170 | 146.89793 | 0.7123869 | 0.0018690 |
| healthyR.ts | 1 | ARIMA | Test | 0.4932989 | 168.6745 | 0.7410656 | 151.72235 | 0.6057393 | 0.0002606 |
| healthyR.ts | 2 | LM | Test | 0.6235677 | 201.0596 | 0.9367638 | 134.14063 | 0.7619916 | 0.0051951 |
| healthyR.ts | 3 | EARTH | Test | 0.4521733 | 116.4122 | 0.6792840 | 136.78146 | 0.5591752 | 0.0040492 |
| healthyR.ts | 4 | NNAR | Test | 0.7238825 | 251.9322 | 1.0874631 | 158.84226 | 0.8210582 | 0.0048804 |
| healthyverse | 1 | ARIMA | Test | 0.7901782 | 147.6274 | 0.8983681 | 153.65014 | 0.9558061 | 0.0054980 |
| healthyverse | 2 | LM | Test | 0.7292697 | 164.3030 | 0.8291201 | 141.01490 | 0.9063732 | 0.0953385 |
| healthyverse | 3 | EARTH | Test | 0.9744072 | 222.2915 | 1.1078214 | 158.75122 | 1.1287034 | 0.0046341 |
| healthyverse | 4 | NNAR | Test | 0.6169388 | 138.2979 | 0.7014090 | 138.62039 | 0.7838745 | 0.0430484 |
| healthyR.ai | 1 | ARIMA | Test | 0.9354023 | 137.5346 | 1.1183134 | 169.65540 | 1.4630212 | 0.0307715 |
| healthyR.ai | 2 | LM | Test | 0.8999550 | 161.6838 | 1.0759346 | 148.62372 | 1.4868769 | 0.0028569 |
| healthyR.ai | 3 | EARTH | Test | 0.9793937 | 161.5756 | 1.1709070 | 163.58923 | 1.4971111 | 0.1155972 |
| healthyR.ai | 4 | NNAR | Test | 0.8874226 | 174.6020 | 1.0609516 | 140.56072 | 1.4331660 | 0.0477083 |
| TidyDensity | 1 | ARIMA | Test | 1.6128238 | 139.9348 | 1.2044610 | 144.57414 | 1.9374686 | 0.0444415 |
| TidyDensity | 2 | LM | Test | 1.9014890 | 140.6712 | 1.4200369 | 168.95896 | 2.3115796 | 0.0005990 |
| TidyDensity | 3 | EARTH | Test | 2.4881627 | 707.0307 | 1.8581664 | 95.86769 | 2.8067643 | 0.0240975 |
| TidyDensity | 4 | NNAR | Test | 1.6755896 | 113.7258 | 1.2513347 | 147.67601 | 2.0187067 | 0.0025456 |
| tidyAML | 1 | ARIMA | Test | 0.9329421 | 119.6504 | 0.8255857 | 161.16394 | 1.3747530 | 0.0158001 |
| tidyAML | 2 | LM | Test | 1.1034781 | 219.2716 | 0.9764976 | 164.59919 | 1.5542257 | 0.0095382 |
| tidyAML | 3 | EARTH | Test | 0.8725423 | 112.3599 | 0.7721363 | 170.27172 | 1.2753245 | 0.1405838 |
| tidyAML | 4 | NNAR | Test | 1.1038200 | 222.9033 | 0.9768002 | 159.97797 | 1.5209412 | 0.0002930 |
| RandomWalker | 1 | ARIMA | Test | 0.8308502 | 538.7320 | 0.6227150 | 152.67660 | 0.9271907 | 0.0534882 |
| RandomWalker | 2 | LM | Test | 0.7835042 | 837.7362 | 0.5872296 | 143.91730 | 0.9287245 | 0.0032850 |
| RandomWalker | 3 | EARTH | Test | 0.7528107 | 119.4292 | 0.5642251 | 181.07526 | 0.9785371 | NA |
| RandomWalker | 4 | NNAR | Test | 0.7767675 | 744.3618 | 0.5821805 | 145.13532 | 0.9582777 | 0.0098002 |

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
      package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
      <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.da…         1 ARIMA       Test  0.638  124. 0.903  162. 0.763 6.12e-6
    2 healthyR             3 EARTH       Test  0.470  377. 0.779  125. 0.551 3.88e-2
    3 healthyR.ts          3 EARTH       Test  0.452  116. 0.679  137. 0.559 4.05e-3
    4 healthyverse         4 NNAR        Test  0.617  138. 0.701  139. 0.784 4.30e-2
    5 healthyR.ai          4 NNAR        Test  0.887  175. 1.06   141. 1.43  4.77e-2
    6 TidyDensity          1 ARIMA       Test  1.61   140. 1.20   145. 1.94  4.44e-2
    7 tidyAML              3 EARTH       Test  0.873  112. 0.772  170. 1.28  1.41e-1
    8 RandomWalker         1 ARIMA       Test  0.831  539. 0.623  153. 0.927 5.35e-2

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
    1 healthyR.data <tibble>     <tibble>     <split [1739|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [1730|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1676|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1647|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1472|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1323|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [930|28]>  <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [353|28]>  <mdl_tm_t [1 × 5]>

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
