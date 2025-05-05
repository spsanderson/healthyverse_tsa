Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
05 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,860
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

The last day in the data set is 2025-05-03 23:42:34, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6468.11
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138860        |
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
| r_version     |    100032 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    100032 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    100032 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11777 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-03 | 2023-06-14 | 1623 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134125.22 | 1520146.56 | 355 | 14701.00 | 278429 | 2367750.25 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10387.59 | 18460.51 | 1 | 291.75 | 3058 | 11669.25 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-03 23:42:34 | 2023-06-14 19:40:33 | 84699 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     24 |       60 |

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
    ## -146.92  -35.89  -11.02   26.52  814.63 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.798e+02  7.121e+01
    ## date                                                1.101e-02  3.776e-03
    ## lag(value, 1)                                       1.053e-01  2.462e-02
    ## lag(value, 7)                                       9.652e-02  2.553e-02
    ## lag(value, 14)                                      9.240e-02  2.555e-02
    ## lag(value, 21)                                      6.804e-02  2.564e-02
    ## lag(value, 28)                                      6.725e-02  2.560e-02
    ## lag(value, 35)                                      6.710e-02  2.566e-02
    ## lag(value, 42)                                      4.912e-02  2.583e-02
    ## lag(value, 49)                                      6.737e-02  2.565e-02
    ## month(date, label = TRUE).L                        -9.980e+00  5.130e+00
    ## month(date, label = TRUE).Q                         3.093e+00  5.189e+00
    ## month(date, label = TRUE).C                        -1.267e+01  5.179e+00
    ## month(date, label = TRUE)^4                        -6.706e+00  5.199e+00
    ## month(date, label = TRUE)^5                        -1.192e+01  5.189e+00
    ## month(date, label = TRUE)^6                        -3.247e+00  5.239e+00
    ## month(date, label = TRUE)^7                        -6.567e+00  5.163e+00
    ## month(date, label = TRUE)^8                        -4.111e+00  5.160e+00
    ## month(date, label = TRUE)^9                         5.388e+00  5.149e+00
    ## month(date, label = TRUE)^10                        4.207e+00  5.221e+00
    ## month(date, label = TRUE)^11                       -5.566e+00  5.324e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.159e+01  2.369e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.109e+00  2.491e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.524 0.011691 *  
    ## date                                                 2.915 0.003606 ** 
    ## lag(value, 1)                                        4.278 2.00e-05 ***
    ## lag(value, 7)                                        3.780 0.000162 ***
    ## lag(value, 14)                                       3.616 0.000308 ***
    ## lag(value, 21)                                       2.654 0.008030 ** 
    ## lag(value, 28)                                       2.627 0.008700 ** 
    ## lag(value, 35)                                       2.615 0.009010 ** 
    ## lag(value, 42)                                       1.901 0.057439 .  
    ## lag(value, 49)                                       2.627 0.008706 ** 
    ## month(date, label = TRUE).L                         -1.945 0.051917 .  
    ## month(date, label = TRUE).Q                          0.596 0.551296    
    ## month(date, label = TRUE).C                         -2.446 0.014564 *  
    ## month(date, label = TRUE)^4                         -1.290 0.197320    
    ## month(date, label = TRUE)^5                         -2.297 0.021767 *  
    ## month(date, label = TRUE)^6                         -0.620 0.535520    
    ## month(date, label = TRUE)^7                         -1.272 0.203629    
    ## month(date, label = TRUE)^8                         -0.797 0.425726    
    ## month(date, label = TRUE)^9                          1.047 0.295488    
    ## month(date, label = TRUE)^10                         0.806 0.420398    
    ## month(date, label = TRUE)^11                        -1.045 0.296009    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.894 1.09e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.255 0.001159 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.83 on 1551 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2431, Adjusted R-squared:  0.2324 
    ## F-statistic: 22.65 on 22 and 1551 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.11293366570217"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.03409502487184"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.90545693078651"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.90545693078651"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.16573839327996"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.16573839327996"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.85770998528438"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.85770998528438"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.92281440657544"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.83971829642949"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.83971829642949"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.78392875908897"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.78392875908897"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.73357586278602"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.73357586278602"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.10635254365663"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.42451453365841"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.42451453365841"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.96984714135538"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.96984714135538"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.45765387964315"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.45765387964315"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.75955141256856"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.20870290018315"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.16219682690574"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.16219682690574"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.31198128609253"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.31198128609253"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.03865772804602"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.03865772804602"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.90853554909379"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.90853554909379"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.5423466447271"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.5423466447271"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 10.1948734250733"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 10.1948734250733"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.51663060556804"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.46942623739311"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.46942623739311"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.40547397686303"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.40547397686303"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.15917455383451"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.15917455383451"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.40055681626712"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.40055681626712"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.67762589069022"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.67762589069022"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.26657384801271"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.26657384801271"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.80927396795897"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.19082741744284"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.13970798366402"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.13970798366402"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.20616420616038"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.20616420616038"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.60200626019791"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.60200626019791"

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
    ## 1 healthyR.data <tibble [1,616 × 2]> <tibble [28 × 2]> <split [1588|28]>
    ## 2 healthyR      <tibble [1,609 × 2]> <tibble [28 × 2]> <split [1581|28]>
    ## 3 healthyR.ts   <tibble [1,553 × 2]> <tibble [28 × 2]> <split [1525|28]>
    ## 4 healthyverse  <tibble [1,523 × 2]> <tibble [28 × 2]> <split [1495|28]>
    ## 5 healthyR.ai   <tibble [1,348 × 2]> <tibble [28 × 2]> <split [1320|28]>
    ## 6 TidyDensity   <tibble [1,199 × 2]> <tibble [28 × 2]> <split [1171|28]>
    ## 7 tidyAML       <tibble [807 × 2]>   <tibble [28 × 2]> <split [779|28]> 
    ## 8 RandomWalker  <tibble [229 × 2]>   <tibble [28 × 2]> <split [201|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6739113 | 129.62677 | 0.6018037 | 132.16752 | 0.8375416 | 0.0057074 |
| healthyR.data | 2 | LM | Test | 0.6888937 | 163.50063 | 0.6151830 | 123.02192 | 0.8351840 | 0.0826482 |
| healthyR.data | 3 | EARTH | Test | 0.7916618 | 291.90952 | 0.7069550 | 115.10520 | 0.9375983 | 0.0826482 |
| healthyR.data | 4 | NNAR | Test | 0.7596980 | 102.39578 | 0.6784114 | 176.65997 | 0.9222922 | 0.0688762 |
| healthyR | 1 | ARIMA | Test | 0.6605060 | 97.60195 | 0.7090504 | 176.51520 | 0.8130629 | 0.0040986 |
| healthyR | 2 | LM | Test | 0.6557971 | 96.18973 | 0.7039954 | 176.26478 | 0.7983449 | 0.1134893 |
| healthyR | 3 | EARTH | Test | 0.6415322 | 99.50520 | 0.6886821 | 148.92348 | 0.7743619 | 0.1134893 |
| healthyR | 4 | NNAR | Test | 0.6589689 | 98.72745 | 0.7074002 | 156.72714 | 0.7899593 | 0.0050712 |
| healthyR.ts | 1 | ARIMA | Test | 0.8080386 | 105.58654 | 0.6953326 | 172.48277 | 0.9768295 | 0.0686469 |
| healthyR.ts | 2 | LM | Test | 0.8956724 | 179.18918 | 0.7707432 | 139.06844 | 1.1256140 | 0.0686469 |
| healthyR.ts | 3 | EARTH | Test | 0.8812748 | 233.53917 | 0.7583537 | 124.35032 | 1.0623949 | 0.0686469 |
| healthyR.ts | 4 | NNAR | Test | 0.8165453 | 129.45754 | 0.7026528 | 182.90188 | 0.9733728 | 0.0001254 |
| healthyverse | 1 | ARIMA | Test | 0.6188194 | 328.50774 | 0.8036916 | 100.36498 | 0.7473370 | 0.0387540 |
| healthyverse | 2 | LM | Test | 0.6353111 | 369.02399 | 0.8251102 | 99.34183 | 0.7672780 | 0.0186887 |
| healthyverse | 3 | EARTH | Test | 0.7900427 | 597.53258 | 1.0260677 | 100.91302 | 0.9346736 | 0.0186887 |
| healthyverse | 4 | NNAR | Test | 0.6005806 | 219.52514 | 0.7800040 | 107.65755 | 0.7462464 | 0.1024880 |
| healthyR.ai | 1 | ARIMA | Test | 0.8021379 | 131.07170 | 0.8359011 | 167.01252 | 1.0406554 | 0.0130439 |
| healthyR.ai | 2 | LM | Test | 0.7836658 | 126.51400 | 0.8166515 | 161.23979 | 1.0430920 | 0.0444298 |
| healthyR.ai | 3 | EARTH | Test | 1.3137051 | 403.94153 | 1.3690010 | 144.70906 | 1.6173932 | 0.0444298 |
| healthyR.ai | 4 | NNAR | Test | 0.7944850 | 140.39990 | 0.8279260 | 157.93265 | 1.0486948 | 0.0450521 |
| TidyDensity | 1 | ARIMA | Test | 0.5571647 | 347.20609 | 0.8385964 | 118.41192 | 0.6793840 | 0.0148884 |
| TidyDensity | 2 | LM | Test | 0.6250080 | 468.51795 | 0.9407083 | 113.58838 | 0.7624616 | 0.0837757 |
| TidyDensity | 3 | EARTH | Test | 0.5542846 | 301.66143 | 0.8342616 | 118.47541 | 0.6821697 | 0.0837757 |
| TidyDensity | 4 | NNAR | Test | 0.5292558 | 187.41623 | 0.7965903 | 134.57087 | 0.6787340 | 0.0239404 |
| tidyAML | 1 | ARIMA | Test | 0.6837896 | 242.00114 | 0.9694986 | 102.84342 | 0.8131370 | 0.0003623 |
| tidyAML | 2 | LM | Test | 0.6739383 | 258.40311 | 0.9555310 | 101.05544 | 0.8059117 | 0.1527018 |
| tidyAML | 3 | EARTH | Test | 0.6635451 | 182.33098 | 0.9407952 | 111.91260 | 0.8151957 | 0.1527018 |
| tidyAML | 4 | NNAR | Test | 0.6900732 | 264.15910 | 0.9784076 | 103.49839 | 0.8173737 | 0.0143995 |
| RandomWalker | 1 | ARIMA | Test | 1.3884918 | 132.18201 | 0.6379231 | 156.38626 | 1.7317549 | 0.2753778 |
| RandomWalker | 2 | LM | Test | 1.2270207 | 111.56729 | 0.5637375 | 172.66433 | 1.4526517 | 0.0147534 |
| RandomWalker | 3 | EARTH | Test | 1.2257967 | 112.26795 | 0.5631751 | 171.10583 | 1.4534563 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1900875 | 144.73295 | 0.5467690 | 155.09599 | 1.3934742 | 0.0777249 |

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
    ## 1 healthyR.da…         2 LM          Test  0.689 164.  0.615  123. 0.835 8.26e-2
    ## 2 healthyR             3 EARTH       Test  0.642  99.5 0.689  149. 0.774 1.13e-1
    ## 3 healthyR.ts          4 NNAR        Test  0.817 129.  0.703  183. 0.973 1.25e-4
    ## 4 healthyverse         4 NNAR        Test  0.601 220.  0.780  108. 0.746 1.02e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.802 131.  0.836  167. 1.04  1.30e-2
    ## 6 TidyDensity          4 NNAR        Test  0.529 187.  0.797  135. 0.679 2.39e-2
    ## 7 tidyAML              2 LM          Test  0.674 258.  0.956  101. 0.806 1.53e-1
    ## 8 RandomWalker         4 NNAR        Test  1.19  145.  0.547  155. 1.39  7.77e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1588|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1581|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1525|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1495|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1320|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1171|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [779|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [201|28]>  <mdl_tm_t [1 × 5]>

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
