Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
02 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,669
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

The last day in the data set is 2025-04-30 23:31:44, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6395.93
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138669        |
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
| r_version     |     99873 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99873 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99873 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11751 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-30 | 2023-06-13 | 1620 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134321.6 | 1520446.70 | 355 | 14701 | 278308 | 2367754 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10395.8 | 18469.19 | 1 | 291 | 3058 | 11710 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-30 23:31:44 | 2023-06-13 11:44:58 | 84582 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 28S |       60 |

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
    ## -146.78  -35.81  -10.90   26.57  814.73 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.868e+02  7.155e+01
    ## date                                                1.138e-02  3.794e-03
    ## lag(value, 1)                                       1.040e-01  2.467e-02
    ## lag(value, 7)                                       9.634e-02  2.556e-02
    ## lag(value, 14)                                      9.270e-02  2.559e-02
    ## lag(value, 21)                                      6.814e-02  2.574e-02
    ## lag(value, 28)                                      6.673e-02  2.562e-02
    ## lag(value, 35)                                      6.911e-02  2.576e-02
    ## lag(value, 42)                                      4.845e-02  2.584e-02
    ## lag(value, 49)                                      6.680e-02  2.570e-02
    ## month(date, label = TRUE).L                        -1.015e+01  5.133e+00
    ## month(date, label = TRUE).Q                         2.786e+00  5.197e+00
    ## month(date, label = TRUE).C                        -1.243e+01  5.185e+00
    ## month(date, label = TRUE)^4                        -6.565e+00  5.202e+00
    ## month(date, label = TRUE)^5                        -1.229e+01  5.199e+00
    ## month(date, label = TRUE)^6                        -3.208e+00  5.241e+00
    ## month(date, label = TRUE)^7                        -6.242e+00  5.172e+00
    ## month(date, label = TRUE)^8                        -4.388e+00  5.167e+00
    ## month(date, label = TRUE)^9                         5.216e+00  5.152e+00
    ## month(date, label = TRUE)^10                        4.710e+00  5.239e+00
    ## month(date, label = TRUE)^11                       -5.975e+00  5.336e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.146e+01  2.373e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.214e+00  2.498e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.611 0.009118 ** 
    ## date                                                 2.999 0.002754 ** 
    ## lag(value, 1)                                        4.214 2.65e-05 ***
    ## lag(value, 7)                                        3.769 0.000170 ***
    ## lag(value, 14)                                       3.623 0.000301 ***
    ## lag(value, 21)                                       2.648 0.008190 ** 
    ## lag(value, 28)                                       2.605 0.009279 ** 
    ## lag(value, 35)                                       2.684 0.007361 ** 
    ## lag(value, 42)                                       1.875 0.061036 .  
    ## lag(value, 49)                                       2.599 0.009426 ** 
    ## month(date, label = TRUE).L                         -1.977 0.048188 *  
    ## month(date, label = TRUE).Q                          0.536 0.591930    
    ## month(date, label = TRUE).C                         -2.397 0.016627 *  
    ## month(date, label = TRUE)^4                         -1.262 0.207143    
    ## month(date, label = TRUE)^5                         -2.365 0.018163 *  
    ## month(date, label = TRUE)^6                         -0.612 0.540529    
    ## month(date, label = TRUE)^7                         -1.207 0.227662    
    ## month(date, label = TRUE)^8                         -0.849 0.395841    
    ## month(date, label = TRUE)^9                          1.012 0.311479    
    ## month(date, label = TRUE)^10                         0.899 0.368825    
    ## month(date, label = TRUE)^11                        -1.120 0.262980    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.829 1.51e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.288 0.001032 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.84 on 1548 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2439, Adjusted R-squared:  0.2331 
    ## F-statistic: 22.69 on 22 and 1548 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.14335010229026"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.14335010229026"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.00590517997555"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.00590517997555"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.81780453398314"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.81780453398314"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.13873007504901"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.13873007504901"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.44336680556565"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.44336680556565"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.23542603812427"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.23542603812427"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.28423654088115"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.50058171632601"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.50058171632601"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.9257410986241"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.9257410986241"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.45178872328829"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.45178872328829"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.75385588030022"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.31212801779925"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.31212801779925"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.15596947985478"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.15596947985478"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.60406579696594"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.60406579696594"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.60746861914752"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.60746861914752"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.3369537220639"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.3369537220639"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.9946779562706"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.9946779562706"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.19921432400124"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.19921432400124"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.92352398038005"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.92352398038005"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.83937558587367"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.83937558587367"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.73242180509181"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.73242180509181"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.39358239825358"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.39358239825358"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.75936324370661"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.75936324370661"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.62228638465574"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.23281341215493"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 70, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.09477324425857"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 70, 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04718726957734"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 70, 84, 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04268668758579"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 70, 84, 63, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.04268668758579"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 70, 84, 63, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.0571809211216"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 70, 84, 63, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.0571809211216"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 70, 84, 63, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.53016018770377"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 70, 84, 63, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.53016018770377"

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
    ## 1 healthyR.data <tibble [1,613 × 2]> <tibble [28 × 2]> <split [1585|28]>
    ## 2 healthyR      <tibble [1,606 × 2]> <tibble [28 × 2]> <split [1578|28]>
    ## 3 healthyR.ts   <tibble [1,550 × 2]> <tibble [28 × 2]> <split [1522|28]>
    ## 4 healthyverse  <tibble [1,521 × 2]> <tibble [28 × 2]> <split [1493|28]>
    ## 5 healthyR.ai   <tibble [1,345 × 2]> <tibble [28 × 2]> <split [1317|28]>
    ## 6 TidyDensity   <tibble [1,196 × 2]> <tibble [28 × 2]> <split [1168|28]>
    ## 7 tidyAML       <tibble [804 × 2]>   <tibble [28 × 2]> <split [776|28]> 
    ## 8 RandomWalker  <tibble [226 × 2]>   <tibble [28 × 2]> <split [198|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7477953 | 177.50385 | 0.6398036 | 134.3824 | 0.8722070 | 0.0000846 |
| healthyR.data | 2 | LM | Test | 0.7334343 | 176.53071 | 0.6275165 | 133.8989 | 0.8602772 | 0.0762302 |
| healthyR.data | 3 | EARTH | Test | 0.9603630 | 420.38292 | 0.8216736 | 120.7541 | 1.1621458 | 0.0762302 |
| healthyR.data | 4 | NNAR | Test | 0.7832706 | 107.36634 | 0.6701558 | 185.0248 | 0.9391761 | 0.1176408 |
| healthyR | 1 | ARIMA | Test | 0.5922174 | 91.92495 | 0.6750091 | 155.6661 | 0.7608525 | 0.0013862 |
| healthyR | 2 | LM | Test | 0.6059908 | 95.04924 | 0.6907080 | 172.0062 | 0.7548506 | 0.0761408 |
| healthyR | 3 | EARTH | Test | 0.6047369 | 100.02156 | 0.6892789 | 146.5221 | 0.7429363 | 0.0761408 |
| healthyR | 4 | NNAR | Test | 0.6028672 | 89.92090 | 0.6871477 | 144.5135 | 0.7581841 | 0.0077240 |
| healthyR.ts | 1 | ARIMA | Test | 0.8155007 | 99.23283 | 0.6647407 | 177.3971 | 0.9734388 | 0.0468129 |
| healthyR.ts | 2 | LM | Test | 0.8523183 | 167.71374 | 0.6947520 | 128.8787 | 1.0949704 | 0.0468129 |
| healthyR.ts | 3 | EARTH | Test | 1.0478351 | 288.37881 | 0.8541240 | 133.5530 | 1.2227080 | 0.0468129 |
| healthyR.ts | 4 | NNAR | Test | 0.8032612 | 208.44154 | 0.6547639 | 172.2785 | 0.9525788 | 0.0939259 |
| healthyverse | 1 | ARIMA | Test | 0.6969136 | 379.47857 | 0.7990302 | 113.2384 | 0.8321200 | 0.0069046 |
| healthyverse | 2 | LM | Test | 0.7157605 | 464.98818 | 0.8206386 | 109.8222 | 0.8359900 | 0.0009835 |
| healthyverse | 3 | EARTH | Test | 0.6854458 | 293.92426 | 0.7858821 | 117.8220 | 0.8580883 | 0.0009835 |
| healthyverse | 4 | NNAR | Test | 0.6577773 | 263.41678 | 0.7541594 | 115.9120 | 0.8201629 | 0.1130030 |
| healthyR.ai | 1 | ARIMA | Test | 0.8385450 | 124.87450 | 0.9191189 | 177.5273 | 1.0843392 | 0.0000545 |
| healthyR.ai | 2 | LM | Test | 0.8237292 | 124.81153 | 0.9028795 | 162.0286 | 1.0813315 | 0.2261503 |
| healthyR.ai | 3 | EARTH | Test | 0.8341338 | 135.46521 | 0.9142839 | 156.2031 | 1.0974015 | 0.2261503 |
| healthyR.ai | 4 | NNAR | Test | 0.8654663 | 158.38181 | 0.9486270 | 161.9786 | 1.1112674 | 0.0005826 |
| TidyDensity | 1 | ARIMA | Test | 0.5595698 | 305.17675 | 0.8375467 | 117.2051 | 0.6864162 | 0.0042011 |
| TidyDensity | 2 | LM | Test | 0.5964781 | 407.61946 | 0.8927898 | 105.5684 | 0.7516267 | 0.0312688 |
| TidyDensity | 3 | EARTH | Test | 0.5612018 | 273.28067 | 0.8399894 | 115.1627 | 0.6849726 | 0.0312688 |
| TidyDensity | 4 | NNAR | Test | 0.5425700 | 178.76457 | 0.8121020 | 134.6238 | 0.6921351 | 0.0235404 |
| tidyAML | 1 | ARIMA | Test | 0.6281977 | 253.25650 | 0.9240348 | 105.9237 | 0.7243568 | 0.0005012 |
| tidyAML | 2 | LM | Test | 0.6167689 | 270.56668 | 0.9072239 | 100.4497 | 0.7269563 | 0.0081809 |
| tidyAML | 3 | EARTH | Test | 0.5777065 | 180.79794 | 0.8497659 | 108.5553 | 0.6904421 | 0.0081809 |
| tidyAML | 4 | NNAR | Test | 0.6052511 | 249.34754 | 0.8902820 | 102.5732 | 0.7075656 | 0.0047853 |
| RandomWalker | 1 | ARIMA | Test | 1.3752585 | 117.63962 | 0.6374472 | 167.1822 | 1.6978682 | 0.2762383 |
| RandomWalker | 2 | LM | Test | 1.2298933 | 112.88758 | 0.5700689 | 171.2858 | 1.4585222 | 0.0055519 |
| RandomWalker | 3 | EARTH | Test | 1.2294062 | 113.20731 | 0.5698431 | 170.6515 | 1.4588793 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1734752 | 112.87523 | 0.5439184 | 159.0655 | 1.3632060 | 0.1271133 |

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
    ## 1 healthyR.da…         2 LM          Test  0.733  177. 0.628  134. 0.860 0.0762 
    ## 2 healthyR             3 EARTH       Test  0.605  100. 0.689  147. 0.743 0.0761 
    ## 3 healthyR.ts          4 NNAR        Test  0.803  208. 0.655  172. 0.953 0.0939 
    ## 4 healthyverse         4 NNAR        Test  0.658  263. 0.754  116. 0.820 0.113  
    ## 5 healthyR.ai          2 LM          Test  0.824  125. 0.903  162. 1.08  0.226  
    ## 6 TidyDensity          3 EARTH       Test  0.561  273. 0.840  115. 0.685 0.0313 
    ## 7 tidyAML              3 EARTH       Test  0.578  181. 0.850  109. 0.690 0.00818
    ## 8 RandomWalker         4 NNAR        Test  1.17   113. 0.544  159. 1.36  0.127

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1585|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1578|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1522|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1493|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1317|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1168|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [776|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [198|28]>  <mdl_tm_t [1 × 5]>

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
