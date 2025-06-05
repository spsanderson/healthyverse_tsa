Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
05 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,505
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

The last day in the data set is 2025-06-03 22:35:17, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.560262^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141505        |
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
| r_version     |    102068 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102068 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102068 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12024 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-03 | 2023-06-29 | 1654 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133103.06 | 1517183.03 | 355 | 14701 | 289681 | 2367727 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10430.93 | 18581.06 | 1 | 279 | 3039 | 11669 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-03 22:35:17 | 2023-06-29 02:30:18 | 86639 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 45S |       60 |

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
    ## -147.10  -35.80  -10.96   26.76  814.57 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.556e+02  6.846e+01
    ## date                                                9.731e-03  3.627e-03
    ## lag(value, 1)                                       1.047e-01  2.437e-02
    ## lag(value, 7)                                       9.706e-02  2.524e-02
    ## lag(value, 14)                                      8.971e-02  2.524e-02
    ## lag(value, 21)                                      6.875e-02  2.531e-02
    ## lag(value, 28)                                      7.284e-02  2.524e-02
    ## lag(value, 35)                                      6.603e-02  2.532e-02
    ## lag(value, 42)                                      4.954e-02  2.544e-02
    ## lag(value, 49)                                      6.924e-02  2.531e-02
    ## month(date, label = TRUE).L                        -9.578e+00  5.110e+00
    ## month(date, label = TRUE).Q                         4.097e+00  5.128e+00
    ## month(date, label = TRUE).C                        -1.350e+01  5.133e+00
    ## month(date, label = TRUE)^4                        -7.318e+00  5.174e+00
    ## month(date, label = TRUE)^5                        -1.095e+01  5.119e+00
    ## month(date, label = TRUE)^6                        -3.219e+00  5.221e+00
    ## month(date, label = TRUE)^7                        -7.626e+00  5.097e+00
    ## month(date, label = TRUE)^8                        -3.529e+00  5.104e+00
    ## month(date, label = TRUE)^9                         6.052e+00  5.116e+00
    ## month(date, label = TRUE)^10                        3.037e+00  5.085e+00
    ## month(date, label = TRUE)^11                       -4.851e+00  5.219e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.149e+01  2.333e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.956e+00  2.457e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.273 0.023131 *  
    ## date                                                 2.683 0.007376 ** 
    ## lag(value, 1)                                        4.297 1.84e-05 ***
    ## lag(value, 7)                                        3.845 0.000125 ***
    ## lag(value, 14)                                       3.554 0.000390 ***
    ## lag(value, 21)                                       2.716 0.006672 ** 
    ## lag(value, 28)                                       2.885 0.003961 ** 
    ## lag(value, 35)                                       2.608 0.009197 ** 
    ## lag(value, 42)                                       1.948 0.051628 .  
    ## lag(value, 49)                                       2.736 0.006290 ** 
    ## month(date, label = TRUE).L                         -1.875 0.061037 .  
    ## month(date, label = TRUE).Q                          0.799 0.424453    
    ## month(date, label = TRUE).C                         -2.630 0.008628 ** 
    ## month(date, label = TRUE)^4                         -1.414 0.157464    
    ## month(date, label = TRUE)^5                         -2.140 0.032489 *  
    ## month(date, label = TRUE)^6                         -0.616 0.537700    
    ## month(date, label = TRUE)^7                         -1.496 0.134840    
    ## month(date, label = TRUE)^8                         -0.691 0.489443    
    ## month(date, label = TRUE)^9                          1.183 0.237020    
    ## month(date, label = TRUE)^10                         0.597 0.550432    
    ## month(date, label = TRUE)^11                        -0.929 0.352852    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.926 9.26e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.238 0.001227 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.72 on 1582 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2397, Adjusted R-squared:  0.2291 
    ## F-statistic: 22.67 on 22 and 1582 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.84305971639704"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.31234039927429"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.31234039927429"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.54772051140696"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.54772051140696"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.58913620435204"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.58913620435204"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.77222231522958"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.77222231522958"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.05500504456311"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.05500504456311"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.72803368710353"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.72803368710353"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.46793854687148"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.16768098715823"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.16768098715823"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.13840658667054"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.13840658667054"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.99335728797437"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.99335728797437"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.44539465204986"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.93391115697534"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.82048132944977"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.82048132944977"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.68682050721118"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.68682050721118"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.30274583094747"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.30274583094747"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.14705457286473"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.00749770339749"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.00749770339749"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.20103492989328"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.20103492989328"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.6479347437039"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.6479347437039"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.50639044416941"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.50639044416941"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.64806965952394"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.64806965952394"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.42146923171446"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.42146923171446"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.51429964174309"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.19457586621613"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.19457586621613"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65992068051311"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65992068051311"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.60061286273888"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.60061286273888"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.01272298370292"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.65941429699859"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.65941429699859"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.30645674048209"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.30645674048209"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.07757049959098"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.07757049959098"

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
    ## 1 healthyR.data <tibble [1,646 × 2]> <tibble [28 × 2]> <split [1618|28]>
    ## 2 healthyR      <tibble [1,640 × 2]> <tibble [28 × 2]> <split [1612|28]>
    ## 3 healthyR.ts   <tibble [1,584 × 2]> <tibble [28 × 2]> <split [1556|28]>
    ## 4 healthyverse  <tibble [1,554 × 2]> <tibble [28 × 2]> <split [1526|28]>
    ## 5 healthyR.ai   <tibble [1,379 × 2]> <tibble [28 × 2]> <split [1351|28]>
    ## 6 TidyDensity   <tibble [1,230 × 2]> <tibble [28 × 2]> <split [1202|28]>
    ## 7 tidyAML       <tibble [838 × 2]>   <tibble [28 × 2]> <split [810|28]> 
    ## 8 RandomWalker  <tibble [260 × 2]>   <tibble [28 × 2]> <split [232|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7310629 | 142.41123 | 0.7844029 | 124.14981 | 0.9526865 | 0.0074169 |
| healthyR.data | 2 | LM | Test | 0.7438693 | 172.20368 | 0.7981437 | 121.62198 | 0.9501229 | 0.2284491 |
| healthyR.data | 3 | EARTH | Test | 0.7514163 | 258.09091 | 0.8062413 | 107.25555 | 0.9486233 | 0.2284491 |
| healthyR.data | 4 | NNAR | Test | 0.8516399 | 97.81285 | 0.9137775 | 175.74804 | 1.0907200 | 0.2777100 |
| healthyR | 1 | ARIMA | Test | 0.7712338 | 134.35310 | 0.8408236 | 162.53056 | 0.9561909 | 0.2298378 |
| healthyR | 2 | LM | Test | 0.7396511 | 96.20827 | 0.8063911 | 165.96848 | 0.9260409 | 0.1303104 |
| healthyR | 3 | EARTH | Test | 0.7028680 | 132.79116 | 0.7662891 | 139.84138 | 0.8551053 | 0.1303104 |
| healthyR | 4 | NNAR | Test | 0.7266929 | 102.77199 | 0.7922637 | 151.71258 | 0.9062507 | 0.1818140 |
| healthyR.ts | 1 | ARIMA | Test | 0.6308844 | 120.18056 | 0.7387236 | 133.11655 | 0.8341744 | 0.1332810 |
| healthyR.ts | 2 | LM | Test | 0.9148942 | 196.46404 | 1.0712802 | 157.95589 | 1.1233493 | 0.1332810 |
| healthyR.ts | 3 | EARTH | Test | 0.6252516 | 231.01835 | 0.7321280 | 98.11142 | 0.8338822 | 0.1332810 |
| healthyR.ts | 4 | NNAR | Test | 0.7186640 | 99.72056 | 0.8415077 | 177.34069 | 0.9179771 | 0.0005789 |
| healthyverse | 1 | ARIMA | Test | 0.6508284 | 117.51179 | 1.0630173 | 81.46379 | 0.8369560 | 0.0000627 |
| healthyverse | 2 | LM | Test | 0.6435990 | 147.24583 | 1.0512092 | 79.08784 | 0.7765806 | 0.0885065 |
| healthyverse | 3 | EARTH | Test | 0.6716749 | 124.87408 | 1.0970665 | 84.55811 | 0.8357205 | 0.0885065 |
| healthyverse | 4 | NNAR | Test | 0.7343743 | 105.45460 | 1.1994752 | 96.97067 | 0.9265333 | 0.0029558 |
| healthyR.ai | 1 | ARIMA | Test | 0.6160647 | 85.81468 | 0.9584713 | 149.50263 | 0.8080047 | 0.0293456 |
| healthyR.ai | 2 | LM | Test | 0.5765309 | 90.43697 | 0.8969647 | 122.56625 | 0.7643506 | 0.1999260 |
| healthyR.ai | 3 | EARTH | Test | 3.4168106 | 1263.22552 | 5.3158615 | 151.36814 | 3.7572372 | 0.1999260 |
| healthyR.ai | 4 | NNAR | Test | 0.6022033 | 90.00620 | 0.9369058 | 129.99469 | 0.7981112 | 0.0845411 |
| TidyDensity | 1 | ARIMA | Test | 0.4444164 | 162.51109 | 1.0386865 | 107.18252 | 0.5899010 | 0.1895217 |
| TidyDensity | 2 | LM | Test | 0.6046306 | 269.46463 | 1.4131379 | 115.93876 | 0.7537276 | 0.1135601 |
| TidyDensity | 3 | EARTH | Test | 0.4257273 | 157.01291 | 0.9950064 | 103.29912 | 0.5729630 | 0.1135601 |
| TidyDensity | 4 | NNAR | Test | 0.3900117 | 110.46869 | 0.9115323 | 118.42221 | 0.4997272 | 0.0118992 |
| tidyAML | 1 | ARIMA | Test | 0.8995476 | 154.83192 | 0.9536810 | 109.14684 | 1.1825885 | 0.0879587 |
| tidyAML | 2 | LM | Test | 0.8459304 | 130.35396 | 0.8968372 | 107.52856 | 1.1282316 | 0.1331166 |
| tidyAML | 3 | EARTH | Test | 1.3952784 | 269.26311 | 1.4792441 | 124.41979 | 1.6346537 | 0.1331166 |
| tidyAML | 4 | NNAR | Test | 0.8564770 | 139.71729 | 0.9080185 | 106.92999 | 1.1457238 | 0.0000055 |
| RandomWalker | 1 | ARIMA | Test | 1.1266776 | 103.32496 | 0.6052530 | 158.56655 | 1.3643129 | 0.0900256 |
| RandomWalker | 2 | LM | Test | 1.1763323 | 100.51526 | 0.6319276 | 198.36952 | 1.4182674 | 0.0916522 |
| RandomWalker | 3 | EARTH | Test | 1.1560911 | 99.43324 | 0.6210540 | 166.80543 | 1.4284955 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2394237 | 146.45526 | 0.6658204 | 165.44223 | 1.4077276 | 0.0186091 |

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
    ## 1 healthyR.data         3 EARTH       Test  0.751 258.  0.806 107.  0.949 0.228 
    ## 2 healthyR              3 EARTH       Test  0.703 133.  0.766 140.  0.855 0.130 
    ## 3 healthyR.ts           3 EARTH       Test  0.625 231.  0.732  98.1 0.834 0.133 
    ## 4 healthyverse          2 LM          Test  0.644 147.  1.05   79.1 0.777 0.0885
    ## 5 healthyR.ai           2 LM          Test  0.577  90.4 0.897 123.  0.764 0.200 
    ## 6 TidyDensity           4 NNAR        Test  0.390 110.  0.912 118.  0.500 0.0119
    ## 7 tidyAML               2 LM          Test  0.846 130.  0.897 108.  1.13  0.133 
    ## 8 RandomWalker          1 ARIMA       Test  1.13  103.  0.605 159.  1.36  0.0900

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1618|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1612|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1556|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1526|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1351|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1202|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [810|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [232|28]>  <mdl_tm_t [1 × 5]>

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
