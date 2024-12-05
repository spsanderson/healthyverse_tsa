Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
05 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 122,845
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

The last day in the data set is 2024-12-03 23:43:59, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -2844.14
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 122845        |
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
| r_version     |     86925 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     86925 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     86925 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10552 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-03 | 2023-03-26 | 1472 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1154648.43 | 1540455.62 | 355 | 14701 | 260378 | 2368034 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10340.97 | 17992.03 | 1 | 317 | 3100 | 11813 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-03 23:43:59 | 2023-03-26 18:03:36 | 74431 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 48S |       60 |

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
    ## -156.06  -34.60   -9.26   26.76  801.30 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.683e+02  8.172e+01
    ## date                                                1.018e-02  4.331e-03
    ## lag(value, 1)                                       1.361e-01  2.569e-02
    ## lag(value, 7)                                       9.635e-02  2.662e-02
    ## lag(value, 14)                                      1.052e-01  2.668e-02
    ## lag(value, 21)                                      3.745e-02  2.690e-02
    ## lag(value, 28)                                      7.866e-02  2.673e-02
    ## lag(value, 35)                                      6.831e-02  2.698e-02
    ## lag(value, 42)                                      3.536e-02  2.704e-02
    ## lag(value, 49)                                      1.115e-01  2.681e-02
    ## month(date, label = TRUE).L                        -1.152e+01  5.577e+00
    ## month(date, label = TRUE).Q                         1.210e+00  5.479e+00
    ## month(date, label = TRUE).C                        -1.192e+01  5.542e+00
    ## month(date, label = TRUE)^4                        -8.254e+00  5.459e+00
    ## month(date, label = TRUE)^5                        -1.308e+01  5.374e+00
    ## month(date, label = TRUE)^6                        -1.285e+00  5.409e+00
    ## month(date, label = TRUE)^7                        -9.101e+00  5.288e+00
    ## month(date, label = TRUE)^8                        -2.397e+00  5.277e+00
    ## month(date, label = TRUE)^9                         4.232e+00  5.266e+00
    ## month(date, label = TRUE)^10                        5.130e+00  5.265e+00
    ## month(date, label = TRUE)^11                       -6.152e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.172e+01  2.454e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.647e+00  2.564e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.060 0.039575 *  
    ## date                                                 2.351 0.018850 *  
    ## lag(value, 1)                                        5.299 1.35e-07 ***
    ## lag(value, 7)                                        3.620 0.000305 ***
    ## lag(value, 14)                                       3.942 8.47e-05 ***
    ## lag(value, 21)                                       1.392 0.164061    
    ## lag(value, 28)                                       2.942 0.003311 ** 
    ## lag(value, 35)                                       2.532 0.011462 *  
    ## lag(value, 42)                                       1.308 0.191119    
    ## lag(value, 49)                                       4.160 3.37e-05 ***
    ## month(date, label = TRUE).L                         -2.066 0.039047 *  
    ## month(date, label = TRUE).Q                          0.221 0.825235    
    ## month(date, label = TRUE).C                         -2.150 0.031700 *  
    ## month(date, label = TRUE)^4                         -1.512 0.130711    
    ## month(date, label = TRUE)^5                         -2.433 0.015089 *  
    ## month(date, label = TRUE)^6                         -0.238 0.812193    
    ## month(date, label = TRUE)^7                         -1.721 0.085455 .  
    ## month(date, label = TRUE)^8                         -0.454 0.649798    
    ## month(date, label = TRUE)^9                          0.804 0.421762    
    ## month(date, label = TRUE)^10                         0.974 0.330042    
    ## month(date, label = TRUE)^11                        -1.165 0.244070    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.776 1.98e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.592 0.009640 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.91 on 1400 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2596, Adjusted R-squared:  0.2479 
    ## F-statistic: 22.31 on 22 and 1400 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,438 × 2]> <tibble [28 × 2]> <split [1410|28]>
    ## 2 healthyR      <tibble [1,431 × 2]> <tibble [28 × 2]> <split [1403|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,377 × 2]> <tibble [28 × 2]> <split [1349|28]>
    ## 5 healthyverse  <tibble [1,348 × 2]> <tibble [28 × 2]> <split [1320|28]>
    ## 6 healthyR.ai   <tibble [1,174 × 2]> <tibble [28 × 2]> <split [1146|28]>
    ## 7 TidyDensity   <tibble [1,028 × 2]> <tibble [28 × 2]> <split [1000|28]>
    ## 8 tidyAML       <tibble [644 × 2]>   <tibble [28 × 2]> <split [616|28]> 
    ## 9 RandomWalker  <tibble [78 × 2]>    <tibble [28 × 2]> <split [50|28]>

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
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7923842 | 115.6127 | 0.6346558 | 166.17828 | 0.9824778 | 0.0453855 |
| healthyR.data | 2 | LM | Test | 0.8199352 | 191.5730 | 0.6567226 | 142.03244 | 0.9389182 | 0.0087996 |
| healthyR.data | 3 | EARTH | Test | 0.8107674 | 187.3388 | 0.6493798 | 142.60297 | 0.9400433 | 0.0087996 |
| healthyR.data | 4 | NNAR | Test | 0.8986229 | 132.4807 | 0.7197471 | 162.67596 | 1.1182424 | 0.0223459 |
| healthyR | 1 | ARIMA | Test | 0.6745766 | 188.5906 | 0.8100762 | 154.78485 | 0.7945986 | 0.0900177 |
| healthyR | 2 | LM | Test | 0.6753551 | 112.8782 | 0.8110110 | 184.86714 | 0.8069374 | 0.0433105 |
| healthyR | 3 | EARTH | Test | 0.7090043 | 217.3092 | 0.8514192 | 150.93931 | 0.8309433 | 0.0433105 |
| healthyR | 4 | NNAR | Test | 0.6382266 | 142.4460 | 0.7664247 | 160.10558 | 0.7625131 | 0.1361035 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 1.0438508 | 156.8975 | 0.8460900 | 126.83683 | 1.2139922 | 0.0510704 |
| healthyR.ts | 2 | LM | Test | 1.0006249 | 129.5404 | 0.8110534 | 136.65013 | 1.1451339 | 0.0510704 |
| healthyR.ts | 3 | EARTH | Test | 1.0016848 | 130.8840 | 0.8119125 | 135.72929 | 1.1479691 | 0.0510704 |
| healthyR.ts | 4 | NNAR | Test | 0.9365115 | 118.7803 | 0.7590865 | 176.23948 | 1.0565694 | 0.1210597 |
| healthyverse | 1 | ARIMA | Test | 0.5141920 | 147.0545 | 0.9459449 | 104.27263 | 0.5923343 | 0.0856999 |
| healthyverse | 2 | LM | Test | 0.4917509 | 243.6268 | 0.9046607 | 82.02698 | 0.6047890 | 0.0824265 |
| healthyverse | 3 | EARTH | Test | 0.6779956 | 104.3648 | 1.2472899 | 173.44955 | 0.7771524 | 0.0824265 |
| healthyverse | 4 | NNAR | Test | 0.5448682 | 145.5156 | 1.0023792 | 108.26270 | 0.6300198 | 0.0049247 |
| healthyR.ai | 1 | ARIMA | Test | 0.8158222 | 121.8982 | 0.9453194 | 167.61730 | 0.9304664 | 0.0000660 |
| healthyR.ai | 2 | LM | Test | 0.7633701 | 116.9074 | 0.8845415 | 158.05719 | 0.8645871 | 0.0443974 |
| healthyR.ai | 3 | EARTH | Test | 0.8414735 | 149.0920 | 0.9750424 | 155.27198 | 0.9631534 | 0.0443974 |
| healthyR.ai | 4 | NNAR | Test | 0.7823100 | 114.6788 | 0.9064877 | 160.95432 | 0.9168803 | 0.0000378 |
| TidyDensity | 1 | ARIMA | Test | 0.7892303 | 188.9824 | 0.6892481 | 126.42109 | 0.9160499 | 0.0287071 |
| TidyDensity | 2 | LM | Test | 0.8387879 | 203.1074 | 0.7325276 | 125.34468 | 0.9758900 | 0.0189887 |
| TidyDensity | 3 | EARTH | Test | 0.7838911 | 169.8276 | 0.6845853 | 129.23723 | 0.9089657 | 0.0189887 |
| TidyDensity | 4 | NNAR | Test | 0.7697220 | 148.3192 | 0.6722111 | 154.20104 | 0.8907647 | 0.0014203 |
| tidyAML | 1 | ARIMA | Test | 0.5847057 | 145.9713 | 0.6440537 | 90.79157 | 0.7562150 | 0.3067847 |
| tidyAML | 2 | LM | Test | 0.6039434 | 166.8620 | 0.6652441 | 87.92233 | 0.7702245 | 0.0788112 |
| tidyAML | 3 | EARTH | Test | 0.5854048 | 229.2758 | 0.6448238 | 79.98338 | 0.7234204 | 0.0788112 |
| tidyAML | 4 | NNAR | Test | 0.5595265 | 177.6467 | 0.6163189 | 82.31956 | 0.7012040 | 0.1590650 |
| RandomWalker | 1 | ARIMA | Test | 1.2846215 | 108.6733 | 0.6030003 | 186.51989 | 1.4315358 | 0.0028863 |
| RandomWalker | 2 | LM | Test | 1.2624236 | 100.4991 | 0.5925807 | 188.58082 | 1.4284981 | 0.0023464 |
| RandomWalker | 3 | EARTH | Test | 1.7689122 | 233.0642 | 0.8303260 | 138.59917 | 2.1683746 | 0.0023464 |
| RandomWalker | 4 | NNAR | Test | 1.8240581 | 268.4655 | 0.8562115 | 139.39136 | 2.2133600 | 0.0103382 |

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
    ## 1 healthyR.da…         2 LM          Test  0.820  192. 0.657 142.  0.939 0.00880
    ## 2 healthyR             4 NNAR        Test  0.638  142. 0.766 160.  0.763 0.136  
    ## 3 healthyR.ts          4 NNAR        Test  0.937  119. 0.759 176.  1.06  0.121  
    ## 4 healthyverse         1 ARIMA       Test  0.514  147. 0.946 104.  0.592 0.0857 
    ## 5 healthyR.ai          2 LM          Test  0.763  117. 0.885 158.  0.865 0.0444 
    ## 6 TidyDensity          4 NNAR        Test  0.770  148. 0.672 154.  0.891 0.00142
    ## 7 tidyAML              4 NNAR        Test  0.560  178. 0.616  82.3 0.701 0.159  
    ## 8 RandomWalker         2 LM          Test  1.26   100. 0.593 189.  1.43  0.00235

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1410|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1403|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1349|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1320|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1146|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1000|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [616|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [50|28]>   <mdl_tm_t [1 × 5]>

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
