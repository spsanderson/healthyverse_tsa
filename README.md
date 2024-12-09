Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 123,293
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

The last day in the data set is 2024-12-07 23:30:34, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -2939.91
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 123293        |
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
| r_version     |     87282 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     87282 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     87282 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10577 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-07 | 2023-03-29 | 1476 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1153365.81 | 1539651.81 | 355 | 14701 | 260378 | 2368012 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10362.38 | 17990.33 | 1 | 326 | 3115 | 11904 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-07 23:30:34 | 2023-03-29 18:59:33 | 74665 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 0S |       60 |

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
    ## -156.66  -34.60   -9.68   26.74  801.21 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.697e+02  8.140e+01
    ## date                                                1.025e-02  4.314e-03
    ## lag(value, 1)                                       1.367e-01  2.565e-02
    ## lag(value, 7)                                       9.518e-02  2.659e-02
    ## lag(value, 14)                                      1.062e-01  2.663e-02
    ## lag(value, 21)                                      4.018e-02  2.677e-02
    ## lag(value, 28)                                      7.781e-02  2.670e-02
    ## lag(value, 35)                                      6.695e-02  2.686e-02
    ## lag(value, 42)                                      3.724e-02  2.699e-02
    ## lag(value, 49)                                      1.110e-01  2.678e-02
    ## month(date, label = TRUE).L                        -1.134e+01  5.556e+00
    ## month(date, label = TRUE).Q                         1.483e+00  5.443e+00
    ## month(date, label = TRUE).C                        -1.172e+01  5.508e+00
    ## month(date, label = TRUE)^4                        -8.026e+00  5.436e+00
    ## month(date, label = TRUE)^5                        -1.293e+01  5.362e+00
    ## month(date, label = TRUE)^6                        -1.167e+00  5.402e+00
    ## month(date, label = TRUE)^7                        -9.047e+00  5.284e+00
    ## month(date, label = TRUE)^8                        -2.362e+00  5.274e+00
    ## month(date, label = TRUE)^9                         4.259e+00  5.264e+00
    ## month(date, label = TRUE)^10                        5.107e+00  5.263e+00
    ## month(date, label = TRUE)^11                       -6.140e+00  5.277e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.177e+01  2.451e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.699e+00  2.562e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.085 0.037215 *  
    ## date                                                 2.375 0.017664 *  
    ## lag(value, 1)                                        5.327 1.16e-07 ***
    ## lag(value, 7)                                        3.579 0.000357 ***
    ## lag(value, 14)                                       3.988 7.02e-05 ***
    ## lag(value, 21)                                       1.501 0.133550    
    ## lag(value, 28)                                       2.914 0.003623 ** 
    ## lag(value, 35)                                       2.493 0.012786 *  
    ## lag(value, 42)                                       1.380 0.167904    
    ## lag(value, 49)                                       4.147 3.57e-05 ***
    ## month(date, label = TRUE).L                         -2.042 0.041355 *  
    ## month(date, label = TRUE).Q                          0.272 0.785323    
    ## month(date, label = TRUE).C                         -2.128 0.033543 *  
    ## month(date, label = TRUE)^4                         -1.476 0.140072    
    ## month(date, label = TRUE)^5                         -2.412 0.015985 *  
    ## month(date, label = TRUE)^6                         -0.216 0.828954    
    ## month(date, label = TRUE)^7                         -1.712 0.087092 .  
    ## month(date, label = TRUE)^8                         -0.448 0.654303    
    ## month(date, label = TRUE)^9                          0.809 0.418614    
    ## month(date, label = TRUE)^10                         0.970 0.332046    
    ## month(date, label = TRUE)^11                        -1.164 0.244748    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.799 1.76e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.615 0.009021 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.89 on 1404 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2609, Adjusted R-squared:  0.2493 
    ## F-statistic: 22.53 on 22 and 1404 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,442 × 2]> <tibble [28 × 2]> <split [1414|28]>
    ## 2 healthyR      <tibble [1,435 × 2]> <tibble [28 × 2]> <split [1407|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,381 × 2]> <tibble [28 × 2]> <split [1353|28]>
    ## 5 healthyverse  <tibble [1,352 × 2]> <tibble [28 × 2]> <split [1324|28]>
    ## 6 healthyR.ai   <tibble [1,178 × 2]> <tibble [28 × 2]> <split [1150|28]>
    ## 7 TidyDensity   <tibble [1,032 × 2]> <tibble [28 × 2]> <split [1004|28]>
    ## 8 tidyAML       <tibble [648 × 2]>   <tibble [28 × 2]> <split [620|28]> 
    ## 9 RandomWalker  <tibble [82 × 2]>    <tibble [28 × 2]> <split [54|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8772305 | 105.14743 | 0.6953602 | 179.26663 | 1.0928466 | 0.0436129 |
| healthyR.data | 2 | LM | Test | 0.8848978 | 195.76793 | 0.7014379 | 146.35313 | 1.0118259 | 0.0078703 |
| healthyR.data | 3 | EARTH | Test | 0.9294229 | 107.46127 | 0.7367319 | 156.92143 | 1.1831487 | 0.0078703 |
| healthyR.data | 4 | NNAR | Test | 0.9342975 | 107.17558 | 0.7405959 | 160.20543 | 1.1912914 | 0.0669414 |
| healthyR | 1 | ARIMA | Test | 0.7210138 | 201.51350 | 0.8266541 | 151.44906 | 0.8437196 | 0.0973996 |
| healthyR | 2 | LM | Test | 0.6828091 | 109.09559 | 0.7828518 | 188.89488 | 0.8071644 | 0.0009840 |
| healthyR | 3 | EARTH | Test | 0.7400158 | 221.39858 | 0.8484402 | 152.55043 | 0.8650351 | 0.0009840 |
| healthyR | 4 | NNAR | Test | 0.7116730 | 155.34148 | 0.8159447 | 169.65921 | 0.8288554 | 0.0002153 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 1.0134697 | 174.71305 | 0.7827693 | 125.03614 | 1.1858992 | 0.3433613 |
| healthyR.ts | 2 | LM | Test | 1.0225370 | 133.45153 | 0.7897727 | 140.41047 | 1.1617213 | 0.0000114 |
| healthyR.ts | 3 | EARTH | Test | 1.2290574 | 213.42288 | 0.9492819 | 126.63951 | 1.4605337 | 0.0000114 |
| healthyR.ts | 4 | NNAR | Test | 1.0530390 | 144.13939 | 0.8133313 | 193.28011 | 1.1534125 | 0.0000047 |
| healthyverse | 1 | ARIMA | Test | 0.5073899 | 115.68227 | 0.9042380 | 111.10142 | 0.5948507 | 0.4244718 |
| healthyverse | 2 | LM | Test | 0.4896554 | 232.27226 | 0.8726326 | 78.13799 | 0.6001830 | 0.0004545 |
| healthyverse | 3 | EARTH | Test | 0.5127592 | 216.39624 | 0.9138068 | 85.27118 | 0.6030259 | 0.0004545 |
| healthyverse | 4 | NNAR | Test | 0.5813154 | 149.20991 | 1.0359833 | 109.64742 | 0.6696473 | 0.0220661 |
| healthyR.ai | 1 | ARIMA | Test | 0.8793353 | 132.98158 | 1.0003228 | 166.19951 | 0.9772393 | 0.0077944 |
| healthyR.ai | 2 | LM | Test | 0.7888111 | 115.71086 | 0.8973435 | 162.37727 | 0.8725958 | 0.0020336 |
| healthyR.ai | 3 | EARTH | Test | 0.8953295 | 149.47300 | 1.0185177 | 157.35209 | 1.0136148 | 0.0020336 |
| healthyR.ai | 4 | NNAR | Test | 0.8465109 | 113.93033 | 0.9629821 | 179.43868 | 0.9511179 | 0.0087247 |
| TidyDensity | 1 | ARIMA | Test | 0.7661990 | 226.83814 | 0.7013841 | 124.27827 | 0.9057827 | 0.0212557 |
| TidyDensity | 2 | LM | Test | 0.8235949 | 273.57215 | 0.7539247 | 122.72749 | 0.9695735 | 0.0159228 |
| TidyDensity | 3 | EARTH | Test | 0.7826778 | 239.11098 | 0.7164689 | 124.94598 | 0.9186102 | 0.0159228 |
| TidyDensity | 4 | NNAR | Test | 0.7914168 | 129.22566 | 0.7244686 | 158.65090 | 0.9323008 | 0.0552335 |
| tidyAML | 1 | ARIMA | Test | 0.6463704 | 79.92262 | 0.7007091 | 96.74236 | 0.7945808 | 0.2032371 |
| tidyAML | 2 | LM | Test | 0.6462472 | 88.05737 | 0.7005756 | 88.90514 | 0.8010884 | 0.0013949 |
| tidyAML | 3 | EARTH | Test | 0.6161893 | 104.12816 | 0.6679907 | 80.73535 | 0.7507940 | 0.0013949 |
| tidyAML | 4 | NNAR | Test | 0.6222731 | 100.53739 | 0.6745859 | 85.60281 | 0.7573122 | 0.0299139 |
| RandomWalker | 1 | ARIMA | Test | 1.3633953 | 100.00000 | 0.6229437 | 200.00000 | 1.5065502 | NA |
| RandomWalker | 2 | LM | Test | 1.3894043 | 102.75262 | 0.6348274 | 182.03119 | 1.5351636 | 0.0061613 |
| RandomWalker | 3 | EARTH | Test | 1.3057426 | 100.42880 | 0.5966018 | 144.05932 | 1.5004629 | 0.0061613 |
| RandomWalker | 4 | NNAR | Test | 1.3447607 | 114.07790 | 0.6144294 | 133.97438 | 1.6881259 | 0.0000477 |

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
    ## 1 healthyR.da…         2 LM          Test  0.885  196. 0.701 146.  1.01  7.87e-3
    ## 2 healthyR             2 LM          Test  0.683  109. 0.783 189.  0.807 9.84e-4
    ## 3 healthyR.ts          4 NNAR        Test  1.05   144. 0.813 193.  1.15  4.65e-6
    ## 4 healthyverse         1 ARIMA       Test  0.507  116. 0.904 111.  0.595 4.24e-1
    ## 5 healthyR.ai          2 LM          Test  0.789  116. 0.897 162.  0.873 2.03e-3
    ## 6 TidyDensity          1 ARIMA       Test  0.766  227. 0.701 124.  0.906 2.13e-2
    ## 7 tidyAML              3 EARTH       Test  0.616  104. 0.668  80.7 0.751 1.39e-3
    ## 8 RandomWalker         3 EARTH       Test  1.31   100. 0.597 144.  1.50  6.16e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1414|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1407|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1353|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1324|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1150|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1004|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [620|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [54|28]>   <mdl_tm_t [1 × 5]>

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
