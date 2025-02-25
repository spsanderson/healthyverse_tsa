Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
25 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 132,338
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

The last day in the data set is 2025-02-23 23:59:31, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4812.4
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 132338        |
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
| r_version     |     94908 |          0.28 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     94908 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     94908 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11176 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-23 | 2023-05-10 | 1554 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1138910.94 | 1528259.65 | 355 | 14701 | 260621.5 | 2367814.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10396.01 | 18393.04 | 1 | 291 | 3064.0 | 11914.5 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-23 23:59:31 | 2023-05-10 10:18:49 | 80256 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     50 |       60 |

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
    ## -154.24  -34.87  -10.50   26.84  809.69 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -221.65292   76.54308
    ## date                                                  0.01307    0.00406
    ## lag(value, 1)                                         0.11328    0.02511
    ## lag(value, 7)                                         0.08850    0.02614
    ## lag(value, 14)                                        0.10196    0.02614
    ## lag(value, 21)                                        0.06392    0.02633
    ## lag(value, 28)                                        0.05894    0.02629
    ## lag(value, 35)                                        0.07538    0.02643
    ## lag(value, 42)                                        0.05428    0.02649
    ## lag(value, 49)                                        0.08629    0.02634
    ## month(date, label = TRUE).L                         -12.91958    5.23100
    ## month(date, label = TRUE).Q                           2.60970    5.19911
    ## month(date, label = TRUE).C                         -10.81651    5.29311
    ## month(date, label = TRUE)^4                          -9.12697    5.27577
    ## month(date, label = TRUE)^5                         -11.12991    5.23365
    ## month(date, label = TRUE)^6                          -2.85052    5.31787
    ## month(date, label = TRUE)^7                          -7.71075    5.26512
    ## month(date, label = TRUE)^8                          -3.11431    5.30156
    ## month(date, label = TRUE)^9                           4.57566    5.31526
    ## month(date, label = TRUE)^10                          5.02753    5.32119
    ## month(date, label = TRUE)^11                         -6.27184    5.33540
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.82247    2.41693
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    7.73459    2.54592
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.896  0.00384 ** 
    ## date                                                 3.220  0.00131 ** 
    ## lag(value, 1)                                        4.511 6.97e-06 ***
    ## lag(value, 7)                                        3.385  0.00073 ***
    ## lag(value, 14)                                       3.900  0.00010 ***
    ## lag(value, 21)                                       2.428  0.01530 *  
    ## lag(value, 28)                                       2.242  0.02509 *  
    ## lag(value, 35)                                       2.852  0.00440 ** 
    ## lag(value, 42)                                       2.049  0.04063 *  
    ## lag(value, 49)                                       3.276  0.00108 ** 
    ## month(date, label = TRUE).L                         -2.470  0.01363 *  
    ## month(date, label = TRUE).Q                          0.502  0.61578    
    ## month(date, label = TRUE).C                         -2.044  0.04118 *  
    ## month(date, label = TRUE)^4                         -1.730  0.08384 .  
    ## month(date, label = TRUE)^5                         -2.127  0.03362 *  
    ## month(date, label = TRUE)^6                         -0.536  0.59202    
    ## month(date, label = TRUE)^7                         -1.464  0.14327    
    ## month(date, label = TRUE)^8                         -0.587  0.55700    
    ## month(date, label = TRUE)^9                          0.861  0.38946    
    ## month(date, label = TRUE)^10                         0.945  0.34491    
    ## month(date, label = TRUE)^11                        -1.176  0.23998    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.892 1.11e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.038  0.00242 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.55 on 1482 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:   0.26,  Adjusted R-squared:  0.249 
    ## F-statistic: 23.66 on 22 and 1482 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,519 × 2]> <tibble [28 × 2]> <split [1491|28]>
    ## 2 healthyR      <tibble [1,512 × 2]> <tibble [28 × 2]> <split [1484|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,458 × 2]> <tibble [28 × 2]> <split [1430|28]>
    ## 5 healthyverse  <tibble [1,429 × 2]> <tibble [28 × 2]> <split [1401|28]>
    ## 6 healthyR.ai   <tibble [1,255 × 2]> <tibble [28 × 2]> <split [1227|28]>
    ## 7 TidyDensity   <tibble [1,109 × 2]> <tibble [28 × 2]> <split [1081|28]>
    ## 8 tidyAML       <tibble [724 × 2]>   <tibble [28 × 2]> <split [696|28]> 
    ## 9 RandomWalker  <tibble [158 × 2]>   <tibble [28 × 2]> <split [130|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6859237 | 107.03830 | 0.6132865 | 161.54293 | 0.8063885 | 0.0974259 |
| healthyR.data | 2 | LM | Test | 0.8096904 | 135.85441 | 0.7239467 | 145.59183 | 0.9458181 | 0.0361230 |
| healthyR.data | 3 | EARTH | Test | 0.8532449 | 201.92266 | 0.7628889 | 123.52582 | 1.0408603 | 0.0361230 |
| healthyR.data | 4 | NNAR | Test | 0.6961068 | 103.91290 | 0.6223912 | 167.41894 | 0.8322736 | 0.0126486 |
| healthyR | 1 | ARIMA | Test | 0.9680417 | 105.45457 | 0.7084286 | 158.83670 | 1.1309038 | 0.0316434 |
| healthyR | 2 | LM | Test | 0.9630099 | 105.55635 | 0.7047462 | 195.47606 | 1.1130262 | 0.0998255 |
| healthyR | 3 | EARTH | Test | 0.9726441 | 111.43500 | 0.7117967 | 193.83638 | 1.1199531 | 0.0998255 |
| healthyR | 4 | NNAR | Test | 1.0151570 | 128.39956 | 0.7429083 | 180.09456 | 1.1589940 | 0.0260965 |
| healthyR.ts | 1 | ARIMA | Test | 1.0323671 | 111.53527 | 0.6249992 | 136.61285 | 1.2288663 | 0.0937102 |
| healthyR.ts | 2 | LM | Test | 1.0217521 | 118.98169 | 0.6185729 | 129.23155 | 1.2099771 | 0.0937102 |
| healthyR.ts | 3 | EARTH | Test | 1.0208419 | 120.02285 | 0.6180218 | 128.44723 | 1.2081116 | 0.0937102 |
| healthyR.ts | 4 | NNAR | Test | 1.1024181 | 135.68806 | 0.6674084 | 179.75058 | 1.3122233 | 0.0197925 |
| healthyverse | 1 | ARIMA | Test | 0.6179406 | 202.72761 | 0.7354034 | 115.86311 | 0.7704843 | 0.0942763 |
| healthyverse | 2 | LM | Test | 0.7010188 | 304.83484 | 0.8342737 | 114.36544 | 0.8724476 | 0.1227544 |
| healthyverse | 3 | EARTH | Test | 0.6868749 | 283.82404 | 0.8174412 | 115.52062 | 0.8509442 | 0.1227544 |
| healthyverse | 4 | NNAR | Test | 0.5900128 | 129.74669 | 0.7021669 | 119.85565 | 0.7231997 | 0.0875669 |
| healthyR.ai | 1 | ARIMA | Test | 0.8165053 | 96.00694 | 0.6516653 | 177.34739 | 0.9719146 | 0.0044814 |
| healthyR.ai | 2 | LM | Test | 0.8496562 | 102.18250 | 0.6781235 | 159.11448 | 1.0215522 | 0.1008817 |
| healthyR.ai | 3 | EARTH | Test | 0.8571684 | 103.89351 | 0.6841191 | 155.94082 | 1.0350708 | 0.1008817 |
| healthyR.ai | 4 | NNAR | Test | 0.8206620 | 100.45251 | 0.6549828 | 164.36043 | 0.9832303 | 0.0248147 |
| TidyDensity | 1 | ARIMA | Test | 0.7224742 | 119.27084 | 0.6665926 | 114.58929 | 0.8451843 | 0.0381557 |
| TidyDensity | 2 | LM | Test | 0.7195243 | 146.76030 | 0.6638709 | 102.80770 | 0.8478538 | 0.0895469 |
| TidyDensity | 3 | EARTH | Test | 0.7352776 | 115.17353 | 0.6784057 | 117.25588 | 0.8673458 | 0.0895469 |
| TidyDensity | 4 | NNAR | Test | 0.7418297 | 88.87637 | 0.6844510 | 140.85352 | 0.9111287 | 0.0714590 |
| tidyAML | 1 | ARIMA | Test | 0.6305388 | 136.40147 | 0.6817694 | 99.22166 | 0.7635233 | 0.1135233 |
| tidyAML | 2 | LM | Test | 0.6721354 | 139.26337 | 0.7267457 | 99.96259 | 0.8093392 | 0.0143161 |
| tidyAML | 3 | EARTH | Test | 1.8235393 | 348.96750 | 1.9717000 | 173.39438 | 2.0652329 | 0.0143161 |
| tidyAML | 4 | NNAR | Test | 0.6377532 | 135.89352 | 0.6895700 | 100.22722 | 0.7776719 | 0.0659463 |
| RandomWalker | 1 | ARIMA | Test | 1.2682669 | 111.29589 | 0.5655263 | 164.30694 | 1.4398097 | 0.0812156 |
| RandomWalker | 2 | LM | Test | 1.3238755 | 114.58632 | 0.5903225 | 193.82067 | 1.4876256 | 0.0002188 |
| RandomWalker | 3 | EARTH | Test | 1.2705557 | 85.31667 | 0.5665469 | 154.04224 | 1.5008567 | NA |
| RandomWalker | 4 | NNAR | Test | 1.5876235 | 167.51808 | 0.7079290 | 163.86425 | 1.8171204 | 0.1346280 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.686 107.  0.613 162.  0.806 0.0974 
    ## 2 healthyR             2 LM          Test  0.963 106.  0.705 195.  1.11  0.0998 
    ## 3 healthyR.ts          3 EARTH       Test  1.02  120.  0.618 128.  1.21  0.0937 
    ## 4 healthyverse         4 NNAR        Test  0.590 130.  0.702 120.  0.723 0.0876 
    ## 5 healthyR.ai          1 ARIMA       Test  0.817  96.0 0.652 177.  0.972 0.00448
    ## 6 TidyDensity          1 ARIMA       Test  0.722 119.  0.667 115.  0.845 0.0382 
    ## 7 tidyAML              1 ARIMA       Test  0.631 136.  0.682  99.2 0.764 0.114  
    ## 8 RandomWalker         1 ARIMA       Test  1.27  111.  0.566 164.  1.44  0.0812

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1491|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1484|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1430|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1401|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1227|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1081|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [696|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [130|28]>  <mdl_tm_t [1 × 5]>

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
