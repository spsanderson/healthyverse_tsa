Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
27 August, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 45,329
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

The last day in the data set is 2022-08-25 23:42:45, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -1291.74
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 45329         |
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
| r_version     |     30753 |          0.32 |   5 |   5 |     0 |       33 |          0 |
| r_arch        |     30753 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     30753 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       28 |          0 |
| country       |      3578 |          0.92 |   2 |   2 |     0 |      117 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-08-25 | 2021-11-16 |      641 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1390626.15 | 1762586.67 | 357 | 16247 | 289858 | 2918356 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8827.94 |   16780.14 |   1 |   168 |   2473 |    9129 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-08-25 23:42:45 | 2021-11-16 15:45:33 |    26503 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 32M 39S |       60 |

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

    ## # A tibble: 6 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 4 healthyverse  <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 5 healthyR.ai   <tibble [369 × 2]> <tibble [28 × 2]> <split [341|28]>
    ## 6 TidyDensity   <tibble [217 × 2]> <tibble [28 × 2]> <split [189|28]>

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
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          1

``` r
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

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wflw_glm <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
  add_model(model_spec_glm)

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
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [369 × 2]> <tibble>     <split [341|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [217 × 2]> <tibble>     <split [189|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |     mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|---------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7768640 | 218.8566 | 0.7443758 | 136.2680 | 1.0618944 | 0.0364449 |
| healthyR      |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7980540 | 117.6498 | 0.7646796 | 181.0325 | 1.0254932 | 0.0014588 |
| healthyR      |         4 | EARTH       | Test  | 1.2253614 | 866.4923 | 1.1741172 | 132.2164 | 1.5173767 | 0.0014588 |
| healthyR      |         5 | NNAR        | Test  | 0.8643640 | 253.4758 | 0.8282166 | 148.8070 | 1.1155596 | 0.0034847 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9358603 | 112.1836 | 0.7245753 | 145.2322 | 1.1048618 | 0.0003166 |
| healthyR.data |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9044467 | 110.8805 | 0.7002538 | 130.8841 | 1.0730279 | 0.0089681 |
| healthyR.data |         4 | EARTH       | Test  | 0.9022069 | 114.1374 | 0.6985197 | 128.5468 | 1.0628915 | 0.0089681 |
| healthyR.data |         5 | NNAR        | Test  | 1.0480191 | 111.3105 | 0.8114125 | 147.6488 | 1.2829907 | 0.0038508 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7168154 | 127.3499 | 0.5875527 | 150.6100 | 0.8502994 | 0.2571803 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8170093 | 187.4874 | 0.6696787 | 161.3064 | 0.9560654 | 0.0010308 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.6162451 | 944.5839 | 1.3247891 | 130.9402 | 1.9072823 | 0.0010308 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7448748 | 213.6553 | 0.6105522 | 140.2785 | 0.9266520 | 0.1231887 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8456656 | 396.6358 | 0.7515412 | 145.2285 | 1.0401975 | 0.0025588 |
| healthyverse  |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7912271 | 588.9899 | 0.7031619 | 123.2923 | 0.9540316 | 0.0029506 |
| healthyverse  |         4 | EARTH       | Test  | 0.7803441 | 702.5524 | 0.6934902 | 117.1665 | 0.9239296 | 0.0029506 |
| healthyverse  |         5 | NNAR        | Test  | 0.8442613 | 352.2418 | 0.7502932 | 143.7106 | 1.0579632 | 0.0034893 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7780093 | 113.8644 | 0.6629196 | 165.0533 | 0.9895860 | 0.0832576 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7871900 | 120.1424 | 0.6707423 | 184.1810 | 1.0020196 | 0.0020738 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8278457 | 346.9787 | 0.7053838 | 141.6962 | 1.0886568 | 0.0020738 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7409051 | 184.0360 | 0.6313042 | 152.8000 | 0.9252257 | 0.0964556 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.9734914 | 102.6974 | 0.6965364 | 186.9152 | 1.1592804 | 0.0192821 |
| TidyDensity   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.9730873 | 107.2332 | 0.6962472 | 146.1043 | 1.1917478 | 0.0042619 |
| TidyDensity   |         4 | EARTH       | Test  | 1.5558949 | 292.3560 | 1.1132480 | 127.8986 | 1.9084520 | 0.0042619 |
| TidyDensity   |         5 | NNAR        | Test  | 1.2949995 | 167.4334 | 0.9265765 | 173.2671 | 1.5242763 | 0.2038027 |

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
    ##   # A tibble: 6 × 10
    ##   package       .model_id .model_d…¹ .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>             <int> <chr>      <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR              3 LM         Test  0.798  118. 0.765  181. 1.03  0.00146
    ## 2 healthyR.data         4 EARTH      Test  0.902  114. 0.699  129. 1.06  0.00897
    ## 3 healthyR.ts           1 ARIMA      Test  0.717  127. 0.588  151. 0.850 0.257  
    ## 4 healthyverse          4 EARTH      Test  0.780  703. 0.693  117. 0.924 0.00295
    ## 5 healthyR.ai           5 NNAR       Test  0.741  184. 0.631  153. 0.925 0.0965 
    ## 6 TidyDensity           1 ARIMA      Test  0.973  103. 0.697  187. 1.16  0.0193 
    ## # … with abbreviated variable name ¹​.model_desc

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
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [369 × 2]> <tibble>     <split [341|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [217 × 2]> <tibble>     <split [189|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

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
