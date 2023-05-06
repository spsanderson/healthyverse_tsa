Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 May, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 65,481
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

The last day in the data set is 2023-05-04 19:13:09, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -7335.25
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 65481         |
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
| r_version     |     44543 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     44543 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     44543 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5510 |          0.92 |   2 |   2 |     0 |      136 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-05-04 | 2022-03-28 |      893 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1309391.12 | 1663825.23 | 357 | 24837 | 313267 | 2721879 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9827.34 |   18088.96 |   1 |   131 |   2622 |   10794 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-05-04 19:13:09 | 2022-03-28 03:35:36 |    39310 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 54M 14S |       60 |

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

    ## # A tibble: 7 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 2 healthyR.ai   <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.data <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 4 healthyR.ts   <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 5 healthyverse  <tibble [534 × 2]> <tibble [28 × 2]> <split [506|28]>
    ## 6 TidyDensity   <tibble [469 × 2]> <tibble [28 × 2]> <split [441|28]>
    ## 7 tidyAML       <tibble [78 × 2]>  <tibble [28 × 2]> <split [50|28]>

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [441|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [50|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.9871350 |  251.97828 | 1.0063825 | 172.5028 | 1.2157497 | 0.0649297 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 1.0376534 |  289.24076 | 1.0578859 | 166.4237 | 1.2907140 | 0.2158103 |
| healthyR      |         4 | EARTH       | Test  | 2.7757546 | 1471.22150 | 2.8298772 | 176.4887 | 2.9294237 | 0.2158103 |
| healthyR      |         5 | NNAR        | Test  | 0.9968337 |  171.22160 | 1.0162703 | 161.5878 | 1.2364073 | 0.0030620 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8611864 |  155.46793 | 0.9166059 | 171.2738 | 1.0578998 | 0.0844664 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8766347 |  102.07052 | 0.9330483 | 197.6283 | 1.1139139 | 0.0445241 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8949468 |  178.69084 | 0.9525389 | 160.5520 | 1.1587785 | 0.0445241 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.9656889 |  246.58685 | 1.0278334 | 165.6590 | 1.1891849 | 0.0070472 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7518939 |  209.95420 | 0.7795140 | 133.3668 | 0.9413661 | 0.0311312 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8245489 |  434.91278 | 0.8548379 | 112.4973 | 1.0791113 | 0.0556615 |
| healthyR.data |         4 | EARTH       | Test  | 0.7811813 |  153.24523 | 0.8098772 | 151.8973 | 0.9544675 | 0.0556615 |
| healthyR.data |         5 | NNAR        | Test  | 0.7771199 |  214.82696 | 0.8056667 | 137.8088 | 0.9464474 | 0.0134016 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0358340 |   93.44079 | 0.8257600 | 140.2346 | 1.3701351 | 0.2268556 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.1080766 |  100.58503 | 0.8833513 | 155.2338 | 1.4505781 | 0.0352243 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.2493161 |  119.03208 | 0.9959465 | 173.3763 | 1.5860240 | 0.0352243 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.1767297 |  104.41363 | 0.9380811 | 177.1298 | 1.5470085 | 0.0808827 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8862412 |  100.47481 | 0.9405438 | 153.4511 | 1.0833979 | 0.0177926 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.8732929 |  475.77390 | 0.9268020 | 110.9127 | 1.1286612 | 0.0287238 |
| healthyverse  |         4 | EARTH       | Test  | 1.0921415 |  201.80604 | 1.1590601 | 162.6899 | 1.2726840 | 0.0287238 |
| healthyverse  |         5 | NNAR        | Test  | 0.8940185 |  208.21939 | 0.9487976 | 149.7567 | 1.0768288 | 0.0012264 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7640475 |  340.53616 | 0.8771736 | 159.8043 | 0.9616362 | 0.0763736 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7907962 |  220.72811 | 0.9078828 | 170.4828 | 1.0109059 | 0.0032685 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7921607 |  273.38328 | 0.9094493 | 169.5508 | 1.0056380 | 0.0032685 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7718364 |  258.13704 | 0.8861157 | 144.9236 | 1.0352283 | 0.0321273 |
| tidyAML       |         1 | ARIMA       | Test  | 0.9879829 |  182.53671 | 1.3020229 | 141.2884 | 1.2833479 |        NA |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 1.4022092 |  350.46219 | 1.8479153 | 145.4583 | 1.7295992 | 0.2925727 |
| tidyAML       |         4 | EARTH       | Test  | 1.4239093 |  358.74028 | 1.8765128 | 145.6224 | 1.7538210 | 0.2925727 |
| tidyAML       |         5 | NNAR        | Test  | 1.3464102 |  325.84808 | 1.7743800 | 138.0279 | 1.7462395 | 0.0063536 |

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
    ##   # A tibble: 7 × 10
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR            1 ARIMA       Test  0.987 252.  1.01   173. 1.22   0.0649 
    ## 2 healthyR.ai         1 ARIMA       Test  0.861 155.  0.917  171. 1.06   0.0845 
    ## 3 healthyR.d…         1 ARIMA       Test  0.752 210.  0.780  133. 0.941  0.0311 
    ## 4 healthyR.ts         1 ARIMA       Test  1.04   93.4 0.826  140. 1.37   0.227  
    ## 5 healthyver…         5 NNAR        Test  0.894 208.  0.949  150. 1.08   0.00123
    ## 6 TidyDensity         1 ARIMA       Test  0.764 341.  0.877  160. 0.962  0.0764 
    ## 7 tidyAML             1 ARIMA       Test  0.988 183.  1.30   141. 1.28  NA

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [441|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [50|28]>  <mdl_tm_t [1 × 5]>

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
