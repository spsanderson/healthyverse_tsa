Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 May, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 66,781
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

The last day in the data set is 2023-05-15 19:48:31, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -7599.84
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 66781         |
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
| r_version     |     45544 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     45544 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     45544 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5583 |          0.92 |   2 |   2 |     0 |      137 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-05-15 | 2022-04-04 |      904 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1311375.24 | 1664310.18 | 357 | 26522 | 322853 | 2721840 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9877.91 |   18118.99 |   1 |   131 |   2659 |   10899 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-05-15 19:48:31 | 2022-04-04 11:13:47 |    40076 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 55M 38S |       60 |

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
    ## 6 TidyDensity   <tibble [480 × 2]> <tibble [28 × 2]> <split [452|28]>
    ## 7 tidyAML       <tibble [89 × 2]>  <tibble [28 × 2]> <split [61|28]>

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [452|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [61|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7308493 |   95.71976 | 0.6375600 | 124.7736 | 0.9354640 | 0.1435404 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8682488 |  113.29057 | 0.7574211 | 168.4839 | 1.0692943 | 0.0386650 |
| healthyR      |         4 | EARTH       | Test  | 2.7851778 |  577.07494 | 2.4296634 | 168.7358 | 3.2008371 | 0.0386650 |
| healthyR      |         5 | NNAR        | Test  | 0.7743603 |  103.67429 | 0.6755169 | 140.1098 | 0.9893995 | 0.0620676 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7383206 |  118.65527 | 0.6883090 | 151.1084 | 0.9379372 | 0.3962475 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7872965 |  131.35975 | 0.7339675 | 158.0829 | 1.0129079 | 0.1413963 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7938893 |  108.66684 | 0.7401137 | 180.5162 | 1.0248835 | 0.1413963 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7717396 |  158.87168 | 0.7194643 | 156.9159 | 0.9921534 | 0.2098251 |
| healthyR.data |         1 | ARIMA       | Test  | 1.0657517 |  177.54295 | 0.8471285 | 158.1799 | 1.3385765 | 0.0196094 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 1.2171830 |  321.03439 | 0.9674959 | 147.5537 | 1.5230590 | 0.0257792 |
| healthyR.data |         4 | EARTH       | Test  | 1.0255325 |  125.85780 | 0.8151596 | 169.3180 | 1.2993920 | 0.0257792 |
| healthyR.data |         5 | NNAR        | Test  | 1.0456809 |  198.22619 | 0.8311749 | 155.6760 | 1.3336663 | 0.0033261 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0304259 |   94.68556 | 0.7295750 | 134.3316 | 1.4925693 | 0.0567765 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.0216191 |   95.29474 | 0.7233395 | 130.1100 | 1.4919083 | 0.1476636 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.3850536 |  156.77396 | 0.9806629 | 168.2748 | 1.8269952 | 0.1476636 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.0305387 |  100.92411 | 0.7296548 | 130.2892 | 1.4799608 | 0.0050058 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8676148 |  245.80541 | 0.8422167 | 132.3503 | 1.1092880 | 0.1672403 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.9380976 |  304.14120 | 0.9106363 | 132.1312 | 1.1826030 | 0.1105882 |
| healthyverse  |         4 | EARTH       | Test  | 2.6994263 | 1237.76473 | 2.6204047 | 160.2861 | 2.8533969 | 0.1105882 |
| healthyverse  |         5 | NNAR        | Test  | 0.7619904 |  144.89939 | 0.7396843 | 146.7801 | 0.9769240 | 0.0045965 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.8162695 |  123.50401 | 0.8592728 | 142.5608 | 1.0307577 | 0.2051706 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.8502065 |  109.25825 | 0.8949978 | 154.9406 | 1.0879555 | 0.1655242 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8556180 |  102.26035 | 0.9006943 | 160.2703 | 1.0901721 | 0.1655242 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8563842 |  140.40340 | 0.9015009 | 128.5909 | 1.1320277 | 0.0191435 |
| tidyAML       |         1 | ARIMA       | Test  | 1.1508394 |  234.36092 | 1.4859286 | 161.1916 | 1.4492158 |        NA |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 1.7309706 |  435.37799 | 2.2349762 | 178.2208 | 1.9062783 | 0.2755413 |
| tidyAML       |         4 | EARTH       | Test  | 1.5557517 |  374.92305 | 2.0087389 | 175.0549 | 1.7560336 | 0.2755413 |
| tidyAML       |         5 | NNAR        | Test  | 1.0690269 |  240.47080 | 1.3802948 | 145.5799 | 1.2965326 | 0.2818822 |

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
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR             1 ARIMA       Test  0.731  95.7 0.638  125. 0.935 0.144  
    ## 2 healthyR.ai          1 ARIMA       Test  0.738 119.  0.688  151. 0.938 0.396  
    ## 3 healthyR.da…         4 EARTH       Test  1.03  126.  0.815  169. 1.30  0.0258 
    ## 4 healthyR.ts          5 NNAR        Test  1.03  101.  0.730  130. 1.48  0.00501
    ## 5 healthyverse         5 NNAR        Test  0.762 145.  0.740  147. 0.977 0.00460
    ## 6 TidyDensity          1 ARIMA       Test  0.816 124.  0.859  143. 1.03  0.205  
    ## 7 tidyAML              5 NNAR        Test  1.07  240.  1.38   146. 1.30  0.282

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [452|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [61|28]>  <mdl_tm_t [1 × 5]>

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
