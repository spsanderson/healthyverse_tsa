Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 November, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 50,282
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

The last day in the data set is 2022-11-03 23:43:12, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -2971.75
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 50282         |
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
| r_version     |     34411 |          0.32 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     34411 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     34411 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       31 |          0 |
| country       |      3968 |          0.92 |   2 |   2 |     0 |      121 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-11-03 | 2021-12-15 |      711 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |        sd |  p0 |     p25 |    p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|----------:|----:|--------:|-------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1371845.33 | 1731377.8 | 357 | 16941.5 | 307810 | 2909721.00 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8843.79 |   16826.9 |   1 |   143.0 |   2378 |    9274.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-11-03 23:43:12 | 2021-12-15 10:50:14 |    29595 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     54 |       60 |

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
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [439 × 2]> <tibble [28 × 2]> <split [411|28]>
    ## 6 TidyDensity   <tibble [287 × 2]> <tibble [28 × 2]> <split [259|28]>

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
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [439 × 2]> <tibble [28 × 2]> <split [411|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [287 × 2]> <tibble [28 × 2]> <split [259|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7578961 | 119.51056 | 0.7055171 | 160.5296 | 0.9952392 | 0.0199101 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7104840 | 100.50958 | 0.6613817 | 192.1805 | 0.9304816 | 0.0061564 |
| healthyR      |         4 | EARTH       | Test  | 0.7242273 | 106.95032 | 0.6741751 | 162.6854 | 0.9638916 | 0.0061564 |
| healthyR      |         5 | NNAR        | Test  | 0.9698175 | 285.11239 | 0.9027924 | 169.3145 | 1.1205255 | 0.0686492 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9533460 |  96.90112 | 0.7050215 | 174.3066 | 1.0686007 | 0.0132702 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8325685 |  91.24076 | 0.6157037 | 108.1417 | 0.9829697 | 0.0028136 |
| healthyR.data |         4 | EARTH       | Test  | 0.8276596 |  92.69344 | 0.6120735 | 105.1983 | 0.9842539 | 0.0028136 |
| healthyR.data |         5 | NNAR        | Test  | 1.1162555 | 116.92101 | 0.8254969 | 171.8464 | 1.2501807 | 0.0969007 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8194116 | 144.08380 | 0.6313087 | 169.3389 | 0.9647017 | 0.0012684 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8084024 | 115.89024 | 0.6228268 | 153.9187 | 0.9605584 | 0.0647611 |
| healthyR.ts   |         4 | EARTH       | Test  | 2.6800668 | 968.87275 | 2.0648348 | 153.3437 | 3.0515495 | 0.0647611 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8010308 | 129.60322 | 0.6171474 | 173.7293 | 0.9496061 | 0.0236553 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8035945 | 180.16082 | 0.6377653 | 116.0481 | 0.9768609 | 0.0031600 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7967732 | 176.29064 | 0.6323516 | 115.0222 | 0.9732139 | 0.0161291 |
| healthyverse  |         4 | EARTH       | Test  | 0.8008733 | 220.22559 | 0.6356056 | 104.9422 | 0.9843019 | 0.0161291 |
| healthyverse  |         5 | NNAR        | Test  | 0.9523963 | 217.29465 | 0.7558604 | 160.1420 | 1.0967952 | 0.0169897 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8298212 | 103.97918 | 0.6486547 | 178.4474 | 1.0043905 | 0.1446454 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8082331 | 101.78821 | 0.6317797 | 150.8539 | 0.9959453 | 0.0627250 |
| healthyR.ai   |         4 | EARTH       | Test  | 3.4034401 | 923.34531 | 2.6604012 | 157.7262 | 3.8482701 | 0.0627250 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8253614 | 114.26131 | 0.6451685 | 153.6648 | 0.9909116 | 0.0025798 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7686077 | 149.24562 | 0.6423745 | 171.1763 | 0.8833566 | 0.1346880 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7790777 | 105.26025 | 0.6511249 | 164.2129 | 0.9301427 | 0.0895559 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7790885 | 109.17300 | 0.6511340 | 160.7562 | 0.9307864 |        NA |
| TidyDensity   |         5 | NNAR        | Test  | 0.8188993 | 141.08087 | 0.6844064 | 133.4154 | 1.0170345 | 0.0014824 |

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
    ## 1 healthyR              3 LM         Test  0.710 101.  0.661  192. 0.930 0.00616
    ## 2 healthyR.data         3 LM         Test  0.833  91.2 0.616  108. 0.983 0.00281
    ## 3 healthyR.ts           5 NNAR       Test  0.801 130.  0.617  174. 0.950 0.0237 
    ## 4 healthyverse          3 LM         Test  0.797 176.  0.632  115. 0.973 0.0161 
    ## 5 healthyR.ai           5 NNAR       Test  0.825 114.  0.645  154. 0.991 0.00258
    ## 6 TidyDensity           1 ARIMA      Test  0.769 149.  0.642  171. 0.883 0.135  
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
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [439 × 2]> <tibble [28 × 2]> <split [411|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [287 × 2]> <tibble [28 × 2]> <split [259|28]> <mdl_tm_t>
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
