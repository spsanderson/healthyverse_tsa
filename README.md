Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,166
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

The last day in the data set is 2024-08-24 23:00:36, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -419.41
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112166        |
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
| r_version     |     78774 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78774 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78774 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9586 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-24 | 2023-01-31 |     1371 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1179873.47 | 1559941.63 | 355 | 14701 | 275048 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10286.28 |   17999.37 |   1 |   317 |   3075 |   11456 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-24 23:00:36 | 2023-01-31 15:18:52 |    67830 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     37 |       60 |

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,342 × 2]> <tibble [28 × 2]> <split [1314|28]>
    ## 2 healthyR      <tibble [1,334 × 2]> <tibble [28 × 2]> <split [1306|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,280 × 2]> <tibble [28 × 2]> <split [1252|28]>
    ## 5 healthyverse  <tibble [1,251 × 2]> <tibble [28 × 2]> <split [1223|28]>
    ## 6 healthyR.ai   <tibble [1,077 × 2]> <tibble [28 × 2]> <split [1049|28]>
    ## 7 TidyDensity   <tibble [931 × 2]>   <tibble [28 × 2]> <split [903|28]> 
    ## 8 tidyAML       <tibble [547 × 2]>   <tibble [28 × 2]> <split [519|28]>

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

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.5333119 |  114.32304 | 0.7286893 | 152.93878 | 0.7012878 | 0.2035659 |
| healthyR.data |         2 | LM          | Test  | 0.8620332 |  331.55909 | 1.1778369 | 171.78403 | 0.9828072 | 0.1882383 |
| healthyR.data |         3 | EARTH       | Test  | 0.4985915 |  145.74409 | 0.6812493 | 107.96927 | 0.6945001 | 0.1882383 |
| healthyR.data |         4 | NNAR        | Test  | 0.5253875 |  112.00488 | 0.7178618 | 130.90517 | 0.6976734 | 0.1282758 |
| healthyR      |         1 | ARIMA       | Test  | 0.6268316 |   85.59207 | 0.8398988 | 130.46404 | 0.8448810 | 0.0003338 |
| healthyR      |         2 | LM          | Test  | 0.7474213 |  113.69469 | 1.0014784 | 178.04715 | 0.9582116 | 0.0953127 |
| healthyR      |         3 | EARTH       | Test  | 0.5970224 |   93.52611 | 0.7999572 | 110.24505 | 0.7881146 | 0.0953127 |
| healthyR      |         4 | NNAR        | Test  | 0.7452549 |  120.51445 | 0.9985756 | 167.40767 | 0.9524579 | 0.0005192 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9403228 |  142.97877 | 0.8583387 | 192.35139 | 1.2031135 | 0.0277171 |
| healthyR.ts   |         2 | LM          | Test  | 0.7409647 |  232.97297 | 0.6763621 | 106.85286 | 1.0203716 | 0.1277519 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7382848 |  238.28422 | 0.6739159 | 105.75686 | 1.0182485 | 0.1277519 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9129126 |  156.07115 | 0.8333184 | 182.47081 | 1.1737157 | 0.0037171 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5858293 |  591.30079 | 0.8421477 | 113.88466 | 0.7048285 | 0.1976267 |
| healthyverse  |         2 | LM          | Test  | 0.6886368 |  798.92982 | 0.9899366 | 113.86081 | 0.7955865 | 0.0437728 |
| healthyverse  |         3 | EARTH       | Test  | 0.6325746 |  551.59260 | 0.9093456 | 121.18231 | 0.7481953 | 0.0437728 |
| healthyverse  |         4 | NNAR        | Test  | 0.5976554 |  358.66406 | 0.8591482 | 127.77566 | 0.7389765 | 0.0830569 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7934963 |  117.16241 | 0.8452521 | 175.12443 | 1.0179407 | 0.0947184 |
| healthyR.ai   |         2 | LM          | Test  | 0.7332240 |  155.28927 | 0.7810486 | 137.63184 | 0.9664523 | 0.0708858 |
| healthyR.ai   |         3 | EARTH       | Test  | 2.4298806 | 1139.41683 | 2.5883696 | 157.63711 | 2.8743008 | 0.0708858 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7787713 |  135.32877 | 0.8295666 | 156.10623 | 0.9917796 | 0.0418904 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4602972 |  223.38228 | 0.7300189 |  79.81897 | 0.5657028 | 0.2457887 |
| TidyDensity   |         2 | LM          | Test  | 0.5219668 |  255.91436 | 0.8278252 |  80.51679 | 0.6449101 | 0.0755092 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8373810 |  161.75937 | 1.3280636 | 186.94818 | 0.9913097 | 0.0755092 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5638105 |   79.25275 | 0.8941881 | 116.16545 | 0.7396674 | 0.0637823 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7392686 |  207.44833 | 0.9211200 | 141.66124 | 0.8852694 | 0.0133824 |
| tidyAML       |         2 | LM          | Test  | 0.6694581 |  177.14697 | 0.8341369 | 116.07718 | 0.8284245 | 0.0693760 |
| tidyAML       |         3 | EARTH       | Test  | 2.2677775 |  934.50597 | 2.8256239 | 179.55193 | 2.4298298 | 0.0693760 |
| tidyAML       |         4 | NNAR        | Test  | 0.6713271 |  246.69384 | 0.8364657 | 117.35411 | 0.8454900 | 0.0009562 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         3 EARTH       Test  0.499 146.  0.681 108.  0.695 0.188 
    ## 2 healthyR              3 EARTH       Test  0.597  93.5 0.800 110.  0.788 0.0953
    ## 3 healthyR.ts           3 EARTH       Test  0.738 238.  0.674 106.  1.02  0.128 
    ## 4 healthyverse          1 ARIMA       Test  0.586 591.  0.842 114.  0.705 0.198 
    ## 5 healthyR.ai           2 LM          Test  0.733 155.  0.781 138.  0.966 0.0709
    ## 6 TidyDensity           1 ARIMA       Test  0.460 223.  0.730  79.8 0.566 0.246 
    ## 7 tidyAML               2 LM          Test  0.669 177.  0.834 116.  0.828 0.0694

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
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1314|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1306|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1252|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1223|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1049|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [903|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [519|28]>  <mdl_tm_t [1 × 5]>

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
