Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 September, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 77,126
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

The last day in the data set is 2023-09-15 23:55:33, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.055595^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 77126         |
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
| r_version     |     53064 |          0.31 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     53064 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     53064 |          0.31 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       46 |          0 |
| country       |      6428 |          0.92 |   2 |   2 |     0 |      142 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-09-15 | 2022-06-04 |     1027 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |       p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|----------:|--------:|:------|
| size          |         0 |             1 | 1277434.15 | 1634754.13 | 357 | 16873 | 322853 | 2448771.0 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9984.04 |   17851.12 |   1 |   158 |   2823 |   11080.5 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-09-15 23:55:33 | 2022-06-04 07:57:33 |    46401 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     35 |       60 |

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
    ## 1 TidyDensity   <tibble [550 × 2]> <tibble [28 × 2]> <split [522|28]>
    ## 2 healthyR      <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 3 healthyR.ai   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyR.data <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 5 healthyR.ts   <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 6 healthyverse  <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 7 tidyAML       <tibble [212 × 2]> <tibble [28 × 2]> <split [184|28]>

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [5 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [184|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.6127867 | 117.83334 | 0.5996021 | 123.33423 | 0.7304323 | 0.1214967 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6287371 | 127.80273 | 0.6152094 | 120.52218 | 0.7492114 | 0.0093015 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6886119 |  98.19262 | 0.6737959 | 171.68163 | 0.8548943 | 0.0093015 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6252378 | 100.66286 | 0.6117853 | 158.45504 | 0.7489700 | 0.2766806 |
| healthyR      |         1 | ARIMA       | Test  | 0.6342607 | 100.73229 | 0.6326991 | 169.80470 | 0.7685833 | 0.1816972 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6640407 | 106.02717 | 0.6624058 | 167.23739 | 0.8081532 | 0.0054436 |
| healthyR      |         4 | EARTH       | Test  | 0.6652640 | 112.19898 | 0.6636261 | 158.74331 | 0.8076792 | 0.0054436 |
| healthyR      |         5 | NNAR        | Test  | 0.6571161 | 100.85226 | 0.6554982 | 169.24388 | 0.7993123 | 0.1086985 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.5702600 | 113.05124 | 0.6127104 | 117.82618 | 0.6691450 | 0.4473049 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7192157 |  99.80309 | 0.7727544 | 197.81009 | 0.8511292 | 0.0051327 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.6134189 | 150.08411 | 0.6590821 | 108.69063 | 0.7822732 | 0.0051327 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.6314951 |  96.04760 | 0.6785039 | 146.36029 | 0.7460221 | 0.3575018 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7187039 | 127.76777 | 0.6883412 | 147.42352 | 0.8211657 | 0.0003359 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7118820 |  97.21549 | 0.6818075 | 161.81014 | 0.7889067 | 0.0042041 |
| healthyR.data |         4 | EARTH       | Test  | 0.6917421 | 101.09789 | 0.6625185 | 135.62246 | 0.7915704 | 0.0042041 |
| healthyR.data |         5 | NNAR        | Test  | 0.7311177 | 130.10525 | 0.7002306 | 184.29704 | 0.7808345 | 0.0288463 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8696877 | 154.57482 | 0.7576836 | 137.72858 | 1.1140305 | 0.1033566 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8789709 | 189.13898 | 0.7657712 | 130.59174 | 1.1046336 | 0.1541259 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8802096 |  92.64196 | 0.7668504 | 157.35767 | 1.2033021 | 0.1541259 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8853690 | 128.90228 | 0.7713454 | 147.84190 | 1.1723986 | 0.0102078 |
| healthyverse  |         1 | ARIMA       | Test  | 0.4700743 | 218.41084 | 0.8160910 |  70.83479 | 0.5833570 | 0.0340796 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.4975683 | 187.23213 | 0.8638230 |  77.72913 | 0.5955956 | 0.0005141 |
| healthyverse  |         4 | EARTH       | Test  | 0.4809492 | 269.06657 | 0.8349708 |  68.21278 | 0.6122437 | 0.0005141 |
| healthyverse  |         5 | NNAR        | Test  | 0.6406027 | 154.13417 | 1.1121434 | 113.09642 | 0.7479393 | 0.0044426 |
| tidyAML       |         1 | ARIMA       | Test  | 0.3861268 | 278.61993 | 0.7358931 |  68.51447 | 0.4893397 | 0.0382644 |
| tidyAML       |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.5022295 | 392.09551 | 0.9571653 |  74.65417 | 0.6065796 | 0.0550135 |
| tidyAML       |         4 | EARTH       | Test  | 0.3939809 | 277.54292 | 0.7508615 |  69.68911 | 0.4840767 | 0.0550135 |
| tidyAML       |         5 | NNAR        | Test  | 0.4144225 | 310.06794 | 0.7898199 |  70.23115 | 0.5223537 | 0.0133093 |

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
    ## 1 TidyDensity           1 ARIMA       Test  0.613  118. 0.600 123.  0.730 0.121 
    ## 2 healthyR              1 ARIMA       Test  0.634  101. 0.633 170.  0.769 0.182 
    ## 3 healthyR.ai           1 ARIMA       Test  0.570  113. 0.613 118.  0.669 0.447 
    ## 4 healthyR.data         5 NNAR        Test  0.731  130. 0.700 184.  0.781 0.0288
    ## 5 healthyR.ts           3 LM          Test  0.879  189. 0.766 131.  1.10  0.154 
    ## 6 healthyverse          1 ARIMA       Test  0.470  218. 0.816  70.8 0.583 0.0341
    ## 7 tidyAML               4 EARTH       Test  0.394  278. 0.751  69.7 0.484 0.0550

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [184|28]> <mdl_tm_t [1 × 5]>

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
