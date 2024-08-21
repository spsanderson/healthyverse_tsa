Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 111,494
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

The last day in the data set is 2024-08-19 23:56:02, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -300.34
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 111494        |
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
| r_version     |     78180 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78180 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78180 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9546 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-19 | 2023-01-27 |     1366 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180435.62 | 1560536.40 | 355 | 14701 | 281133 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10312.38 |   18026.56 |   1 |   317 |   3082 |   11493 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-19 23:56:02 | 2023-01-27 15:53:44 |    67488 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     22 |       60 |

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
    ## 1 healthyR.data <tibble [1,337 × 2]> <tibble [28 × 2]> <split [1309|28]>
    ## 2 healthyR      <tibble [1,329 × 2]> <tibble [28 × 2]> <split [1301|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,275 × 2]> <tibble [28 × 2]> <split [1247|28]>
    ## 5 healthyverse  <tibble [1,246 × 2]> <tibble [28 × 2]> <split [1218|28]>
    ## 6 healthyR.ai   <tibble [1,072 × 2]> <tibble [28 × 2]> <split [1044|28]>
    ## 7 TidyDensity   <tibble [926 × 2]>   <tibble [28 × 2]> <split [898|28]> 
    ## 8 tidyAML       <tibble [542 × 2]>   <tibble [28 × 2]> <split [514|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5438577 |  106.01179 | 0.6531511 | 157.40833 | 0.7180700 | 0.2711921 |
| healthyR.data |         2 | LM          | Test  | 0.8206067 |  309.67544 | 0.9855156 | 161.99588 | 0.9567853 | 0.0177148 |
| healthyR.data |         3 | EARTH       | Test  | 1.7184963 |  914.19932 | 2.0638447 | 165.68944 | 1.9126428 | 0.0177148 |
| healthyR.data |         4 | NNAR        | Test  | 0.5543467 |   96.68091 | 0.6657480 | 129.91318 | 0.7656404 | 0.0022878 |
| healthyR      |         1 | ARIMA       | Test  | 0.6638929 |   83.55630 | 0.8628303 | 136.33366 | 0.8559956 | 0.0805834 |
| healthyR      |         2 | LM          | Test  | 0.7617103 |  104.26055 | 0.9899590 | 166.49940 | 0.9752001 | 0.0270415 |
| healthyR      |         3 | EARTH       | Test  | 0.6785515 |  101.34409 | 0.8818814 | 132.04541 | 0.8543607 | 0.0270415 |
| healthyR      |         4 | NNAR        | Test  | 0.7385563 |  106.07032 | 0.9598669 | 161.91063 | 0.9415274 | 0.0405483 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.7571323 |  744.11825 | 1.4363849 | 167.60531 | 1.9933106 | 0.1232482 |
| healthyR.ts   |         2 | LM          | Test  | 0.8217854 |  226.50421 | 0.6717764 | 121.01735 | 1.0624301 | 0.0201175 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.8208934 |  231.62192 | 0.6710473 | 120.17825 | 1.0610937 | 0.0201175 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8738595 |  127.30087 | 0.7143449 | 182.65704 | 1.0965402 | 0.0720818 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7341697 |  474.45810 | 0.9442032 | 117.67143 | 0.8441438 | 0.1499067 |
| healthyverse  |         2 | LM          | Test  | 0.7660880 |  582.88722 | 0.9852528 | 111.76804 | 0.8979844 | 0.0111863 |
| healthyverse  |         3 | EARTH       | Test  | 0.7507674 |  413.76979 | 0.9655492 | 122.85951 | 0.8802323 | 0.0111863 |
| healthyverse  |         4 | NNAR        | Test  | 0.7436690 |  276.89503 | 0.9564201 | 137.09640 | 0.8986448 | 0.0417886 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9074784 |  111.61906 | 0.8951396 | 175.16079 | 1.1458914 | 0.1917922 |
| healthyR.ai   |         2 | LM          | Test  | 0.7983847 |  136.54293 | 0.7875293 | 133.16581 | 1.0270335 | 0.0000570 |
| healthyR.ai   |         3 | EARTH       | Test  | 6.5038150 | 2742.79498 | 6.4153844 | 173.45484 | 6.9845843 | 0.0000570 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8860021 |  126.83312 | 0.8739554 | 148.21737 | 1.1651551 | 0.1190014 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5831911 |  235.59306 | 0.7261585 |  94.68688 | 0.6915950 | 0.5172992 |
| TidyDensity   |         2 | LM          | Test  | 0.6363117 |  257.55324 | 0.7923014 |  94.48190 | 0.7835459 | 0.0007909 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8650461 |  185.62058 | 1.0771093 | 182.63612 | 1.0312438 | 0.0007909 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6270426 |   99.39167 | 0.7807600 | 129.52724 | 0.7677169 | 0.2127152 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7401091 |  112.09872 | 0.8732813 | 134.39800 | 0.9152718 | 0.1319711 |
| tidyAML       |         2 | LM          | Test  | 0.7685117 |  120.81028 | 0.9067947 | 123.30816 | 0.9125007 | 0.0262888 |
| tidyAML       |         3 | EARTH       | Test  | 2.4777076 |  524.41239 | 2.9235365 | 168.11640 | 2.7610232 | 0.0262888 |
| tidyAML       |         4 | NNAR        | Test  | 0.6343041 |   99.81715 | 0.7484383 | 105.83166 | 0.8360942 | 0.1929275 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.544 106.  0.653 157.  0.718 2.71e-1
    ## 2 healthyR             3 EARTH       Test  0.679 101.  0.882 132.  0.854 2.70e-2
    ## 3 healthyR.ts          3 EARTH       Test  0.821 232.  0.671 120.  1.06  2.01e-2
    ## 4 healthyverse         1 ARIMA       Test  0.734 474.  0.944 118.  0.844 1.50e-1
    ## 5 healthyR.ai          2 LM          Test  0.798 137.  0.788 133.  1.03  5.70e-5
    ## 6 TidyDensity          1 ARIMA       Test  0.583 236.  0.726  94.7 0.692 5.17e-1
    ## 7 tidyAML              4 NNAR        Test  0.634  99.8 0.748 106.  0.836 1.93e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1309|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1301|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1247|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1218|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1044|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [898|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [514|28]>  <mdl_tm_t [1 × 5]>

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
