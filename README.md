Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 May, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 66,445
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

The last day in the data set is 2023-05-11 20:26:38, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -7504.47
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 66445         |
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
| r_version     |     45265 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     45265 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     45265 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5561 |          0.92 |   2 |   2 |     0 |      137 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-05-11 | 2022-04-02 |      900 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1311575.23 | 1665047.06 | 357 | 24837 | 313412 | 2721879 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9880.42 |   18120.67 |   1 |   131 |   2659 |   10899 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-05-11 20:26:38 | 2022-04-02 10:37:49 |    39873 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 54M 35S |       60 |

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
    ## 6 TidyDensity   <tibble [476 × 2]> <tibble [28 × 2]> <split [448|28]>
    ## 7 tidyAML       <tibble [85 × 2]>  <tibble [28 × 2]> <split [57|28]>

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [448|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [57|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7565687 |  122.97680 | 0.6428809 | 142.4842 | 0.9588785 | 0.0445408 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8917399 |  168.56709 | 0.7577403 | 172.8559 | 1.0856182 | 0.0041672 |
| healthyR      |         4 | EARTH       | Test  | 4.7252051 | 1686.50317 | 4.0151598 | 160.5114 | 5.1851403 | 0.0041672 |
| healthyR      |         5 | NNAR        | Test  | 0.8473118 |  139.12563 | 0.7199883 | 149.2647 | 1.0134459 | 0.0607767 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7943343 |  190.44312 | 0.7271118 | 141.7310 | 1.0187521 | 0.0573869 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8016890 |  155.67858 | 0.7338441 | 166.0896 | 1.0219702 | 0.0083040 |
| healthyR.ai   |         4 | EARTH       | Test  | 2.1526023 | 2692.78893 | 1.9704331 | 139.8429 | 2.4223862 | 0.0083040 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8569800 |  260.63154 | 0.7844560 | 164.5917 | 1.0284719 | 0.0450210 |
| healthyR.data |         1 | ARIMA       | Test  | 1.0448134 |  148.36371 | 0.8607951 | 169.7504 | 1.2968734 | 0.0149987 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 1.1546124 |  425.10340 | 0.9512558 | 138.8881 | 1.4801403 | 0.1186525 |
| healthyR.data |         4 | EARTH       | Test  | 0.9971918 |  112.25265 | 0.8215609 | 188.2653 | 1.2340887 | 0.1186525 |
| healthyR.data |         5 | NNAR        | Test  | 1.0115177 |  264.28794 | 0.8333637 | 144.9681 | 1.3034117 | 0.0091489 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0936969 |  128.61074 | 0.8098548 | 159.6665 | 1.4969499 | 0.0884618 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.0047467 |   96.23477 | 0.7439894 | 135.7037 | 1.4798300 | 0.0427726 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.6050830 |  216.44017 | 1.1885232 | 174.5934 | 2.0153883 | 0.0427726 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.0246720 |   97.70822 | 0.7587436 | 138.0589 | 1.4737642 | 0.0172008 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8720816 |  234.49428 | 0.7604270 | 134.4295 | 1.0914419 | 0.0379307 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.9626004 |  358.09508 | 0.8393565 | 128.0756 | 1.2075967 | 0.0163188 |
| healthyverse  |         4 | EARTH       | Test  | 3.1965295 | 1961.40149 | 2.7872706 | 156.2142 | 3.4512864 | 0.0163188 |
| healthyverse  |         5 | NNAR        | Test  | 0.8577045 |  220.65359 | 0.7478906 | 148.5341 | 1.0905408 | 0.0224543 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7820349 |  138.50340 | 0.8198484 | 146.8956 | 1.0121933 | 0.0941577 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.8329786 |  148.99261 | 0.8732553 | 162.2729 | 1.0710809 | 0.0147222 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8220087 |  180.12628 | 0.8617550 | 155.9703 | 1.0512485 | 0.0147222 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7916255 |  119.67062 | 0.8299027 | 126.7783 | 1.0910610 | 0.0326849 |
| tidyAML       |         1 | ARIMA       | Test  | 1.1790424 |  236.11609 | 1.4590841 | 158.9237 | 1.4671511 |        NA |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 1.6930796 |  405.59841 | 2.0952133 | 169.9709 | 1.9169448 | 0.0000012 |
| tidyAML       |         4 | EARTH       | Test  | 1.8610678 |  460.53500 | 2.3031015 | 171.9352 | 2.0756027 | 0.0000012 |
| tidyAML       |         5 | NNAR        | Test  | 1.0602844 |  221.35084 | 1.3121191 | 165.4884 | 1.3387983 | 0.0035034 |

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
    ## 1 healthyR             1 ARIMA       Test  0.757 123.  0.643  142. 0.959 0.0445 
    ## 2 healthyR.ai          1 ARIMA       Test  0.794 190.  0.727  142. 1.02  0.0574 
    ## 3 healthyR.da…         4 EARTH       Test  0.997 112.  0.822  188. 1.23  0.119  
    ## 4 healthyR.ts          5 NNAR        Test  1.02   97.7 0.759  138. 1.47  0.0172 
    ## 5 healthyverse         5 NNAR        Test  0.858 221.  0.748  149. 1.09  0.0225 
    ## 6 TidyDensity          1 ARIMA       Test  0.782 139.  0.820  147. 1.01  0.0942 
    ## 7 tidyAML              5 NNAR        Test  1.06  221.  1.31   165. 1.34  0.00350

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [448|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [57|28]>  <mdl_tm_t [1 × 5]>

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
