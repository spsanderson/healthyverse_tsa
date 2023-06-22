Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 June, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 69,902
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

The last day in the data set is 2023-06-20 23:58:41, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -8468.01
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 69902         |
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
| r_version     |     47763 |          0.32 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     47763 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     47763 |          0.32 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       44 |          0 |
| country       |      5847 |          0.92 |   2 |   2 |     0 |      139 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-06-20 | 2022-04-24 |      940 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1298806.65 | 1654323.06 | 357 | 23389 | 322873 | 2688776 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9939.63 |   18111.23 |   1 |   135 |   2727 |   10899 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-06-20 23:58:41 | 2022-04-24 03:03:32 |    41971 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     11 |       60 |

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
    ## 1 healthyR      <tibble [545 × 2]> <tibble [28 × 2]> <split [517|28]>
    ## 2 healthyR.ai   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 4 healthyR.ts   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 5 healthyverse  <tibble [535 × 2]> <tibble [28 × 2]> <split [507|28]>
    ## 6 TidyDensity   <tibble [516 × 2]> <tibble [28 × 2]> <split [488|28]>
    ## 7 tidyAML       <tibble [125 × 2]> <tibble [28 × 2]> <split [97|28]>

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [488|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [97|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7746924 | 113.60840 | 0.8604426 | 159.84619 | 0.9140277 | 0.1451325 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7458018 |  95.09725 | 0.8283541 | 172.36660 | 0.9172237 | 0.0209671 |
| healthyR      |         4 | EARTH       | Test  | 0.7552243 | 141.05392 | 0.8388195 | 120.73922 | 0.9847080 | 0.0209671 |
| healthyR      |         5 | NNAR        | Test  | 0.7216240 | 100.99349 | 0.8015001 | 171.60799 | 0.8529619 | 0.2168088 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8563688 | 115.54843 | 0.8975082 | 176.66773 | 0.9934721 | 0.0600661 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.9404754 | 139.63261 | 0.9856552 | 171.65424 | 1.0795073 | 0.0139951 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8550707 | 101.06495 | 0.8961478 | 189.98358 | 0.9999523 | 0.0139951 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7743333 | 112.71342 | 0.8115318 | 137.88990 | 0.9513156 | 0.0683858 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7617221 | 129.95923 | 0.7317739 | 156.88336 | 0.9103119 | 0.0079259 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6959544 | 124.98260 | 0.6685920 | 137.81329 | 0.8388446 | 0.0175288 |
| healthyR.data |         4 | EARTH       | Test  | 2.1313052 | 840.71863 | 2.0475100 | 143.69225 | 2.4427049 | 0.0175288 |
| healthyR.data |         5 | NNAR        | Test  | 0.7165291 | 129.46378 | 0.6883578 | 154.17776 | 0.8409060 | 0.0005099 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9183041 | 115.14030 | 0.7110007 | 127.03895 | 1.2263879 | 0.0250703 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.0007875 | 134.62478 | 0.7748639 | 133.60796 | 1.2406702 | 0.0088688 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.9591360 | 116.97239 | 0.7426151 | 141.88800 | 1.1879102 | 0.0088688 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8730080 | 103.14540 | 0.6759301 | 175.31126 | 1.0791107 | 0.1666704 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6816564 | 143.08749 | 0.9445265 | 125.53041 | 0.8115240 |        NA |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5924865 | 185.84483 | 0.8209696 |  95.28228 | 0.7251427 | 0.0282428 |
| healthyverse  |         4 | EARTH       | Test  | 0.6633992 | 367.56631 | 0.9192286 |  83.91335 | 0.7997026 | 0.0282428 |
| healthyverse  |         5 | NNAR        | Test  | 0.7042518 | 134.69402 | 0.9758354 | 133.58162 | 0.8333523 | 0.0084321 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6030303 | 102.19062 | 0.8478620 |  96.09312 | 0.9062400 | 0.3122360 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6180304 | 107.80068 | 0.8689522 |  96.82289 | 0.9617182 | 0.2322001 |
| TidyDensity   |         4 | EARTH       | Test  | 1.6539167 | 636.05987 | 2.3254108 | 121.55081 | 1.8301054 | 0.2322001 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7701321 | 119.27762 | 1.0828076 | 143.84358 | 1.1287239 | 0.0365129 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7256554 | 102.61769 | 1.0497609 | 191.90454 | 0.9083857 | 0.1999985 |
| tidyAML       |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.6724961 |  88.69572 | 0.9728586 | 151.09050 | 0.8593706 | 0.0605151 |
| tidyAML       |         4 | EARTH       | Test  | 0.7643126 | 284.22422 | 1.1056839 |  96.10187 | 0.8488467 | 0.0605151 |
| tidyAML       |         5 | NNAR        | Test  | 0.7722658 | 145.22170 | 1.1171893 | 145.08204 | 0.9559511 | 0.0064783 |

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
    ## 1 healthyR              5 NNAR        Test  0.722  101. 0.802 172.  0.853 0.217 
    ## 2 healthyR.ai           5 NNAR        Test  0.774  113. 0.812 138.  0.951 0.0684
    ## 3 healthyR.data         3 LM          Test  0.696  125. 0.669 138.  0.839 0.0175
    ## 4 healthyR.ts           5 NNAR        Test  0.873  103. 0.676 175.  1.08  0.167 
    ## 5 healthyverse          3 LM          Test  0.592  186. 0.821  95.3 0.725 0.0282
    ## 6 TidyDensity           1 ARIMA       Test  0.603  102. 0.848  96.1 0.906 0.312 
    ## 7 tidyAML               4 EARTH       Test  0.764  284. 1.11   96.1 0.849 0.0605

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [488|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [97|28]>  <mdl_tm_t [1 × 5]>

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
