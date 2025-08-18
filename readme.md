![](./man/figures/hex_small.png)

# truffle

{truffle} is an R package for teaching purposes. It allows you to create datasets with various known effects to be rediscovered (truffles, via `truffle_` functions), and then create data processing headaches that have to be solved (dirt, via `dirt_` functions). Users must then search for truffles among the dirt. 

Generated datasets can include demographics variables and item-level Likert responses. Known effects (truffles) can be buried in the data including differences in sum-score means between conditions, known correlations between the different outcomes' sum-scores, known Cronbach's alpha values for each scale, etc. Data can also be made messy, contain impossible values, or contain missingness, to create data processing challenges.

The package's functions are currently quite fragile: it is designed for a specific internal use case in our teaching and not (yet) highly flexible, nor does it contain tests or handle errors or make them visible. Currently it can only generate data for a single design: a between groups experiment with equal sample sizes.



## Usage

Generate data for the following experiment design (between groups factorial design with two groups):

- Item level Likert data (no choice)
- Between subjects experiment (control vs intervention) (2 conditions only, but can be renamed)
- Three outcome variables (arbitrary number)
- Known Cronbach's alpha for each scale (arbitrary number)
- Known number of items per scale (arbitrary number)
- Known number of Likert response options (1:k)
- Known correlations between the latent scale scores
- Known APPROXIMATE Cohen's d between the two conditions' latent scale scores (arbitrary number, but recovered value will differ due to reliability, distortion due to converting continuous data to Likert, etc.)

```{r}
library(truffle)
library(knitr)
library(kableExtra)

dat_truffle <- 
  truffle_likert(study_design = "factorial_between2",
                 n_per_condition = 5,
                 factors  = "X1_latent",
                 prefixes = "X1_item",
                 alpha = .70,
                 n_items = 5,
                 n_levels = 7,
                 approx_d_between_groups = 0.50,
                 seed = 42) |>
  truffle_demographics()

dat_truffle |>
  head(n = 10) |>
  kable() |>
  kable_classic(full_width = FALSE)
```

Output:

![](./man/figures/truffle.png)



Check that the sum scores conform to the predefined properties and that the item level data is approximately normal.

```{r}
truffle_check(dat_truffle)
```



Generate data for the same study but make the demographics data mess, add missingness, and add impossible values to the item level data.

```{r}
dat_truffle_and_dirt <- 
  # make truffle
  ## Likert data with known effects
  truffle_likert(n_per_condition = 5,
                 factors  = "X1_latent",
                 prefixes = "X1_item",
                 alpha = .70,
                 n_items = 5,
                 n_levels = 7,
                 approx_d_between_groups = 0.50,
                 seed = 42) |>
  ## RT data (no within or between effects baked in)
  mutate(completion_time = truffle_reaction_times(n = n())) |>
  # add dirt
  dirt_demographics() |>
  dirt_impossible_values(prop = .04, replacement_value = 8) |>
  
  dirt_numbers(cols = "completion_time") |>
  dirt_dates(col = "date") |>
  dirt_missingness(prop = .05) |>
  dirt_untidy(col = "block_trial") |>
  dirt_duplicates(prop = 0.05) |>
  ## move some columns around before dirt_colnames() makes it hard
  relocate(date, .after = "id") |>
  relocate(completion_time, .after = "id") |>
  relocate(block_trial, .after = "id")
  dirt_colnames() 

dat_truffle_and_dirt |>
  head(n = 10) |>
  kable() |>
  kable_classic(full_width = FALSE)
```

Output:

![](./man/figures/truffle_and_dirt.png)



## TODO

Possible extensions:

- Other study designs, e.g., cross sectional for regressions studies; Mixed within between for 2X2 RCT
- Improve fragility. Many functions rely on columns being called "X1_..." Etc.
- Improve ecological validity of column names. Eg help users rename the generated variables to things like "BDI\_", "MADRS\_", etc
- personal info 



## License

© Ian Hussey (2025)

MIT licence

## Suggested citation
Hussey, I. (2025) truffle: Tools to create datasets with various known effects to be rediscovered. https://github.com/ianhussey/truffle
