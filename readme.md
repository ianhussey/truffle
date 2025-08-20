![](./man/figures/hex_small.png)

# truffle

{truffle} is an R package for teaching users to process data.

It allows you to create datasets with various known effects to be rediscovered (truffles, via `truffle_` functions), and then create data processing headaches that have to be solved (dirt, via `dirt_` functions). Users must then search for truffles among the dirt. 

Generated datasets can include demographics variables and item-level Likert responses. Known effects (truffles) can be buried in the data including differences in sum-score means between conditions, known correlations between the different outcomes' sum-scores, known Cronbach's alpha values for each scale, etc. 

Data can then be made dirty in several different ways to create common data processing challenges, such as poorly named columns that disagree with R, duplicate rows, missingness with irregular codings, mixed date formats, untidy demographics data with misspellings and erroneous entries, number columns with comma seperators and units, impossible values beyond the scale range, header rows that disrupt reading data into R, and untidy columns that contain more than one variable. 

Users can then be set the challenge to properly wrangle the dataset to remove the dirt and uncover the truffle, e.g., to answer questions like "are there significant differences between groups, and of what effect size?"

Note that the package's functions are currently quite fragile: it is designed for a specific use-case for my teaching and not (yet) highly flexible, nor does it contain unit tests or handle errors well. Currently the `truffle_` functions can only generate data for a single study design: a between groups experiment with equal sample sizes. It's likely I'll extend this to cover other cases in the future.

## Existing R packages

Other packages exist to cover some of these features, but none fit mine and not all in the one place. For example:

Data generation/simulation:

- [{lavaan}](https://lavaan.ugent.be/) :  Used internally by {truffle} to generate observed variables from latent models, allow you to generate data with specific population Cronbach's alpha values, M, SD, associtions, etc. {lavaan} is quite focused on non-experimental design. You can generate seperate datasets that systematically differ in their means, as I have done in {truffle}, but it's not the most common use-case that {lavaan} caters to. 
- [{latent2likert}](https://latent2likert.lalovic.io/) : Used internally by {truffle} to convert continuous observed variables to Likert with minimal distortion.
- [{faux}](https://debruine.github.io/faux/) : Can also be used to generate multilevel data for correlational or experimental designs, but not both at the same time, at least in the way I wanted. Maybe I'm wrong - {faux} is definitely worth a look. Also contains functions for converting continuous data to Likert with minimal distortion.
- [{fabricatr}](https://declaredesign.org/r/fabricatr/), part of the {declardesign} packages : A very interesting set of packages that I have not fully gotten my head around, despite Dorthy Bishop's encouragement that more people do so. 
- [{wakefield}](https://github.com/trinker/wakefield) : A popular package for generating simulated data, {wakefield} is particularly useful for generating common demographic and psychological data columns, but also repeated measures and time series data. However, it's not orientated towards multilevel structures, as far as I understand.  

Adding mess, dirt, and data processing challenges:

- {wakefield} and {fabricatr} :  As well as generating data, both packages also allow you to add problems. 
- [{messy}](https://github.com/nrennie/messy) : A cool package for adding messiness to existing data. However, I wanted to make much, much more mess.
- [{toddler}](https://github.com/rbcavanaugh/toddler) : Very well named and adds even more mess than {messy}, but I wanted even more chaos.



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

<br>

Applied to a more complex design with three correlated outcomes, with more participants, and with broken headers, the data is now truly dirty. Perfect for training students to overcome the sort of data processing issues they'll encounter in the real world.

![](./man/figures/truffle_and_dirt_complex.png)





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
