# Examine missingness across variables

df |> 
  summarise(across(everything(), ~mean(is.na(.)))) |> 
  pivot_longer(everything()) |> 
  arrange(desc(value)) |> 
  head(20)

# Missingness of key covariates
df |> 
  summarise(
    ASD_missing = mean(is.na(ASD)),
    age_missing = mean(is.na(b_age_m)),
    edu_missing = mean(is.na(b_edu_m)),
    parity_missing = mean(is.na(b_order))
  )

# Demographic characteristics table (Table 1)

vars <- c("b_sex","b_age_m","b_edu_m","b_order","b_edu_f","b_wic")
factor_vars <- c("b_sex","b_edu_m","b_edu_f","b_wic")

tab1 <- CreateTableOne(
  vars = vars,
  strata = "Race_Label",
  data = df,
  factorVars = factor_vars,
  addOverall = TRUE
)

print(tab1, showAllLevels = TRUE)

# Examine distribution of ASD variable
table(df$aut_levl, useNA = "ifany")

df |> 
  summarise(
    n = n(),
    cases = sum(ASD == 1, na.rm = TRUE),
    prevalence = mean(ASD == 1, na.rm = TRUE)
  )

df |> 
  summarise(
    autlevl_nonmissing = mean(!is.na(aut_levl)),
    autlevl_missing = mean(is.na(aut_levl))
  )

# ASD prevalence by subgroup with Wilson 95% CI

prev_tab <- df |> 
  group_by(Race_Label) |>
  summarise(
    N = n(),
    Cases = sum(ASD == 1),
    Prev = Cases / N
  ) |>
  rowwise() |>
  mutate(
    ci = list(binom.confint(Cases, N, methods="wilson")),
    CI_low = ci$lower,
    CI_high = ci$upper
  ) |>
  ungroup() |>
  select(-ci) |>
  arrange(desc(Prev))

prev_tab