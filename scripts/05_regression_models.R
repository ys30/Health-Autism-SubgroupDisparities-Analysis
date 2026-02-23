# Model A: Adjusted for age, sex, and birth year

modelA <- glm(
  ASD ~ relevel(factor(Race_Label), ref="Chinese") +
    factor(b_sex) +
    Year +
    b_age_m,
  data = df,
  family = binomial()
)

OR_A <- tidy(modelA, exponentiate = TRUE, conf.int = TRUE)
OR_A

# Model B: Fully adjusted model

modelB <- glm(
  ASD ~ relevel(factor(Race_Label), ref="Chinese") +
    factor(b_sex) +
    Year +
    b_age_m +
    factor(b_edu_m) +
    factor(b_order) +
    factor(b_wic),
  data = df,
  family = binomial()
)

OR_B <- tidy(modelB, exponentiate = TRUE, conf.int = TRUE)
OR_B

# Subgroup-specific p-trend

ptrend_full <- df |> 
  group_by(Race_Label) |>
  group_modify(~{
    fit <- glm(ASD ~ Year, data=.x, family=binomial())
    tibble(
      n = nrow(.x),
      p_trend = coef(summary(fit))["Year","Pr(>|z|)"]
    )
  }) |>
  arrange(p_trend)

ptrend_full

# model output
install.packages("modelsummary")
library(modelsummary)

modelsummary(
  list("Model A" = modelA,
       "Model B" = modelB),
  exponentiate = TRUE,
  estimate = "{estimate} ({conf.low}, {conf.high})",
  statistic = NULL,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik",
  output = "model_results.docx"
)
