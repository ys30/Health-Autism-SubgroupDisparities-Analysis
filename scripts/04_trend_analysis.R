# Select subgroups for trend visualization
keep_groups <- c("Vietnamese","Chinese","Filipino","Korean")

trend <- df |>
  filter(Race_Label %in% keep_groups,
         Year >= 2007, Year <= 2015) |>
  group_by(Year, Race_Label) |>
  summarise(
    N = n(),
    Cases = sum(ASD == 1),
    .groups = "drop"
  ) |>
  mutate(
    Prev = Cases / N
  ) |>
  rowwise() |>
  mutate(
    ci = list(binom.confint(Cases, N, methods = "wilson")),
    CI_low = ci$lower,
    CI_high = ci$upper
  ) |>
  ungroup() |>
  select(-ci)

# Trend plot with 95% confidence interval

ggplot(trend, aes(x = Year, y = Prev, color = Race_Label)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = Race_Label),
              alpha = 0.15, colour = NA, show.legend = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = 2007:2015) +
  labs(
    title = "ASD prevalence trends by selected Asian subgroups (2007–2015)",
    x = "Birth year",
    y = "ASD prevalence (%, 95% CI)"
  ) +
  theme_classic()

# Subgroup-specific p-trend

ptrend <- df |>
  filter(Race_Label %in% keep_groups,
         Year >= 2007, Year <= 2015) |>
  group_by(Race_Label) |>
  group_modify(~{
    fit <- glm(ASD ~ Year, data = .x, family = binomial())
    tibble(p_trend = coef(summary(fit))["Year","Pr(>|z|)"])
  }) |>
  ungroup()

ptrend