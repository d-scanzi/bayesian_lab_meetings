library(tidyverse)
library(emmeans)
library(brms)
library(tidybayes)

## EXPERIMENTAL SETTINGS
participants_per_group <- 10
n_groups <- 3
n_participants <- participants_per_group * n_groups

## Load data
rt_data <- read_csv("data/rt_data.csv")

## ANOVA

# 1a. Compute participants' means
participants_mean_rt <- rt_data |>
    group_by(participant_id, condition) |>
    summarise(
        average_rt = mean(rt),
        .groups = "drop"
    )

# 1b. Compute conditions' means
conditions_mean_rt <- rt_data |>
    group_by(condition) |>
    summarise(
        group_mean_rt = mean(rt),
        .groups = "drop") 

# 1c. Put data together
rt_data_anova <- left_join(participants_mean_rt, conditions_mean_rt, by="condition")

# 2. Compute Sum of Squares (between and within)
ss_between <- sum(participants_per_group*(conditions_mean_rt$group_mean_rt - mean(rt_data_anova$average_rt))^2)
ss_within <- sum((rt_data_anova$average_rt - rt_data_anova$group_mean_rt)^2)

# 3. Compute degrees of freedom
df_between <- n_groups - 1
df_within <- n_participants - n_groups

# 4. Compute F-ratio
between_group_variability <- ss_between / df_between
within_group_variability <- ss_within / df_within

F_ratio <- between_group_variability / within_group_variability

## Hypothesis testing
pf(F_ratio, df1=df_between, df2=df_within, lower.tail = F)

# Compare results to function
aov_results <- aov(average_rt ~ condition, data=rt_data_anova)
summary(aov_results)
estimated_aov_means <- emmeans(aov_results, spec = "condition")

## COMPARE WITH REGRESSION
lm_model <- lm(average_rt ~ condition, data=rt_data_anova)
summary(lm_model)

## BAYESIAN MODEL
bayesian_model <- brm(
    rt ~ condition + (1|participant_id),
    data = rt_data,
    family = gaussian(),
    iter    = 2000,
    warmup  = 1000,
    backend = "cmdstanr", 
    cores   = 4,
    threads = threading(14),
    file = file.path("data/rt_bayesian_model"),file_refit = "never"
)

# Looking at the posterior distributions
posterior_draws <- as_draws_df(bayesian_model) |>
    mutate(congruent = b_Intercept, 
           incongruent = b_Intercept + b_conditionincongruent,
           mixed = b_Intercept + b_conditionmixed) |>
    dplyr::select(congruent, incongruent, mixed) |>
    pivot_longer(
        cols = everything(),
        names_to = "condition",
        values_to = "rt"
    )

posterior_draws |>
    ggplot(aes(x=rt, y=condition)) +
    stat_slabinterval(colour="black", fill="lightgray") +
    geom_point(data=conditions_mean_rt, aes(x=group_mean_rt, y=condition), colour="green", shape=18, size=2) +
    labs(x="RT [ms]",
         y="") +
    theme_minimal()
