library(tidyverse)
library(gamlss.dist)
library(emmeans)
library(gghalves)
library(brms)
library(tidybayes)

source("plot_theming.R")

set.seed(2026)
## Simulate RT data
n_participants <- 30
n_groups <- 3
n_trials <- 100
group_rt_means <- c(325, 328, 332)

participant_data_list <- list()
for (p in 1:n_participants) {
    
    participant_group <- (p %% n_groups) + 1
    participant_mean_rt <- rnorm(1, mean=group_rt_means[participant_group])
    participant_sd_rt <- runif(1, min = 1, max = 2.5)
    
    participant_rt <- tibble(
        participant_id = paste0("part-", p),
        condition = participant_group,
        rt = rexGAUS(n=n_trials, mu=participant_mean_rt, sigma=participant_sd_rt, nu=5)
    )
    participant_data_list <- append(participant_data_list, list(participant_rt))
}

rt_data <- Reduce(rbind, participant_data_list)
rt_data <- rt_data |>
    mutate(
        condition = case_when(
            condition == 1 ~ "congruent",
            condition == 2 ~ "incongruent",
            condition == 3 ~ "mixed"
        )
    )
rt_data$condition <- factor(rt_data$condition)

# Get means by group
rt_group_means <- rt_data |>
    group_by(condition) |>
    summarise(
        group_mean_rt = mean(rt),
        .groups = "drop") 

# Plot data
rt_data |>
    ggplot(aes(x=condition, y=rt)) +
    geom_jitter(width = 0.1, colour="gray", size=0.5, alpha=0.65) +
    geom_point(data=rt_group_means, aes(x=condition, y=group_mean_rt), colour="purple") +
    geom_half_violin(nudge = 0.15, fill="white", colour="black") +
    geom_smooth() +
    labs(
        x="",
        y="RT [s]"
    ) +
    theme_personal()
ggsave("dev/slides/images/rt_data_distr.svg")
write_csv(rt_data, "data/rt_data.csv")

## ANOVA
# H0: means of three groups are identical
# Ha: at least one mean is different from another one

# 1. compute participants' mean
rt_data_summary <- rt_data |>
    group_by(participant_id, condition) |>
    summarise(
        average_rt = mean(rt),
        .groups = "drop"
    )

rt_data_summary <- left_join(rt_data_summary, rt_group_means, by="condition")

# 2. compute variance
# Variance plot
variance_different <- ggplot(data.frame(x=seq(-6, 6, by=0.1)), aes(x)) +
    geom_function(fun = dnorm, args = list(mean=-2, sd=1), colour=colour_palette[1], size=2) +
    geom_function(fun = dnorm, args = list(mean=2, sd=1), colour=colour_palette[2], size=2) +
    labs(
        x="",
        y=""
    ) +
    theme_personal() +
    theme(axis.text.y = element_blank())
ggsave("dev/slides/images/distributions_different.svg", variance_different)

variance_same <- ggplot(data.frame(x=seq(-10, 10, by=0.1)), aes(x)) +
    geom_function(fun = dnorm, args = list(mean=0, sd=4), colour=colour_palette[1], size=2) +
    geom_function(fun = dnorm, args = list(mean=3, sd=2), colour=colour_palette[2], size=2) +
    labs(
        x="",
        y=""
    ) +
    theme_personal() +
    theme(axis.text.y = element_blank())
ggsave("dev/slides/images/distributions_overlap.svg", variance_same)

rt_variance <- sum((rt_data_summary$average_rt - mean(rt_data_summary$average_rt))^2) / n_participants
# 3 . compute the total sum of squares
ss_total <- sum((rt_data_summary$average_rt - mean(rt_data_summary$average_rt))^2)
ss_within <- sum((rt_data_summary$average_rt - rt_data_summary$group_mean_rt)^2)

participants_per_group <- n_participants / n_groups
ss_between <- sum(participants_per_group*(rt_group_means$group_mean_rt - mean(rt_data_summary$average_rt))^2)
# 4. Compute degrees of freedom
df_within <- n_participants - n_groups
df_between <- n_groups - 1
# 5. Compute F-ratio
MM_within <- ss_within / df_within
MM_between <- ss_between / df_between
F_ratio <- MM_between / MM_within

## Hypothesis testing
# Is the F_ratio really different from zero? 
# H0: NO, therefore all the observations come from the same population (distribution)
# H1: YES: therefore the observations come from different distributions depending on the group
pf(F_ratio, df1=df_between, df2=df_within, lower.tail = F)

# Compare results to function
aov_results <- aov(average_rt ~ condition, data=rt_data_summary)
summary(aov_results)
estimated_aov_means <- emmeans(aov_results, spec = "condition")
## REGRESSION
# Main thing to know here: R2 = SS_residuals / SS_total
# SS_residuals = SS of difference between predicted and observed values
# SS_total = SS between observed values and their overall mean

#  MM_between / MM_within = (ss_bet / df_bet) * (df_with / ss_with) = (ss_bet / ss_with) * (df_with / df_bet)
lm_model <- lm(average_rt ~ condition, data=rt_data_summary)
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
    file = file.path("data/rt_bayesian_model"),
    file_refit = "always"
)

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

posterior_plot <- posterior_draws |>
    ggplot(aes(x=rt, y=condition)) +
    stat_slabinterval(colour=colour_palette[3], fill="lightgray") +
    geom_point(data=rt_group_means, aes(x=group_mean_rt, y=condition), colour="green", shape=18, size=2) +
    labs(x="RT [ms]",
         y="") +
    theme_personal() +
    theme(panel.grid.major = element_line(colour="white"))
ggsave("dev/slides/images/bayesian_posteriors.svg", posterior_plot)


ggplot() |>
    geom_point(data=rt_group_means, aes(x=group_rt_mean, y=condition))


rt_group_means |>
    ggplot() +
    geom_jitter(aes(y=group_mean_rt, x=condition))
