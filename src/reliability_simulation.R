library(tidyverse)
library(here)

# Failure rate values
p_vals <- c(0.1, 0.2, 0.3, 0.4, 0.5)

# Number of teams
n_vals <- 1:10

# Independent teams

df <- expand_grid(p = p_vals, n = n_vals) %>%
  mutate(
    success_prob = 1 - p^n,
    p_label = paste0("p = ", p)
  )

ggplot(df, aes(x = n, y = success_prob, color = p_label)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Success probability as number of teams increases",
    x = "Number of independent teams (n)",
    y = "Success probability",
    color = "Failure rate"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = n_vals) 

ggsave(here("plots", "independent_teams_performance.png"), width = 10, height = 6)

# Non-independent teams 

# Let $p$ be the marginal failure rate for each team.
# 
# Let $\rho \in [0,1]$ control how correlated the teams are.
# 
# - If $\rho = 0$, teams are independent.  
# - If $\rho = 1$, teams are “clones” that always all succeed or all fail together.
# 
# In each trial:
#   
#   1. With probability $\rho$ we draw one Bernoulli outcome and give it to all teams (perfectly correlated case).  
# 2. With probability $1 - \rho$ we draw independent Bernoulli outcomes for each team.
# 
# This setup keeps each teams failure probability at $p$ but induces positive correlation across teams.

simulate_parallel_correlated <- function(p, n, rho = 0, trials = 10000) {
  success_any <- logical(trials)
  
  for (t in seq_len(trials)) {
    if (runif(1) < rho) {
      # Perfectly correlated in this trial
      s <- rbinom(1, 1, prob = 1 - p)   # 1 = success
      successes <- rep(s, n)
    } else {
      # Independent in this trial
      successes <- rbinom(n, 1, prob = 1 - p)
    }
    success_any[t] <- any(successes == 1)
  }
  
  mean(success_any)
}

rho_vals <- c(0, 0.5, 0.9)   # 0 = independent, 0.9 = highly correlated

df_sim <- expand_grid(p = p_vals, n = n_vals, rho = rho_vals) %>%
  rowwise() %>%
  mutate(
    success_prob_sim = simulate_parallel_correlated(
      p = p,
      n = n,
      rho = rho,
      trials = 20000
    ),
    p_label   = paste0("p = ", p),
    rho_label = paste0("rho = ", rho)
  ) %>%
  ungroup()

ggplot(df_sim, aes(x = n, y = success_prob_sim, color = p_label)) +
  geom_line(size = 1.1) +
  facet_wrap(~ rho_label) +
  labs(
    title = "Success probability with correlated teams",
    subtitle = "Higher rho means more shared risk and less benefit from extra teams",
    x = "Number of teams (n)",
    y = "Simulated success probability",
    color = "Failure rate"
  ) +
  theme_minimal(base_size = 14)

ggsave(here("plots", "non_independent_teams_performance.png"), width = 10, height = 6)