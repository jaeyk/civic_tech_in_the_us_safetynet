library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)

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
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = n_vals) 

ggsave(here("plots", "non_independent_teams_performance.png"), width = 10, height = 6)

# Simulation: heterogeneous teams, with and without shared learning

simulate_learning <- function(
    n_teams     = 10,
    T           = 20,     # number of iterations
    p_min       = 0.3,    
    p_max       = 0.8,    
    learn_rate  = 0.1,    # strength of learning update each step (0 = none, 1 = full)
    beta_limit  = 0.5     # how far teams can move toward best (0 = full, 1 = none)
) {
  # Initial heterogeneous failure rates
  p0 <- runif(n_teams, p_min, p_max)
  
  # Start each scenario at the same heterogeneous baseline
  p_iso    <- p0                      # 1. no learning
  p_best   <- p0                      # 2. learn from best (with constraints)
  p_peers  <- p0                      # 3. learn from local peers
  
  # Precompute "ideal but not fully attainable" targets for best-learning scenario
  p0_best <- min(p0)
  p_best_target <- p0_best + beta_limit * (p0 - p0_best)
  
  # System level success probabilities over time
  res_sys <- tibble(
    t = integer(),
    scenario = character(),
    success_prob = numeric()
  )
  
  # Team level failure paths over time (for the two learning scenarios)
  res_paths <- tibble(
    t = 0,
    scenario = factor(
      c(rep("Learn from the best (within constraints)", n_teams),
        rep("Learn from local peers", n_teams)),
      levels = c("Learn from the best (within constraints)", "Learn from local peers")
    ),
    team = factor(rep(seq_len(n_teams), 2)),
    p = c(p_best, p_peers)
  )
  
  for (t_step in seq_len(T)) {
    
    # System success probability at this step
    res_sys <- bind_rows(
      res_sys,
      tibble(
        t = t_step,
        scenario = "No learning (isolated)",
        success_prob = 1 - prod(p_iso)
      ),
      tibble(
        t = t_step,
        scenario = "Learn from the best (within constraints)",
        success_prob = 1 - prod(p_best)
      ),
      tibble(
        t = t_step,
        scenario = "Learn from local peers",
        success_prob = 1 - prod(p_peers)
      )
    )
    
    ## Update: learn from best, toward team-specific targets
    p_best <- (1 - learn_rate) * p_best + learn_rate * p_best_target
    p_best <- pmax(p_best, 0.01)
    
    ## Update: learn from *local* peers
    peer_targets <- numeric(n_teams)
    for (i in seq_len(n_teams)) {
      diffs <- abs(p_peers - p_peers[i])
      diffs[i] <- Inf                 # exclude self
      j <- which.min(diffs)           # closest neighbor in p-space
      peer_targets[i] <- p_peers[j]
    }
    p_peers <- (1 - learn_rate) * p_peers + learn_rate * peer_targets
    p_peers <- pmax(p_peers, 0.01)
    
    # Save paths for both learning scenarios
    res_paths <- bind_rows(
      res_paths,
      tibble(
        t = t_step,
        scenario = factor(
          c(rep("Learn from the best (within constraints)", n_teams),
            rep("Learn from local peers", n_teams)),
          levels = c("Learn from the best (within constraints)", "Learn from local peers")
        ),
        team = factor(rep(seq_len(n_teams), 2)),
        p = c(p_best, p_peers)
      )
    )
  }
  
  list(
    initial_p    = p0,
    p_best_target = p_best_target,
    data_sys     = res_sys,
    data_paths   = res_paths
  )
}

# Run the simulation
set.seed(1234)
sim <- simulate_learning()

# System-level success probability
system_plot <- sim$data_sys %>%
  ggplot(aes(x = t, y = success_prob, color = scenario)) +
  geom_line(size = 1.2) +
  labs(
    title = "System success probability over time",
    x = "Iteration",
    y = "Probability at least one team succeeds (log scale)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_log10() +
  coord_cartesian(xlim = c(1, 5)) +
  theme(legend.position = "bottom")

# Team-level success rates over time by learning rule
team_plot <- sim$data_paths %>%
  mutate(success = 1 - p) %>%
  ggplot(aes(x = t, y = success, group = team, color = team)) +
  geom_line(size = 0.9) +
  facet_wrap(~ scenario) +
  labs(
    title = "Team success rates over time under different learning rules",
    x = "Iteration",
    y = "Team success rate",
    color = "Team"
  ) +
  theme_minimal(base_size = 14)

system_plot / team_plot + plot_annotation(tag_levels = 'a')

ggsave(here("plots", "learning_simulation.png"), width = 10, height = 12)