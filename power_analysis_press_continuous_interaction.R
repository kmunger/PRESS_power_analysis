


library(DeclareDesign)
library(tidyverse)

####modeled on 

## http://discuss.declaredesign.org/t/power-for-a-continuous-interaction/57


design <-
  declare_population(
    N = 1150,
    #range of age blocks
    age = sample(-4:4, size = N, replace = TRUE), 
    noise = rnorm(N)
  ) +
  declare_potential_outcomes(Y ~ main * Z + age_fx*age + het * age * Z + noise) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), interaction = het) +
  declare_assignment(prob = 0.5) +
  declare_reveal(Y, Z) +
  declare_estimator(
    Y ~ Z  + Z * age,
    model = lm_robust,
    term = c("Z", "Z:age"),
    estimand = c("ATE", "interaction")
  )

designs <- redesign(design, het = c( .05, .07 , .09), main = c(.2), age_fx = c(.2))
simulations <- simulate_design(designs, sims = 100)
summary_df <-
  simulations %>%
  group_by(het, main, estimand_label) %>%
  summarize(power = mean(p.value <= 0.05),
            mean_estimate = mean(estimate))  


ggplot(summary_df, aes(het, power, group = estimand_label, color = estimand_label)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8) +
  theme_bw()


###generate a plausible estimate of the ratio in power between the ATE and the Het FX for a fixed N and model

