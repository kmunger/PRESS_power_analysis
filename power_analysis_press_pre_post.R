


library(DeclareDesign)
library(tidyverse)

N <- 100
ate <- 0.25
sd_1 <- 1
sd_2 <- 1
rho <- 0.5
attrition_rate <- 0.1

population <- declare_population(N = N, u_t1 = rnorm(N) * 
                                   sd_1, u_t2 = rnorm(N, rho * u_t1, sqrt(1 - rho^2)) * 
                                   sd_2, Y_t1 = u_t1)
potential_outcomes <- declare_potential_outcomes(Y_t2 ~ u_t2 + 
                                                   ate * Z)
estimand <- declare_estimand(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
assignment <- declare_assignment()
report <- declare_assignment(prob = 1 - attrition_rate, assignment_variable = R)
reveal_t2 <- declare_reveal(Y_t2)
manipulation <- declare_step(difference = (Y_t2 - Y_t1), 
                             handler = fabricate)
pretest_lhs <- declare_estimator(difference ~ Z, model = lm_robust, 
                                 estimand = estimand, subset = R == 1, label = "Change score")
pretest_rhs <- declare_estimator(Y_t2 ~ Z + Y_t1, model = lm_robust, 
                                 estimand = estimand, subset = R == 1, label = "Condition on pretest")
posttest_only <- declare_estimator(Y_t2 ~ Z, model = lm_robust, 
                                   estimand = estimand, label = "Posttest only")
pretest_posttest_design <- population + potential_outcomes + 
  estimand + assignment + reveal_t2 + report + manipulation + 
  pretest_lhs + pretest_rhs + posttest_only



diagnosis <- diagnose_design(pretest_posttest_design)


#####make a plot of the power of the different estimators as your vary the pre- and post-test outcomes


