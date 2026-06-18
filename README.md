# parametric-anova-and-nonparametric-kw-analysis

---
USRESP SUBMISSION 2026
---

This is a power analysis simulator that compares the powers of both analysis of variance (ANOVA) and the Kruskal-Wallis _H_-test.


##Guide
scenarios: default, reject, skewed_, hetero_, hetero_skewed_ highvariance_, highvariance_skewed_, low_n_
null hypothesis: true/false
effect size: 0.01, 0.1, 0.2, 0.3, 0.5

default: runs simulations where no assumptions are violated at effectsize=0
reject: runs simulation where no assumptions are violated and where effect size is greater than zero
skewed: normality assumption is violated (data is log-normal distributed)
hetero: homoscedasticity assumption is violated (heteroscedasticity)
highvariance: sd shifts from [1,1,1] to [5,5,5]
low_n_x: CLT is violated where x=[1,2,3,4] which correlates to n=[25,10,5,2] respectively (effect size set at 0.3)

EXAMPLES
reject001: runs simulations on ANOVA and KW under no assumption violations and effect size=0.01
hetero_skewed_true: runs simulations on ANOVA and KW under heteroskedastic and log-normal distributed data with effect size=0
low_n_3: runs simulations on ANOVA and KW where n=5 and effect size=0.3

You can configure these settings in config.yml! Instructions are repeated in usresp project code.R file in comments as well.
