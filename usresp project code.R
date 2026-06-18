
#Analysis of Variance and Kruskal-Wallis h-Test Simulations

#Guide:
#scenarios: default, reject, skewed_, hetero_, hetero_skewed_ highvariance_, highvariance_skewed_, low_n_
#null hypothesis: true/false
#effect size: 0.01, 0.1, 0.2, 0.3, 0.5

#Guide:
#default: runs simulations where no assumptions are violated at effectsize=0
#reject: runs simulation where no assumptions are violated and where effect size is greater than zero
#skewed: normality assumption is violated (data is log-normal distributed)
#hetero: homoscedasticity assumption is violated (heteroscedasticity)
#highvariance: sd shifts from [1,1,1] to [5,5,5]
#low_n_x: CLT is violated where x=[1,2,3,4] which correlates to n=[25,10,5,2] respectively (effect size set at 0.3)

#EXAMPLES
#reject001: runs simulations on ANOVA and KW under no assumption violations and effect size=0.01
#hetero_skewed_true: runs simulations on ANOVA and KW under heteroskedastic and log-normal distributed data with effect size=0
#low_n_3: runs simulations on ANOVA and KW where n=5 and effect size=0.3

#you can configure these settings in config.yml !

#generates seed, grabs config, and creates empty vectors anovaset1 and kwset1
set.seed(1)
conf <- config::get(config="default")

anovaset1 <- numeric(conf$iterations)
kwset1 <- numeric(conf$iterations)

#generates empty vectors anovaset2 and kwset2
for (w in seq_len(conf$iterations)){
  
  
  anovaset2 <- numeric(conf$simulations)
  kwset2 <- numeric(conf$simulations)
  
  for (c in seq_len(conf$simulations)){
    
    #generates random data based on specified parameters
    data <- lapply(seq_len(conf$groups), function(u){
      if (conf$condition=="normal"){
        rnorm(conf$sample, unlist(conf$miu[u]), unlist(conf$sd[u]))
      } else if (conf$condition=="skewed") {
        rlnorm(conf$sample, unlist(conf$miu[u]), unlist(conf$sd[u]))
      }
    }
    )
    
    #cleans data
    raw <- unlist(data)
    groups <- factor(rep(1:conf$groups, each = conf$sample))
    
    df <- data.frame(
      values = raw,
      group = groups
    )
    
    #extracts p-value and stores it to anovaset2 and kwset2 respectively
    results <- oneway.test(
      values ~ group,
      df,
      var.equal=TRUE
    )
    pVal <- results$p.value
    anovaset2[c] <- pVal
    
    results_kw <- kruskal.test(
      values ~ group,
      data = df,
    )
    pVal_kw <- results_kw$p.value
    kwset2[c] <- pVal_kw
    
  }
  
  #calculates proportion that were rejected from each test respectively
  anova_reject_proportion <- (sum(anovaset2<0.05))/conf$simulations
  kw_reject_proportion <- (sum(kwset2<0.05))/conf$simulations
  
  #stores each proportion into anovaset1 and kwset1 respectively
  anovaset1[w] <- anova_reject_proportion
  kwset1[w] <- kw_reject_proportion
}

print("Simulations are finished!")
print(anovaset1)
print(kwset1)

# -- Plot --

plot(density(anovaset1),col=rgb(0,0,1), 
     xlim=c(0, 1), 
     ylim=c(0,conf$iterations),
     
     xlab=
       if (conf$reject=="No"){
         "Proportion of Type I Error"
       } else {
         "Power of the test"
       },
     ylab="Density",
     main=conf$title
     )

lines(density(kwset1), col=rgb(1,0,0,))
legend(
  "topleft",
  legend= c("ANOVA", 
            "Kruskal-Wallis"),
  col= c("blue", "red"),
  pch= c(16,16)
)


# -- Statistical Analysis --
t_statistic <-t.test(anovaset1, kwset1)
print(t_statistic)
if (t_statistic$p.value > 0.05){
  print("Not significant")
} else if (t_statistic$p.value < 0.05){
  print("Significant")
}


