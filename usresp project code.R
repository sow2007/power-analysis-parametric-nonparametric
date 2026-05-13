# test

# combined anova and kw scripts

# remove the printed p-values
# remove graphs
# plot proportions of accepted/rejected
#run base code 1000 times


#REFERENECE

#Change the parameters of the code by switching 'conf <- config::get(config="x")'

#Index:
#default, reject
#skewed_true, skewed_false
#hetero_true, hetero_false, hetero_skewed_true, hetero_skewed_false
#highvarience_true, highvarience_false, highvarience_skewed_true, highvarience_skewed_false
#low_n_1, low_n_2, low_n_3, low_n_4

#Guide:
#default: runs simulations where no assumptions are violated (normality, homoscedasticity, CLT, etc.)
#skewed: normality assumption is violated (data is log-normal distributed)
#hetero: homoscedasticity assumption is violated (heteroscedasticity)
#highvarience: sd shifts from [1,1,1] to [5,5,5]
#low_n_x: CLT is violated (n<30)
#true/false: the center of each groups are equal/unequal respectively

#beta function changes by 0.01, 0.1, 0.3, 0.5


set.seed(1)
conf <- config::get(config="skewed_false")

anovaset1 <- numeric(conf$iterations)
kwset1 <- numeric(conf$iterations)

for (w in seq_len(conf$iterations)){
  
  
  anovaset2 <- numeric(conf$simulations)
  kwset2 <- numeric(conf$simulations)
  
  for (c in seq_len(conf$simulations)){
    
    
    #control group negative
    data <- lapply(seq_len(conf$groups), function(u){
      if (conf$condition=="normal"){
        rnorm(conf$sample, unlist(conf$miu[u]), unlist(conf$sd[u]))
      } else if (conf$condition=="skewed") {
        rlnorm(conf$sample, unlist(conf$miu[u]), unlist(conf$sd[u]))
      }
    }
    )
    
    
    raw <- unlist(data)
    groups <- factor(rep(1:conf$groups, each = conf$sample))
    
    df <- data.frame(
      values = raw,
      group = groups
    )
    
    #tests
    
    
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
  
  anova_reject_proportion <- (sum(anovaset2<0.05))/conf$simulations
  kw_reject_proportion <- (sum(kwset2<0.05))/conf$simulations
  
  anovaset1[w] <- anova_reject_proportion
  kwset1[w] <- kw_reject_proportion
}

print("Simulations are finished!")
print(anovaset1)
print(kwset1)

# -- Plot --

plot(density(anovaset1),col=rgb(0,0,1), 
     xlim=c(min(min(anovaset1), min(kwset1))-0.01, 
            max(max(anovaset1), max(kwset1))+0.01), 
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
  legend= c("ANOVA", "Kruskal-Wallis"),
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


