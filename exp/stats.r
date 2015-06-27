################################################################################################
################################################################################################

# c45 = c(0.2433, 0.1733, 0.1733, 0.2633, 0.1633, 0.2400 , 0.2067 , 0.1500 , 0.2667 , 0.2967)
# nb = c(0.0733, 0.0667, 0.0167, 0.0700, 0.0733, 0.0500 , 0.1533 , 0.0400 , 0.0367 , 0.0733)
# > t.test(c45, nb, paired=TRUE) Paired t−test

# data : c45 and nb
# t = 8.0701, df = 9, p−value = 2.064e−05
# alternative hypothesis: true difference in means is not equal
# to0
# 95 percent confidence interval :
# 0.1096298 0.1950302 sample estimates :
# mean of the differences
# ￼0.15233

	
# if(p.value < aplha)
# 	#rejeita H0 - há diferenças 
# senaão
	#nao ha diferencás

# We obtained p-value greater than 0.05, 
# then we can conclude that the averages of two groups are significantly similar. 


################################################################################################
################################################################################################

#Two sided paired t-test
ttest = function(alg1, alg2, conf=0.99){

	#applying test
	obj = t.test(alg1, alg2, paired=TRUE, 
		alternative="two.sided", conf.level= conf);
	p.value = obj$p.value;
	alpha = 1 - conf;

	return(p.value < alpha);
}

################################################################################################
################################################################################################

wilcoxon = function(alg1, alg2, conf=0.99){

	obj = wilcox.test(alg1, alg2, paired= TRUE)
	p.value = obj$p.value;
	alpha = 1 - conf;

	return(p.value < alpha);
}


################################################################################################
################################################################################################

# Computing the effect size using Cohen’s statistic.
cohen.stat = function(alg1, alg2){

	d =  abs(mean(alg1) - mean(alg2)) / (sqrt((var(alg1)+var(alg2))/2)) 
	return(d);
}

################################################################################################
################################################################################################

friedman.nemenyi = function(data){

  rankData = c();
  for (i in 1:nrow(data)) {
    rankData = rbind(rankData, rank(data[i, ]))
  }

  rk = colMeans(rankData);
  fried = friedman.test(data);
  nem = posthoc.friedman.nemenyi.test(data);
  return(nem);

}

################################################################################################
################################################################################################
