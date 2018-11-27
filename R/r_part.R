library(psych)
library(GPArotation)
library(tidyverse)
library(corrplot)



data(bfi)

# Description of example data
mydata = bfi[1:25]
desp = describe(mydata ) %>%
  select(vars, n, mean, sd, median, min, max, range)
desp

sum(complete.cases(bfi[1:25]))

mydata = bfi[1:25][complete.cases(bfi[1:25]),]
corrplot(cor(mydata), type = "upper", tl.col = "darkblue", tl.srt = 45)

# Checking of adequacy of factor analysis
KMO(mydata)

cortest.bartlett(mydata)



#  Determining the Number of Factors to Extract
scree(mydata, fa = TRUE, pc = FALSE, main = "scree plot")
fa.parallel(mydata, fa = 'fa', error.bars = TRUE)



#  Conducting Factor Analysis
fa4 = fa(mydata, nfactors = 4, fm="pa", max.iter = 100, rotate = "varimax")
fa.diagram(fa4, main = "# factors = 4")


fa6 = fa(mydata, nfactors = 6, fm="pa", max.iter = 100, rotate = "varimax")
fa.diagram(fa6, main = "# factors = 6")


fa5 = fa(mydata, nfactors = 5, fm="pa", max.iter = 100, rotate = "varimax")
fa.diagram(fa5, main = "# factors = 5")


# Detailed analysis with 5 factors
print(fa5$loadings, cutoff=0, digits=3)

print(fa5$communality, cutoff=0, digits=3)

com_matrix = c()
for(i in 1:11){
  result <- fa(mydata, nfactors = i, fm="pa", max.iter = 100, rotate = "varimax")
  com_matrix = rbind(com_matrix, result$communality)
}

as.tibble(com_matrix) %>%  
  gather(items, loadings) %>%
  group_by(items) %>%
  mutate(n = 1:11) %>%
  ggplot(aes(x = n ,y = loadings)) +
  geom_line(aes(colour = factor(items)), size = 0.5) +
  geom_vline(xintercept = 5, linetype="dotted", color = "blue", size=1) +
  geom_vline(xintercept = 6, linetype="dotted", color = "red", size=1) +
  xlab("Number of Factors") + ylab("Communalities")



nfactors = 5
fa5 = fa(mydata, nfactors = nfactors, fm="pa", max.iter = 100, rotate = "varimax")
var_per = 100*fa5$values[1:nfactors]/sum(fa5$values[1:nfactors])
cum_var = cumsum(var_per)
tb = as.data.frame(cbind(Var = var_per, Cum.var = cum_var))
tb %>%
  transmute(Factor = paste0('factor',1:nfactors), 
            Var = sprintf('%4.2f%%', Var),
            Cum.var = sprintf('%4.2f%%', Cum.var))



factor.plot(fa5, labels = rownames(fa5$loadings))







