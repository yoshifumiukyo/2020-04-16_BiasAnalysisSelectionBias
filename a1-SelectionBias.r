############################################################
# R-project                                                #
# Program      : a1-SelectionBias.r                        #
# Protocol     :                                           #
# Date         :                                           #
# Last         :                                           #
# Programmer   : yoshifumi ukyo                            #
#                                                          #
############################################################
# [Ver.0000]                                               #
# Memorandom   :                                           #
#                                                          #
############################################################


#----- clean-up working directory 
rm(list = (ls(all = TRUE)))
#----- library assignment 
base_dir <- ""
setwd(base_dir)
set.seed(4989)


############################################################
# case-control study                                       #
############################################################

a1 <- 45 
a0 <- 94
b1 <- 257 
b0 <- 945 

or <- (a1/a0)/(b1/b0)
varlogor <- 1/a1 + 1/a0 + 1/b1 + 1/b0 
or_low <- exp(log(or) - qnorm(p = 0.975) * sqrt(varlogor)) 
or_upp <- exp(log(or) + qnorm(p = 0.975) * sqrt(varlogor)) 


############################################################
# Bias model 1                                             #
############################################################

a1 <- 45 
a0 <- 94
b1 <- 257 
b0 <- 945 

or_crude <- (a1/a0)/(b1/b0)
log_S_asta <- rnorm(n = 50000, mean = 0, sd = 0.21)
log_OR_adj <- log(or_crude) - log_S_asta
OR_adj <- exp(log_OR_adj)

quantile(x = OR_adj, probs = c(0.025, 0.50, 0.975))

d <- data.frame(or = OR_adj)

library(ggplot2)
p <- ggplot(data = subset(d, or < 10), aes(x = or))
p <- p + theme_bw() 
p <- p + geom_histogram(binwidth = 0.05, colour = "white", fill = "black")
p <- p + scale_x_continuous(limits = c(0.0, 10.0), breaks = seq(from = 0, to = 10, by = 2))
p <- p + scale_y_continuous(limits = c(0, 3000), breaks = seq(from = 0, to = 3000, by = 1000))
p <- p + xlab("Odds-ratio")
p <- p + ylab("Frequency")
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a1-01ORtrue.png"), dpi = 400, plot = p, w = 6, h = 3)



############################################################
# Bias model 2                                             #
############################################################

a1 <- 45 
a0 <- 94
b1 <- 257 
b0 <- 945 

or_crude <- (a1/a0)/(b1/b0)
log_S_asta <- rnorm(n = 50000, mean = 0, sd = 0.21)
epi <- rnorm(n = 50000, mean = 0, sd = 0.1944)
log_OR_adj <- log(or_crude) - log_S_asta + epi
OR_adj <- exp(log_OR_adj)

quantile(x = OR_adj, probs = c(0.025, 0.50, 0.975))

d <- data.frame(or = OR_adj)

library(ggplot2)
p <- ggplot(data = subset(d, or < 10), aes(x = or))
p <- p + theme_bw() 
p <- p + geom_histogram(binwidth = 0.05, colour = "white", fill = "black")
p <- p + scale_x_continuous(limits = c(0.0, 10.0), breaks = seq(from = 0, to = 10, by = 2))
p <- p + scale_y_continuous(limits = c(0, 3000), breaks = seq(from = 0, to = 3000, by = 1000))
p <- p + xlab("Odds-ratio")
p <- p + ylab("Frequency")
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a1-02ORtrueErr.png"), dpi = 400, plot = p, w = 6, h = 3)



