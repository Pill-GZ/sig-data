data <- read.csv(file = "stockdata3.csv")

plot(data$a, type = 'l')
plot(data$b, type = 'l')
plot(data$c, type = 'l')
plot(data$d, type = 'l')
plot(data$e, type = 'l')
plot(data$f, type = 'l')


#### clean data$a ####

boxplot(data[letters[1:6]])

which(is.na(data$a))
which(data$a == 0)

plot(is.na(data$a), type = 'l')
plot(data$a == 0, type = 'l')

mean(is.na(data$a))
mean(is.na(data$b))
mean(is.na(data$c))
mean(is.na(data$d))
mean(is.na(data$e))
mean(is.na(data$f))

sum(data$a == 0, na.rm = T) / nrow(data)
sum(data$b == 0 | data$b == 1, na.rm = T) / nrow(data)
sum(data$c == 0 | data$c == 1, na.rm = T) / nrow(data)
sum(data$d == 1, na.rm = T) / nrow(data)
sum(data$e == 0 | data$e == 1, na.rm = T) / nrow(data)
sum(data$f == 0 | data$f == 1, na.rm = T) / nrow(data)

min(data$d, na.rm = T)

#### annualized percent return ####

plot(data$a, type = 'l')
points(which(is.na(data$a)), data$a[which(is.na(data$a))-1], col = 2)
plot(log(data$a), type = 'l')
plot(diff(log(data$a)), type = 'l')
r <- diff(log(data$a))
summary(r)
sum(r==0, na.rm = T)

# ACF won't calculate if when there are 0 values
# 1. Convert 0's into missings
# 2. Fill missing/0's with last non-missing data
# 3. Splice data pre- and post-missing data

#### 1 ############################################################

a_missing <- data$a
mean(a_missing == 0, na.rm = T)
a_missing[a_missing == 0] <- NA

plot(a_missing, type = 'l')
plot(log(a_missing), type = 'l')
plot(diff(log(a_missing)), type = 'l')
acf(diff(log(a_missing)), lag.max = 500, na.action = na.pass)

#### 2 #############################################################

a_fill <- data$a
a_fill[a_fill == 0] <- NA

# # extrat indices of missing values
# NA.idx <- which(is.na(a_fill))
# NA.idx.is.start <- !is.na(a_fill[which(is.na(a_fill))-1])
# NA.idx.is.end <- !is.na(a_fill[which(is.na(a_fill))+1])
# NA.idx.start <- NA.idx[NA.idx.is.start]
# NA.idx.end <- NA.idx[NA.idx.is.end]
# # fill missing indices with last non-missing data
# NA.groups <- apply(X = cbind(NA.idx.start, NA.idx.end), MARGIN = 1, 
#                    FUN = function(x){seq(x[1], x[2])})
# for (group in NA.groups) {
#   a_fill[group] <- a_fill[min(group) - 1]
# }

# or more succintly
while (anyNA(a_fill)) {
  a_fill[is.na(a_fill)] <- a_fill[which(is.na(a_fill))-1]
}

plot(a_fill, type = 'l')
plot(log(a_fill), type = 'l')
plot(diff(log(a_fill)), type = 'l')
acf(diff(log(a_fill)), lag.max = 500, na.action = na.pass)

# lag-1 scatter plot
plot(diff(log(a_fill[(t*9/10):t])), diff(log(a_fill[(t*9/10):t - 1])))

# vol over time
par(mar = c(3,4,1,1))
plot(data$timestr[2:t], diff(log(a_fill)), pch = 20, cex = 0.5, 
     xaxt = 'n', las = 1)
# plot(data$timestr[2:t], diff(log(a_fill)), pch = 20, cex = 0.5, 
#      ylim = c(-0.01, 0.01), xaxt = 'n', las = 1)
axis(side = 1, at = seq(1, nlevels(data$timestr), 30), las = 1,
     labels = substr(levels(data$timestr)[seq(1, nlevels(data$timestr), 30)], start = 1, stop = 5))



#### 3 splice ################################################

# not done

#### b ####

plot(data$b, type = 'l')
points(which(is.na(data$b)), data$b[which(is.na(data$b))-1], col = 2)
# plot(diff(data$b), type = 'l')
plot(diff(log(data$b)), type = 'l')
plot(density(diff(data$b), na.rm = T), log ='y')
abline(v = -10:10/10, lty = 2)
plot(density(diff(log(data$b)), na.rm = T), log ='y')
abline(v = -10:10/100, lty = 2)

t <- nrow(data)
acf(diff(log(data$b[(t/2):t])))
acf(diff(log(data$b[1:(t/2)])))
acf(diff(log(data$b[1:t])))

# lag-1 scatter plot
plot(diff(data$b[(t*9/10):t]), diff(data$b[(t*9/10):t - 1]), pch = '.', cex = 2)
plot(diff(log(data$b[(t*9/10):t])), diff(log(data$b[(t*9/10):t - 1])), pch = '.', cex = 2)

#### c ####

plot(data$c, type = 'l')
points(which(is.na(data$c)), data$c[which(is.na(data$c))-1], col = 2)
plot(diff(data$c), type = 'l')
plot(diff(log(data$c)), type = 'l')
plot(density(diff(data$c), na.rm = T), log ='y')
plot(density(diff(log(data$c)), na.rm = T), log ='y')

t <- nrow(data)
acf(diff(log(data$c[1:t])), na.action = na.pass)
acf(diff(log(data$c[(t/2):t])), na.action = na.pass)
acf(diff(log(data$c[1:(t/2)])), na.action = na.pass)

# lag-1 scatter plot
plot(diff(data$c[(t*9/10):t]), diff(data$c[(t*9/10):t - 1]), pch = '.', cex = 2)
plot(diff(log(data$c[(t*9/10):t])), diff(log(data$c[(t*9/10):t - 1])), pch = '.', cex = 2)

#### d ####

plot(data$d, type = 'l')
points(which(is.na(data$d)), data$d[which(is.na(data$d))-1], col = 2)

d_fill <- data$d
d_fill[d_fill == 1] <- NA

while (anyNA(d_fill)) {
  d_fill[is.na(d_fill)] <- d_fill[which(is.na(d_fill))-1]
}

plot(d_fill, type = 'l')
plot(diff(d_fill), type = 'l')
plot(diff(log(d_fill)), type = 'l')
plot(density(diff(d_fill), na.rm = T), log ='y', xlim = c(-10, 10))
abline(v = -10:10, lty = 2)
plot(density(diff(log(d_fill)), na.rm = T), log ='y')
plot(density(diff(log(d_fill)), na.rm = T))

t <- nrow(data)
acf(diff(log(d_fill[1:t])), na.action = na.pass)
acf(diff(log(d_fill[(t/2):t])), na.action = na.pass)
acf(diff(log(d_fill[1:(t/2)])), na.action = na.pass)

acf(diff(log(d_fill[(t*9/10):t])), na.action = na.pass)
plot(diff(log(d_fill[(t*9/10):t])), type = 'l')
abline(h = c(-0.1, 0.1), lty = 2)
plot(density(diff(log(d_fill[(t*9/10):t])), na.rm = T), log = 'y')
plot(density(diff(log(d_fill[(t*9/10):t])), na.rm = T), ylim = c(0, 5))

# lagged scatter plot
# lag 1 diff
plot(diff(log(d_fill[(t*9/10):t - 1])), diff(log(d_fill[(t*9/10):t])))
plot(diff(d_fill[(t*9/10):t - 1]), diff(d_fill[(t*9/10):t]))

plot(diff(d_fill[(t*1/10):t - 1]), diff(d_fill[(t*1/10):t]), pch = 20, 
     col = ifelse(abs(diff(d_fill[(t*1/10):t - 2])) < 0.1, 2, 1))
# lag 2 diff
plot(diff(log(d_fill[(t*9/10):t - 2])), diff(log(d_fill[(t*9/10):t])))
# lag 1 marginal
plot(log(d_fill[(t*9/10):t - 1]), log(d_fill[(t*9/10):t]))
abline(a = 0, b = 1)
# lagged scatter plot - micro scale
plot(diff(log(d_fill[(t*9/10):t - 1])), diff(log(d_fill[(t*9/10):t])), 
     pch = '.', cex = 2, xlim = c(-0.005, 0.005), ylim = c(-0.005, 0.005))
plot(diff(log(d_fill[(t*9/10):t - 2])), diff(log(d_fill[(t*9/10):t])), 
     pch = '.', cex = 2, xlim = c(-0.005, 0.005), ylim = c(-0.005, 0.005))

# vol over time
par(mar = c(3,4,1,1))
plot(data$timestr[2:t], diff(log(d_fill)), pch = 20, cex = 0.5, 
     ylim = c(-0.01, 0.01), xaxt = 'n', las = 1)
axis(side = 1, at = seq(1, nlevels(data$timestr), 30), las = 1,
     labels = substr(levels(data$timestr)[seq(1, nlevels(data$timestr), 30)], start = 1, stop = 5))

#### e ####

plot(data$e, type = 'l')
which(is.na(data$e))
# plot(diff(data$e), type = 'l')
plot(diff(log(data$e)), type = 'l')
plot(density(diff(log(data$e)), na.rm = T), log ='y')
plot(density(diff(log(data$e)), na.rm = T))

t <- nrow(data)
acf(diff(log(data$e[1:t])), na.action = na.pass)
acf(diff(log(data$e[(t/2):t])), na.action = na.pass)
acf(diff(log(data$e[1:(t/2)])), na.action = na.pass)

acf(diff(log(data$e[(t*9/10):t])), na.action = na.pass)
plot(diff(log(data$e[(t*9/10):t])), type = 'l')
plot(density(diff(log(data$e[(t*9/10):t])), na.rm = T), log = 'y')
plot(density(diff(log(data$e[(t*9/10):t])), na.rm = T), ylim = c(0, 5))

# lagged scatter plot
plot(diff(log(data$e[(t*9/10):t - 1])), diff(log(data$e[(t*9/10):t])))
plot(diff(data$e[(t*9/10):t - 1]), diff(data$e[(t*9/10):t]))
plot(diff(log(data$e[(t*9/10):t - 2])), diff(log(data$e[(t*9/10):t])))
plot(log(data$e[(t*99/100):t - 1]), log(data$e[(t*99/100):t]))
abline(a = 0, b = 1)

#### f ####

plot(data$f, type = 'l')
points(which(is.na(data$f)), data$f[which(is.na(data$f))-1], col = 2)

f_fill <- data$f
while (anyNA(f_fill)) {
  f_fill[is.na(f_fill)] <- f_fill[which(is.na(f_fill))-1]
}

plot(f_fill, type = 'l')
plot(diff(f_fill), type = 'l')
plot(diff(log(f_fill)), type = 'l')
plot(density(diff(f_fill), na.rm = T), log ='y')
abline(v = -10:10, lty = 2)
plot(density(diff(log(f_fill)), na.rm = T), log ='y')
plot(density(diff(log(f_fill)), na.rm = T))

t <- nrow(data)
acf(diff(log(f_fill[1:t])), lag.max = 500, na.action = na.pass)
acf(diff(log(f_fill[(t/2):t])), lag.max = 500, na.action = na.pass)
acf(diff(log(f_fill[1:(t/2)])), lag.max = 500, na.action = na.pass)

acf(diff(log(f_fill[(t*9/10):t])), na.action = na.pass)
plot(diff(log(f_fill[(t*9/10):t])), type = 'l')
plot(density(diff(log(f_fill[(t*9/10):t])), na.rm = T), log = 'y')
plot(density(diff(log(f_fill[(t*9/10):t])), na.rm = T), ylim = c(0, 5))

# lagged scatter plot
plot(diff(log(f_fill[(t*9/10):t - 1])), diff(log(f_fill[(t*9/10):t])))
plot(diff(f_fill[(t*9/10):t - 1]), diff(f_fill[(t*9/10):t]))
plot(diff(log(f_fill[(t*9/10):t - 2])), diff(log(f_fill[(t*9/10):t])))
plot(log(f_fill[(t*9/10):t - 1]), log(f_fill[(t*9/10):t]))
abline(a = 0, b = 1)
# lagged scatter plot - micro scale
plot(diff(log(f_fill[(t*9/10):t - 1])), diff(log(f_fill[(t*9/10):t])), 
     pch = '.', cex = 2, xlim = c(-0.005, 0.005), ylim = c(-0.005, 0.005))
# plot(diff((f_fill[(t*9/10):t - 1])), diff((f_fill[(t*9/10):t])), c  ol = 2,
#      pch = '.', cex = 4, xlim = c(-0.05, 0.05), ylim = c(-0.05, 0.05))
# abline(v = -10:10/100, h = -10:10/100, lty = 2)
plot(diff(log(f_fill[(t*9/10):t - 2])), diff(log(f_fill[(t*9/10):t])), 
     pch = '.', cex = 2, xlim = c(-0.005, 0.005), ylim = c(-0.005, 0.005))

# high proportion of zeros
sum(diff(log(f_fill)) == 0) / t

########## time #####################

setdiff(1:362, union(unique(data$day), 7*rep(1:52, each = 2) + c(-1, 0)))

levels(data$timestr)


par(mar = c(3,4,1,1))
plot(data$timestr[2:t], diff(log(d_fill)), pch = 20, cex = 0.5, 
     ylim = c(-0.01, 0.01), xaxt = 'n', las = 1)
axis(side = 1, at = seq(1, nlevels(data$timestr), 30), las = 1,
     labels = substr(levels(data$timestr)[seq(1, nlevels(data$timestr), 30)], start = 1, stop = 5))

plot(data$timestr[(t*9/10):(t-1)], diff(log(f_fill[(t*9/10):t])))

