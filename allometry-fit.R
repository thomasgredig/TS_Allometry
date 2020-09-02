# Author: Thomas Gredig

# load libraries
library(fBasics)
library(ggplot2)

# read data
path.data = 'data'
d = read.csv(file.path(path.data,'data.csv'))
plot(d)

# get fit starting values
d1 = subset(d,X<4.6)
lm(data=d1, Y ~ X)

d1 = subset(d,X>4.6)
lm(data=d1, Y ~ X)

# fitting data with known differential point
nls(data=d,
    Y ~ (N*X+B)*((Sign(X,a=4.6)+1)/2) +
      (M*X+4.6*(N-M)+B)*(1-Sign(X,a=4.6))/2,
    start = list(M=5.2,N=0.22, B=3.8)) -> fit

# add fitted line
d$type = 'data'
xvals = seq(from=4.4,to=4.8, by=0.01)
y = predict(fit, list(X=xvals))
plot(xvals, y)
d1 = data.frame(X = xvals, Y=y, type='fit')

# graph data
ggplot(d, aes(X,Y, color=type)) +
  geom_point(col='black',size=2.5) +
  geom_point(size=2) +
  geom_line(data = d1) +
  theme_bw()

# fit with break-point
# fitting data with known differential point
nls(data=d,
    Y ~ (N*X+B)*((Sign(X,a=k)+1)/2) +
      (M*X+k*(N-M)+B)*(1-Sign(X,a=k))/2,
    start = list(k=4.6, M=5.2,N=0.22, B=3.8)) -> fit
summary(fit)

# add fitted line
xvals = seq(from=4.4,to=4.8, by=0.01)
y = predict(fit, list(X=xvals))
d1 = data.frame(X = xvals, Y=y, type='fit')

# graph data
ggplot(d, aes(X,Y, color=type)) +
  geom_point(col='black',size=2.5) +
  geom_point(size=2) +
  geom_line(data = d1) +
  theme_bw()
ggsave('fig.png')
