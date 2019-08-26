
perc.rank <- function(x) trunc(rank(x)/length(x) * 100)

# sample
ids <- sample(1:100000, 10000)


# relative mobility

parent <- runif(100000, 5.0, 7.5)
child <- runif(100000, 5.0, 7.5) + (0.8 * parent)

a <- parent[ids]
b <- child[ids]


pa <- perc.rank(a)
pb <- perc.rank(b)

mean(pa)
mean(pb)

mob <- cor(pb, pa)
lm(pb ~ pa)
mob



pparent <- perc.rank(parent)
pchild <- perc.rank(child)
mean(pparent)
mean(pchild)

mobtot <- cor(child,parent)
mobtot

mean(pchild[ids][which(pparent[ids] == 25)])
50 + mobtot * (25 - 50)

pchild[ids]
m <- lm(pchild ~ pparent)
coeff <- m$coeff
coeff
cor(pchild, pparent)

mean(pparent)
mean(pchild)

# absolute mobility

# mean rank of children (in the national child income distribution) of those children whose parents were at the 25% of the national distribution.

parent_p <- 0.25

