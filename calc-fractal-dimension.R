# calculate dimension
# using boxing method

# https://en.wikipedia.org/wiki/Fractal_dimension
#
# D = lim (e->0) = Log N / log (1/e)
#
# how many boxes do we need to cover all
#
# e is the scaling factor, N = number of boxes with something inside
#
# e = 1, then it's the entire picture,
# e = 2, then we have 4 boxes, etc.

source('matrix.2.xyz.R')
path.results = 'Results-Generated'
file.list = dir(path.results, pattern='N=50')

df = read.csv(file.path(path.results,file.list[1]))
q = as.matrix(df)
N = dim(q)[1]
plot(raster(q))
q2 = rbind(q,q)
q2 = cbind(q2,q2)
plot(raster(q2))


# calculate dimension
e = 6
box.size = ceiling(N/e)

# count, how many boxes too many
e = e - signif((e*box.size-N)/box.size,0)

h1 = matrix(0, nrow=e, ncol=e)
# fill out h1 matrix
for(i in 0:(e-1)) {
  for(j in 0:(e-1)) {
    print(paste(i*box.size+(1)," to ",i*box.size+(box.size)))
    s1 = sum(q2[i*box.size+(1:box.size),j*box.size+(1:box.size)])
    if (s1>0) { h1[(i+1),(j+1)] = 1 }
  }
}

h1
plot(raster(q))
plot(raster(h1))

# df = matrix.2.xyz(q)
# df2 = matrix.2.xyz(h1)
# 
# 
# ggplot(df, aes(x, y)) +
#   geom_tile(aes(fill = z), colour = "grey50") +
#   scale_fill_gradient(low="white", high="blue") +
#   geom_tile(data=df2, alpha=0.2)
# h1

## get dimension


result = data.frame()
for(e in 4:N) {
  d1 = data.frame(
    e = e,
    Ne = num.boxes.to.cover(e,N, q2),
    e.inv.log = log(1/e)
  )
  d1$Ne.log = log(d1$Ne)
  result = rbind(result, d1)
}
# plot(result$e, 1/result$Ne, log='xy')
plot(result$e.inv.log, result$Ne.log)
lm(data = result, Ne.log ~ e.inv.log ) -> fit
abline(fit, col='red')
summary(fit)$coeff
frac.box.dim = summary(fit)$coeff[1]
frac.box.dim.sd = summary(fit)$coeff[3]



## run this portion in the background
## to update the dimensions file
gd = read.csv(file.path(path.results, master.result.file), stringsAsFactors = FALSE)
table(gd$anisotropy)
# load all files, compute dimensions, if not yet computed
for(i in 1:nrow(gd)) {
  if (!is.na(gd$dim[i])) {
    if (gd$dim[i]==0) {
      q = read.csv(file.path(path.results,paste(gd$fname[i],'.csv',sep='')))
      l2 = get.fractal.dimension.boxing(q)
      fit2 = l2[['fit']]
      print(summary(fit2)$coef[2])
      gd$dim[i] = summary(fit2)$coef[2]
      gd$dim.sd[i] = summary(fit2)$coef[4]
    }
  }
}
gd$anisotropy = factor(gd$N.anisotropy)
write.csv(gd, file = file.path(path.results, master.result.file), row.names = FALSE)
gd1 = subset(gd, N>=100)
table(gd1$anisotropy)
ggplot(gd1, aes(anisotropy, -dim)) + 
  geom_boxplot(fill = 'orange') +
  theme_bw()

names(gd)
ggplot(gd1, aes(mean.iter, -dim, color=anisotropy)) + geom_point()

ggplot(gd1, aes(N.anisotropy, -dim, color=anisotropy, size=N)) + geom_point() +
  geom_errorbar(aes(ymax=-dim+dim.sd, ymin=-dim-dim.sd))


ggplot(gd, aes(N, generation.speed.sec)) + geom_point()
