library(raster)
source('func.fractals.R') 
N1 = 100
l1 = generate.fractal(N1)
str(l1)
l1['generation.speed.sec'][[1]]

q1 = l1['q'][[1]]
plot(raster(q1))

# speed test
r = data.frame()
for(N1 in c(10,20,30,40,50,70,100,120)) {
  l1 = generate.fractal(N1) 
  r1 = data.frame(
    N = N1,
    speed = l1['generation.speed.sec'][[1]],
    filename = l1['fname'][[1]]
  )
  r = rbind(r, r1)
}
r
plot(r$N, r$speed, log='xy', col='red', pch=20, ylab='time (s)', xlab='N')
r


library(raster)
q = read.csv(file.path(path.results,paste(l1[[2]],'.csv',sep='')))
q = as.matrix(q)
plot(raster(q))
l2 = get.fractal.dimension.boxing(q)
str(l2)
nm = l2['result'][[1]]
str(nm)
ggplot(nm, aes(e.inv.log, Ne.log)) + geom_point()
fit = l2['fit'][[1]]
summary(fit)
plot(log(1/nm$e), log(nm$Ne))



###

source('func.fractals.R')
N1 = 150

for(k in 1:4) {
  print(k)
  l1 = generate.fractal(N1, N.anisotropy=0, flip.FACTOR=1)
  plot(raster(l1[['q']]))
  l1 = generate.fractal(N1, N.anisotropy=20, flip.FACTOR=1)
  plot(raster(l1[['q']]))
}

str(l1)

table(gd$anisotropy)
