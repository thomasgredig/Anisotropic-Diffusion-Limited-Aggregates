
################
# 
# set parameters
CONTRAST = 300
path.figs = 'Figs-Generated'
path.results = 'Results-Generated'
master.result.file = 'func.fractal.master.csv'
#
################


library(ggplot2)
library(raster)

# convert matrix to (x,y,z) for
# use in ggplot
#
# m : matrix
matrix.2.xyz <- function(m) {
  N1 = dim(m)[1]
  df = data.frame(
    x = rep(1:N1,each=N1),
    y = rep(1:N1,by=N1),
    z = as.vector(m)
  )
  df
}


# find the number of boxes needed to cover
# a large matrix
#
# scale.e   : scaling factor
# N         : dimensions of original N
# q2        : data matrix (needs to be larger)
num.boxes.to.cover <- function(scale.e, N, q2 ) {
  e = scale.e
  box.size = ceiling(N/e)
  # count, how many boxes too many
  e = e - signif((e*box.size-N)/box.size,0)
  
  h1 = matrix(0, nrow=e, ncol=e)
  # fill out h1 matrix
  for(i in 0:(e-1)) {
    for(j in 0:(e-1)) {
      s1 = sum(q2[i*box.size+(1:box.size),j*box.size+(1:box.size)])
      if (s1>0) { h1[(i+1),(j+1)] = 1 }
    }
  }
  sum(h1)
}

# is it touching the matrix
# returns 0 = not touching, or 1+ = touching
#
# (x,y) : position
# q     : matrix
# isTouching <- function(x,y,q) {
#   x1 = (x %% N) + 1
#   x2 = ((x-2) %% N) + 1
#   y1 = (y %% N) + 1
#   y2 = ((y-2) %% N) + 1
#   q[x1,y1]+q[x1,y2]+q[x2,y1]+q[x2,y2]+q[x,y1]+q[x,y2]+q[x1,y]+q[x2,y]
#   #q[x,y1]+q[x,y2]+q[x1,y]+q[x2,y]
# }


# test the generate.fractal function
#
# l1 = generate.fractal(50)
# str(l1)
# plot(l1[[4]])
#
# N.anisotropy = 1 (isotropic)
#

generate.fractal <- function(N, N.anisotropy = 0, flip.FACTOR = 1, max.iter = 3000) {
  # initialize matrix
  q = matrix(0, nrow=N, ncol=N)
  ## set middle point to 1
  q[N/2, N/2] = 1
  
  gd = read.csv(file.path(path.results,master.result.file))
  
  isTouching <- function(x,y) {
    x1 = (x %% N) + 1
    x2 = ((x-2) %% N) + 1
    y1 = (y %% N) + 1
    y2 = ((y-2) %% N) + 1
    q[x1,y1]+q[x1,y2]+q[x2,y1]+q[x2,y2]+q[x,y1]+q[x,y2]+q[x1,y]+q[x2,y]
    #q[x,y1]+q[x,y2]+q[x1,y]+q[x2,y]
  }
  
  
  # measure time
  start.time = Sys.time()
  nm=c()
  flip.num = sample(c(0,flip.FACTOR), max.iter, replace=TRUE)
  for(j in 1:max.iter) {
    if (flip.num[j]==1) { ## add particle top 
      start.pos = c(sample(1:N,1),1)
    } else { ## add particle left 
      start.pos = c(1,sample(1:N,1))
    }
    
    i=0
    if (N.anisotropy>0) {
      while (isTouching(start.pos[1], start.pos[2])==0) {
        i=i+1
        start.pos[1] = start.pos[1] + pmin(sample(0:N.anisotropy, 1, replace=TRUE),1)
        start.pos[2] = start.pos[2] + sample(-1:1,1, replace=TRUE)
        start.pos = ((start.pos-1) %% N)+1
      }
    } else {
      while (isTouching(start.pos[1], start.pos[2])==0) {
        i=i+1
        start.pos = start.pos + sample(-1:1, 2, replace=TRUE)
        start.pos = ((start.pos-1) %% N)+1
      }
    }
    nm = c(nm, i)
    if (i==0) break
    q[start.pos[1], start.pos[2]] = length(nm) + CONTRAST
  }
  end.time = Sys.time()
  
  # save data to file
  as.data.frame(q) -> df
  fname = paste(substr(Sys.time(), 1,16),' - run=',length(nm),' - N=',N,sep='')
  fname = gsub(':','_',fname)
  write.csv(df, file=file.path(path.results, paste(fname,'.csv',sep='')), row.names = FALSE)
  
  generation.speed.sec = as.numeric(end.time - start.time, units = "secs")
  
  df2 = matrix.2.xyz(q)
  ggplot(df2, aes(x,y,fill=z)) + 
    geom_raster() +
    scale_fill_gradient(low="white", high="blue") +
    ggtitle(fname)
  
  # ggplot(df2, aes(x, y)) +
  #   geom_tile(aes(fill = z), colour = "grey50") +
  #   scale_fill_gradient(low="lightyellow", high="blue") +
  #   ggtitle(fname)
  
  ggsave(file.path(path.figs,paste(fname,'.png',sep='')), width=4,height=3)
  
  r = list(
    N = N,
    fname = fname,
    generation.speed.sec = generation.speed.sec,
    random.walk = nm,
    q = q
    )
  # save to master file
  gd1 = data.frame(
    N = N,
    generation.speed.sec,
    N.anisotropy,
    flip.FACTOR,
    max.iter = length(nm),
    mean.iter = mean(nm),
    fname,
    dim=0,
    dim.sd=0, anisotropy=NA
  )
  gd = rbind(gd, gd1)
  write.csv(gd, file = file.path(path.results, master.result.file), row.names = FALSE)
  return(r)
}



get.fractal.dimension.boxing <- function(q) {
  # make the fractal matrix bigger, so that we don't get out
  # of bounds
  q2 = rbind(q,q)
  q2 = cbind(q2,q2)
  
  # compute the # of boxes needed to cover for each
  # scaling factor e
  result = data.frame()
  N = dim(q)[1]
  for(e in 2:(N/3)) {
    d1 = data.frame(
      e = e,
      Ne = num.boxes.to.cover(e,N, q2),
      e.inv.log = log(1/e)
    )
    d1$Ne.log = log(d1$Ne)
    result = rbind(result, d1)
  }
  # plot(result$e, 1/result$Ne, log='xy')
  # plot(result$e.inv.log, result$Ne.log)
  lm(data = result, Ne.log ~ e.inv.log ) -> fit
  # abline(fit, col='red')
  # summary(fit)$coeff
  # frac.box.dim = summary(fit)$coeff[1]
  # frac.box.dim.sd = summary(fit)$coeff[3]
  # 
  r = list(
      result = result,
      fit = fit
  )
  r
}
