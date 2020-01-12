library(ggplot2)
library(raster)
source('func.fractals.R')

################
# 
# set parameters
N = 50
max.iter = 1000
CONTRAST = 300
#
################

path.figs = 'Figs-Generated'
path.results = 'Results-Generated'


q = matrix(0, nrow=N, ncol=N)

## set middle point to 1
q[N/2, N/2] = 1

## plot matrix

plot(raster(q))



start.time = Sys.time()
nm=c()
flip.num = sample(c(0,1), max.iter, replace=TRUE)
for(j in 1:max.iter) {
  ## add particle top middle
  if (flip.num[j]==0) {
    start.pos = c(sample(1:N,1),1)
  } else {
    start.pos = c(1,sample(1:N,1))
  }
  
  i=0
  while (isTouching(start.pos[1], start.pos[2] )==0) {
    i=i+1
    start.pos = start.pos + sample(c(-1,0,1), 2, replace=TRUE)
    start.pos = ((start.pos-1) %% N)+1
  }
  nm = c(nm, i)
  if (i==0) break
  q[start.pos[1], start.pos[2]] = length(nm) + CONTRAST
  if ((length(nm) %% 200) == 0) {
    plot(raster(q)) 
  }
}
end.time = Sys.time()


as.data.frame(q) -> df
fname = paste(substr(Sys.time(), 1,16),' - run=',length(nm),' - N=',N,sep='')
fname = gsub(':','_',fname)
write.csv(df, file=file.path(path.results, paste(fname,'.csv',sep='')), row.names = FALSE)

plot(nm, log='y')
print(end.time - start.time)
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
