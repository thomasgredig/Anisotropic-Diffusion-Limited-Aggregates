# use ggplot to draw a matrix
# using rastering

# converts the square matrix m into
# a dataframe with (x,y,z)
matrix.2.xyz <- function(m) {
  N1 = dim(m)[1]
  df = data.frame(
    x = rep(1:N1,each=N1),
    y = rep(1:N1,by=N1),
    z = as.vector(m)
  )
  df
}

# for testing
#
# 
# N = 8
# m1 = matrix(1:(N*N), nrow=N)
# m1
# 
# df = matrix.2.xyz(m1)
# names(df)
# df
# 
# library(ggplot2)
# ggplot(df, aes(x,y)) + 
#   geom_raster(aes(fill=z))


