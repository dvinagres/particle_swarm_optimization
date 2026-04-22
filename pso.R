# ---
# Particle Swarm Optimization
# Based on "Particle Swarm Optimization" by J.Kennedy and R.Eberhart
# ---

# -- Target function -- 
target = function(x, y){
  x^2 + y^2
}

# -- Random initialization of particles -- 
n.particles = 30
rows = 2
upper = 40
lower = 2
coords = matrix(runif(n.particles * rows, lower, upper), nrow=2, ncol=n.particles)
#plot(as.vector(coords))


# -- Testing Nearest Neighbor Velocity Matching and Craziness -- 
# nn.index returns a vector containing the index of the nearest neighbor for each particle
nn.index = function(coord.matrix){
  rows = ncol(coord.matrix)
  cols = ncol(coord.matrix)
  
  dist.matrix = matrix(0, nrow=rows, ncol=cols)
  argmin = c()
  
  # Euclidean distance
  for(i in 1:cols){
    for(j in 1:cols){
      if(i==j){
        dist.matrix[i, j] = Inf
      } else{
        dist.matrix[i, j] = ((coord.matrix[1, i] - coord.matrix[1, j])^2 + (coord.matrix[2, i] - coord.matrix[2, j])^2)^(1/2)
      }
    }
    # Argmin of current row
    argmin[i] = which.min(dist.matrix[i, ])
  }

  return(argmin)
}



