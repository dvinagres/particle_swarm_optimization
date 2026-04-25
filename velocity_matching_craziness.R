# ---
# Nearest Neighbor Velocity Matching and Craziness
# ---

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

# Loop
run.1 = function(coords, velocity, lower, upper, n.iterations, n.crazy.particles = 4){
  n.particles = ncol(coords)
  
  for(i in 1:n.iterations){
    argmin = nn.index(coords)
    velocity = velocity[, argmin]
    
    # Add craziness (stochastic variable -> randomly chosen velocities)
    crazy.particles = sample(1:n.particles, n.crazy.particles)
    craziness = matrix(runif(2 * n.crazy.particles, -2, 2), nrow=2, ncol=n.crazy.particles)
    velocity[, crazy.particles] = velocity[, crazy.particles] + craziness
    
    # %% keeps torus structure 
    coords = (coords + velocity) %% upper
    
    plot(coords[1, ], 
         coords[2, ], 
         xlim = c(lower, upper), 
         ylim = c(lower, upper),
         pch = 20, 
         col = "purple")
    
    # Movement arrows
    arrows(x0 = coords[1, ], 
           y0 = coords[2, ], 
           x1 = coords[1, ] + velocity[1, ] * 1.5, 
           y1 = coords[2, ] + velocity[2, ] * 1.5, 
           length = 0.05, 
           col = "red")
    
    Sys.sleep(0.8)
  }
}

