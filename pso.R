# ---
# Particle Swarm Optimization
# Based on "Particle Swarm Optimization" by J.Kennedy and R.Eberhart
# ---

# -- Target function -- 
target = function(x, y){
  x^2 + y^2
}

# -- Random initialization of particles and velocity -- 
n.particles = 30
rows = 2
upper = 40
lower = 0
coords = matrix(runif(n.particles * rows, lower, upper), nrow=2, ncol=n.particles)
velocity = matrix(runif(n.particles * rows, -2, 2), nrow=2, ncol=n.particles)


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

# Loop
n.iterations = 20

coords.copy = coords
velocity.copy = velocity

n.crazy.particles = 4

for(i in 1:n.iterations){
  argmin = nn.index(coords.copy)
  velocity.copy = velocity.copy[, argmin]
  
  # Add craziness (stochastic variable -> randomly chosen velocities)
  crazy.particles = sample(1:n.particles, n.crazy.particles)
  craziness = matrix(runif(2 * n.crazy.particles, -2, 2), nrow=2, ncol=n.crazy.particles)
  velocity.copy[, crazy.particles] = velocity.copy[, crazy.particles] + craziness
  
  # %% keeps torus structure 
  coords.copy = (coords.copy + velocity.copy) %% upper
  
  plot(coords.copy[1, ], 
       coords.copy[2, ], 
       xlim = c(lower, upper), 
       ylim = c(lower, upper),
       pch = 20, 
       col = "purple")
  
  # Movement arrows
  arrows(x0 = coords.copy[1, ], 
         y0 = coords.copy[2, ], 
         x1 = coords.copy[1, ] + velocity.copy[1, ] * 1.5, 
         y1 = coords.copy[2, ] + velocity.copy[2, ] * 1.5, 
         length = 0.05, 
         col = "red")
  
  Sys.sleep(0.8)
}

# -- Testing Cornfield Vector --
# Evaluation function
evaluation = function(coords){
  presentx = coords[1, ]
  presenty = coords[2, ]
  # Roost located at (20, 20)
  return(sqrt((presentx - 20)^2) + sqrt((presenty - 20)^2))
}

# Update function
update = function(coords, pbestx, pbesty, gbest, g.increment, velocity.matrix){
  presentx = coords[1, ]
  presenty = coords[2, ]
  n.particles = length(presentx)
  
  # Random value for each particle
  random = runif(n.particles, 0, 1)

  velocity.matrix[1, ] = ifelse(presentx > pbestx[gbest], 
                                velocity.matrix[1, ] - random * g.increment, 
                                velocity.matrix[1, ] + random * g.increment)
  
  velocity.matrix[2, ] = ifelse(presenty > pbesty[gbest],
                                velocity.matrix[2, ] - random * g.increment,
                                velocity.matrix[2, ] + random * g.increment)
  
  return(velocity.matrix)
}
