# ---
# Multidimensional Search
# ---

# rand.format is a helper function that returns random values for each particle
# (for individual and general knowledge)
rand.format = function(dimensions, n.particles){
  ind.rand.matrix = matrix(runif(dimensions * n.particles, 0, 1), nrow=dimensions, ncol=n.particles)
  gen.rand.matrix = matrix(runif(dimensions * n.particles, 0, 1), nrow=dimensions, ncol=n.particles)
  
  return(list(ind.rand.matrix = ind.rand.matrix, gen.rand.matrix = gen.rand.matrix))
}

# Mean Squared Error
mse = function(target, pred){
  return(mean((target - pred)^2))
}

# Basically update function from cornfield.R for D x N matrices, 
# where D is any number of dimensions and N is the number of particles
md.update = function(coords, velocity.matrix, pbesti, gbest, p.increment, g.increment){
  dimensions = nrow(coords)
  n.particles = ncol(coords)
  
  # Random values for each particle
  random.values = rand.format(dimensions, n.particles)
  ind.rand = random.values$ind.rand.matrix
  gen.rand = random.values$gen.rand.matrix
  
  # Individual memory
  for(i in 1:dimensions){
    velocity.matrix[i, ] = ifelse(coords[i, ] > pbesti[i, ], 
                                  velocity.matrix[i, ] - ind.rand[i, ] * p.increment,
                                  velocity.matrix[i, ] + ind.rand[i, ] * p.increment)
  }
  
  # Global memory
  for(i in 1:dimensions){
    velocity.matrix[i, ] = ifelse(coords[i, ] > pbesti[i, gbest], 
                                  velocity.matrix[i, ] - gen.rand[i, ] * g.increment,
                                  velocity.matrix[i, ] + gen.rand[i, ] * g.increment)
  }
  
  return(velocity.matrix)
}

# XOR Neural Net
xor.nn = function()




