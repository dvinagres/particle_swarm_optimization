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

# ReLU and Sigmoid
relu = function(x){
  return(pmax(0, x))
}

sigmoid = function(z){
  return(1 / (1 + exp(-z)))
}

# XOR Neural Net
xor.nn = function(input, w1, b1, w2, b2){
  z1 = (input %*% w1) + b1
  a1 = relu(z1)
  
  z2 = (a1 %*% w2) + b2
  a2 = sigmoid(z2)
  
  return(a2)
}




