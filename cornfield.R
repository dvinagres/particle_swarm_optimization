# -- Cornfield Vector --

# Evaluation function
evaluation = function(coords){
  presentx = coords[1, ]
  presenty = coords[2, ]
  # Roost located at (20, 20)
  return(sqrt((presentx - 20)^2) + sqrt((presenty - 20)^2))
}

# Update function
update = function(coords, pbestx, pbesty, gbest, p.increment, g.increment, velocity.matrix){
  presentx = coords[1, ]
  presenty = coords[2, ]
  n.particles = length(presentx)
  
  # Random value for each particle
  rand.ix = runif(n.particles, 0, 1)
  rand.iy = runif(n.particles, 0, 1)
  rand.gx = runif(n.particles, 0, 1)
  rand.gy = runif(n.particles, 0, 1)
  
  # Individual memory
  velocity.matrix[1, ] = ifelse(presentx > pbestx, 
                                velocity.matrix[1, ] - rand.ix * p.increment, 
                                velocity.matrix[1, ] + rand.ix * p.increment)
  
  velocity.matrix[2, ] = ifelse(presenty > pbesty, 
                                velocity.matrix[2, ] - rand.iy * p.increment, 
                                velocity.matrix[2, ] + rand.iy * p.increment)
  
  # Global memory
  velocity.matrix[1, ] = ifelse(presentx > pbestx[gbest], 
                                velocity.matrix[1, ] - rand.gx * g.increment, 
                                velocity.matrix[1, ] + rand.gx * g.increment)
  
  velocity.matrix[2, ] = ifelse(presenty > pbesty[gbest],
                                velocity.matrix[2, ] - rand.gy * g.increment,
                                velocity.matrix[2, ] + rand.gy * g.increment)
  
  return(velocity.matrix)
}

# Loop
run.2 = function(coords, velocity, lower, upper, n.iterations, p.increment=1, g.increment=1){
  # Initialize values
  # pbest keeps the best individual value and gbest the index of the global best
  # pbestx and pbesty keep the best x and y coords individually
  pbest = evaluation(coords)
  gbest = which.min(pbest)
  pbestx = coords[1, ]
  pbesty = coords[2, ]
  
  for(i in 1:n.iterations){
    velocity = update(coords, pbestx, pbesty, gbest, p.increment, g.increment, velocity)
    coords = (coords + velocity) %% upper
    
    current.pbest = evaluation(coords)
    
    # Check if current.pbest is better than pbest
    better = which(current.pbest < pbest)
    
    # Update values
    pbest[better] = current.pbest[better]
    pbestx[better] = coords[1, better]
    pbesty[better] = coords[2, better]
    
    # Update gbest
    gbest = which.min(pbest)
    
    plot(coords[1, ], 
         coords[2, ], 
         xlim = c(lower, upper), 
         ylim = c(lower, upper),
         pch = 20, 
         col = "purple")
    
    # Roost
    points(20, 20, pch = 19, cex = 2, col = "violet")
    
    Sys.sleep(0.5)
  }
}
