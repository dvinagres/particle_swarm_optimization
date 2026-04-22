# Particle Swarm Optimization
# Based on "Particle Swarm Optimization" by J.Kennedy and R.Eberhart (1995)

# Target function
target = function(x, y){
  x^2 + y^2
}

# Random initialization of particles
n.particles = 30
rows = 2
upper = 40
lower = 2
coords = matrix(runif(n.particles * rows, lower, upper), nrow=2, ncol=n.particles)
#plot(as.vector(coords))



       