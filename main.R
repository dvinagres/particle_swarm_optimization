# ---
# Particle Swarm Optimization
# Based on "Particle Swarm Optimization" by J.Kennedy and R.Eberhart (1995)
# ---

source("velocity_matching_craziness.R")
source("cornfield.R")
source("xor_neural_net.R")

# -- Random initialization of particles and velocity -- 
n.particles = 30
rows = 2
upper = 40
lower = 0
coords = matrix(runif(n.particles * rows, lower, upper), nrow=2, ncol=n.particles)
velocity = matrix(runif(n.particles * rows, -2, 2), nrow=2, ncol=n.particles)

# -- Testing Nearest Neighbor Velocity Matching and Craziness --
run.1(coords, velocity, lower, upper, n.iterations=20)

# -- Testing Cornfield Vector --
run.2(coords, velocity, lower, upper, n.iterations=50)

# -- Testing Multidimensional Search --
input = matrix(c(0, 0, 0, 1, 1, 0, 1, 1), nrow = 4, byrow = TRUE)
target = matrix(c(0, 1, 1, 0), nrow = 4)

dimensions = 13
n.particles = 30
upper = 2
lower = -2
coords = matrix(runif(dimensions * n.particles, lower, upper), nrow=dimensions, ncol=n.particles)
velocity = matrix(runif(dimensions * n.particles, lower, upper), nrow=dimensions, ncol=n.particles)

n.iterations = 100

best.particle = run.3(coords, velocity, lower, upper, n.iterations, p.increment=1, g.increment=1, input, target)

