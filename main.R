# ---
# Particle Swarm Optimization
# Based on "Particle Swarm Optimization" by J.Kennedy and R.Eberhart (1995)
# ---

source("velocity_matching_craziness.R")
source("cornfield.R")

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
