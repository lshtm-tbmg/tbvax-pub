library(deSolve)
vx <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # rate of change
    dN <- -N / tau_n
    dV <- N / tau_n - V / tau_v
    dU <- V / tau_v
    # return the rate of change
    list(c(dN, dV, dU))
  }) # end with(as.list ...
}
vx2 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # rate of change
    dA0B0 <- -kAv * A0B0 - kBv * A0B0
    dA0B1 <- -kAv * A0B1 + kBv * A0B0 - kBw * A0B1
    dA0B2 <- -kAv * A0B2 + kBw * A0B1
    dA1B0 <- kAv * A0B0 - kAw * A1B0 - kBv * A1B0
    dA1B1 <- kAv * A0B1 - kAw * A1B1 + kBv * A1B0 - kBw * A1B1
    dA1B2 <- kAv * A0B2 - kAw * A1B2 + kBw * A1B1
    dA2B0 <- kAw * A1B0 - kBv * A2B0
    dA2B1 <- kAw * A1B1 + kBv * A2B0 - kBw * A2B1
    dA2B2 <- kAw * A1B2 + kBw * A2B1
    list(c(dA0B0, dA0B1, dA0B2, dA1B0, dA1B1, dA1B2, dA2B0, dA2B1, dA2B2))
  }) # end with(as.list ...
}

times <- seq(0, 20, by = 0.1)

state <- c(N = 1, V = 0, U = 0)
parameters <- c(tau_n = 3, tau_v = 5)
outVXa <- ode(y = state, times = times, func = vx, parms = parameters)
parameters <- c(tau_n = 2, tau_v = 0.5)
outVXb <- ode(y = state, times = times, func = vx, parms = parameters)

plot(outVXa[, "time"], outVXa[, "N"], type = "l", col = "blue")
lines(outVX2[, "time"], outVXb[, "N"], type = "l", col = "red")
lines(outVX2[, "time"], outVXa[, "V"], type = "l", col = "magenta")
lines(outVX2[, "time"], outVXb[, "V"], type = "l", col = "orange")


parms2 <- c(kAv = 1 / 3, kAw = 1 / 5, kBv = 1 / 2, kBw = 1 / 0.5)
times <- seq(0, 20, by = 0.1)
state2 <- c(A0B0 = 1, A0B1 = 0, A0B2 = 0, A1B0 = 0, A1B1 = 0, A1B2 = 0, A2B0 = 0, A2B1 = 0, A2B2 = 0)

outVX2 <- ode(y = state2, times = times, func = vx2, parms = parms2)
lines(outVX2[, "time"], outVX2[, "A0B0"] + outVX2[, "A0B1"] + outVX2[, "A0B2"], type = "l", col = "darkgreen")
lines(outVX2[, "time"], outVX2[, "A0B0"] + outVX2[, "A1B0"] + outVX2[, "A2B0"], type = "l", col = "purple")
lines(outVX2[, "time"], outVX2[, "A1B0"] + outVX2[, "A1B1"] + outVX2[, "A1B2"], type = "l", col = "darkgreen")
lines(outVX2[, "time"], outVX2[, "A0B1"] + outVX2[, "A1B1"] + outVX2[, "A2B1"], type = "l", col = "purple")
