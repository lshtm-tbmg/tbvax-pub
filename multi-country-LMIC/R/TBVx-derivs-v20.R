# NOTE: large parts of this code should be rewritten to take advantage of FP principles and reduce duplication of code

aging.and.births.and.update.contact.matrix.event <- function(t, X, p) {
  # cat("event at ",signif(t,15),"\n")
  if (t > p$run.params$simulation$years[1]) {
    Y <- matrix(X, p$nVXaSESRISKHIVTB, p$nAGES)
    rownames(Y) <- p$VXaSESRISKHIVTB
    colnames(Y) <- names(p$AGES)
    if (t >= p$run.params$rescale.pop.yr & !p$run.params$rescaled.pop) {
      Y <- scale.alive.population(Y, p)
      p$run.params$rescaled.pop <- T
    }
    alive.by.age.group <- colSums(Y[p$ALIVE, ])
    if (p$contacts$defined) {
      p$contacts$M <- update.contact.matrix(p, alive.by.age.group)
    }
    dY <- t(tcrossprod(p$aging.matrix, Y))
    dY[!p$ALIVE, ] <- 0. # the dead do not age
    dY[, 1] <- dY[, 1] + p$birthrate(t) * sum(alive.by.age.group) * p$at.birth
    Y <- Y + dY
    # allow deaths to accumulate over time
    # alternative:
    Y[!p$ALIVE, ] <- 0.
    dY <- 0 * Y

    for (i in seq_along(p$inci$TB)) {
      if (p$inci$TB[[i]]$inci.once.per.year) {
        dY <- dY + derivs.inci(t, Y, p, p$inci$TB[[i]], T)
      }
    }
    for (i in seq_along(p$inci$HIV)) {
      if (p$inci$HIV[[i]]$inci.once.per.year) {
        dY <- dY + derivs.inci(t, Y, p, p$inci$HIV[[i]], T)
      }
    }
    for (i in seq_along(p$inci$VXa)) {
      if (p$inci$VXa[[i]]$inci.once.per.year) {
        dY <- dY + derivs.inci(t, Y, p, p$inci$VXa[[i]], T)
      }
    }

    Y <- Y + dY

    if (sum(is.nan(Y)) > 0) {
      modlog(level = "FATAL", msg = paste0(" at t = ", t, " NaNs in state variables ; reduce euler / rk2 / rk4 time step"))
    } else if (sum(Y < 0.) > 0) {
      neg <- p$run.params$num.int$min.value.for.state.var
      Y[Y < 0. & Y >= neg] <- 0.
      if (sum(Y < neg) > 0) {
        idx <- which(Y < neg)
        r <- idx %% nrow(Y)
        c <- (idx %/% nrow(Y)) + 1
        for (i in seq_along(idx)) {
          modlog(level = "FATAL", paste0(
            " at t = ", t, " state variables < ", neg, " :",
            " state = ", rownames(Y)[r[i]], " ; age group = ", colnames(Y)[c[i]], "; value = ", signif(Y[r[i], c[i]]), " ; please reconsider parameter values, incidence data or setting min.value.for.state.var to a more negative value"
          ))
        }
        if (!(is.null(p$paths$parameters) | is.na(p$paths$parameters))) {
          if (!dir.exists(file.path(p$paths$output.dir, "weird"))) {
            dir.create(file.path(p$paths$output.dir, "weird"))
          }
          outfname <- paste0(create.filename(paste0(p$paths$output.dir, "/weird"), p, runtype = "weird"), ".csv")
          df <- data.frame(unique.name = model$modified.params$unique.name, value = model$modified.params$mean)
          write.csv(df, outfname, row.names = F)
          return(NULL)
        }
        assert_that(sum(Y < neg) == 0, msg = paste("state variable <", neg))
      }
    }
    return(as.vector(Y))
  } else {
    return(X)
  }
}

derivs.Tr <- function(t, Y, Tr, fn) {
  tparams <- Tr$timed.parameters
  if (is.null(tparams)) {
    return(fn(Tr, Y))
  } else {
    mult <- 1.0
    nms <- names(tparams)
    for (nm in nms) {
      mult <- mult * get(nm, envir = tparams)(t)
    }
    return(fn(Tr, Y, mult))
  }
}

foi <- function(t, Y, parms) {
  I_ <- matmul.by.age.group(parms$Infc, Y) # always > 0
  mult <- 1.0
  tparams <- parms$Tm$timed.parameters
  if (!is.null(tparams)) {
    nms <- names(tparams)
    for (nm in nms) {
      mult <- mult * get(nm, envir = tparams)(t)
    }
  }
  if (parms$contacts$defined) {
    i_ <- colSums(I_) / colSums(Y[parms$ALIVE, ])
    i_[is.nan(i_)] <- 0.
    return(as.numeric(mult * parms$DAYSPERYEAR * parms$contacts$M %*% i_)) # return the foi ; foi is a column vector of foi by age
    # NOTE: this may seem incorrect as originally the contactees are in the columns of the contact matrix read
    #       however adjusting the contact matrix takes care of this ... the new contact matrix M depends on M plus t(M) anyway
    #       and the adjustment takes care of balancing i.e. the number of individuals in a row age group (i.e. susc) multiplied with an
    #       off-diagonal contact rate and the number of individuals in that column balances that same number mirrored wrt the diagonal
    #       effectively, the rows now contain the contactees (susceptibles) and therefore the foi is calculated correctly
  } else {
    avg.i_ <- sum(I_) / sum(Y[parms$ALIVE, ]) # i.e. the average fraction infectious
    i_ <- rep(avg.i_, ncol(Y))
    return(as.numeric(mult * parms$DAYSPERYEAR * i_)) # return the foi ; foi is a column vector with identical average fraction infectious times mult and DAYSPERYEAR i.e. contact rate = 1 / day
  }
}

derivs.Tm <- function(t, Y, parms, fn) {
  foi <- foi(t = t, Y = Y, parms = parms)
  susc <- fn(parms$Tm, Y)
  if (is.list(susc)) {
    dY.in <- t(t(susc$dY.in) * foi)
    dY.out <- t(t(susc$dY.out) * foi)
    return(list(dY = (dY.in + dY.out), dY.in = dY.in, dY.out = dY.out))
  } else {
    return(t(t(susc) * foi))
  }
}

derivs.inci.common <- function(t = NA, Y = NA, p = NA, p.inci = NA) {
  dX <- NULL
  x_inci <- as.numeric(p.inci$inci[p.inci$inci.fn(t), ])
  if (sum(x_inci) > 0) {
    if (!is.null(p.inci$inci.trend.fn)) {
      x_inci <- x_inci * p.inci$inci.trend.fn(t)
    }
    X <- t(Y)
    dX <- 0 * X
    if (!p.inci$inci.proportions) {
      if (p.inci$inci.denominator == "all") {
        denom <- rowSums(X[, p.inci$susc])
        denom <- sapply(denom, max, 1e-6)
        scaleby <- rowSums(X[, p.inci$alive]) / denom
        scaleby <- sapply(sapply(scaleby, max, 1.), min, 10)
      } else {
        scaleby <- 1.0
      }
    } else {
      if (p.inci$inci.denominator == "all") {
        denom <- rowSums(X[, p.inci$alive])
      } else {
        denom <- rowSums(X[, p.inci$susc])
      }
      denom <- sapply(denom, max, 1)
      scaleby <- sum(denom) / denom
      scaleby <- sapply(sapply(scaleby, max, 1.), min, 50)
    }
    dX[, p.inci$susc] <- x_inci * X[, p.inci$susc] * scaleby # OK - that is simple
  }
  dX
}

derivs.inci <- function(t = NA, Y = NA, p = NA, p.inci = NA, once = F) {
  if (once == p.inci$inci.once.per.year) {
    dX <- derivs.inci.common(t = t, Y = Y, p = p, p.inci = p.inci)
    if (!is.null(dX) & sum(dX) > 1e-30) {
      return(as.matrix(p.inci$inci.matrix %*% t(dX)))
    }
  }
  return(0. * Y)
}

derivs.inci.in.out <- function(t = NA, Y = NA, p = NA, p.inci = NA, once = F) {
  if (once == p.inci$inci.once.per.year) {
    dX <- derivs.inci.common(t = t, Y = Y, p = p, p.inci = p.inci)
    if (!is.null(dX)) {
      P <- Q <- p.inci$inci.matrix
      P[p.inci$inci.matrix < 0] <- 0.
      Q[p.inci$inci.matrix > 0] <- 0.
      dY.in <- P %*% t(dX)
      dY.out <- Q %*% t(dX)
      return(list(dY = as.matrix(dY.in + dY.out), dY.in = as.matrix(dY.in), dY.out = as.matrix(dY.out)))
    }
  }
  return(list(dY = 0. * Y, dY.in = 0. * Y, dY.out = 0. * Y))
}

# in principe kunnen ART matrixen worden gecombineerd (van HIV1 -> ART1 en van HIV2 -> ART2)
# de techniek voor HIV en ART incidentie kunnen ook worden gebruikt voor vaccinatie
# data: file met kolommen YEAR from to en leeftijden in de overige kolommen
# YEAR from to    0 5 10 etc
# 2025 never vac  0.2 0.4 0.8 etc
# In feite is de techniek identiek voor incidentie van sterft, HIV, vaccinatie .....

derivs.demography <- function(t, Y, parms, dY.HIVTBdeaths) {
  # dY.HIVTBdeaths : entry into TBdead HIVdead etc called with dY[!p$ALIVE,] (resulting from transition, treatment etc)
  # NOTE_1: dY.HIVTBdeaths is positive when moving _into_ death states !!!
  #
  # NOTE_2: popadjrate < 0 if leaving an age group (due to aging into next higher age group, emigration, death)
  #
  # we do not need this here ....
  # mu.allcauses  = get.deathrate.allcauses(parms, t) # which includes HIV and TB deaths
  # mu.background = pmax(0, mu.all.causes  - dY.HIVTBdeaths / colSums(Y[parms$ALIVE,])) # subtract HIV and TB deaths
  # if (t>2000){
  #    print("2000")
  #  }
  adj <- get.popadjrate(parms, t)
  if (!(parms$intervention & t >= parms$intervention.start)) {
    adj <- adj + dY.HIVTBdeaths / colsums(Y[parms$ALIVE, ])
  }
  adj[is.infinite(adj) | is.nan(adj)] <- 0.
  dY <- -parms$DEATH.vector * t(adj * t(Y))
  # X = t(Y)
  # dX = 0*X
  # dX[,parms$ALIVE]   =  -adj * X[,parms$ALIVE] # we need to multiply with -adj as the death matrix has -1 in cells to move from
  # dY = parms$DEATH.matrix %*% t(dX)
  dY
}

scale.alive.population <- function(Y, parms) {
  Y[!parms$ALIVE, ] <- 0.
  ini.age.dist <- apply(parms$y.ini, 2, sum)
  current.age.dist <- apply(Y, 2, sum)
  Y <- scale(Y, center = F, scale = current.age.dist / ini.age.dist)
  Y[is.nan(Y) | is.infinite(Y)] <- 0
  Y
}

colsums <- function(M) {
  if (is.null(dim(M))) {
    return(M)
  } else {
    return(colSums(M))
  }
}

# called for every t listed and using prev matrix Y as input
derivs.deSolve <- function(t, X, p, efnc = NULL) {

  # cat("t=",t,"\n")
  ntimesteps <<- ntimesteps + 1
  Y <- matrix(X, p$nVXaSESRISKHIVTB, p$nAGES)
  rownames(Y) <- p$VXaSESRISKHIVTB
  colnames(Y) <- names(p$AGES)

  dY <- matmul.by.age.group(p$TBp, Y)
  dY <- dY + derivs.Tm(t, Y, p, fn = matmul.by.age.group)
  for (i in seq_along(p$inci$TB)) {
    dY <- dY + derivs.inci(t, Y, p, p$inci$TB[[i]])
  }
  if (!is.null(p$TBtr)) {
    for (i in seq_along(p$TBtr)) {
      dY <- dY + derivs.Tr(t, Y, p$TBtr[[i]], fn = matmul.by.age.group)
    }
  }

  # BUG fix in v17: the function dY.nat.death was called with dY.Ti and dY.HIVp so no other causes of TB and HIV death were included
  # other possible causes are Tr and inci

  if (p$nHIV > 1) {
    dY.HIVp <- matmul.by.age.group(p$HIVp, Y)
    if (!is.null(p$HIVtr)) {
      for (i in seq_along(p$HIVtr)) {
        dY.HIVp <- dY.HIVp + derivs.Tr(t, Y, p$HIVtr[[i]], fn = matmul.by.age.group)
      }
    }
    dY <- dY + dY.HIVp
    for (i in seq_along(p$inci$HIV)) {
      dY <- dY + derivs.inci(t, Y, p, p$inci$HIV[[i]])
    }
  }

  if (p$nRISK > 1 & !is.null(p$RISKp)) {
    dY <- dY + matmul.by.age.group(p$RISKp, Y)
  }

  if (p$nSES > 1 & !is.null(p$SESp)) {
    dY <- dY + matmul.by.age.group(p$SESp, Y)
  }

  if (p$nVXa > 1) {
    if (!is.null(p$VXap)) {
      dY <- dY + matmul.by.age.group(p$VXap, Y)
    }
    for (i in seq_along(p$inci$VXa)) {
      # cat(t,sum(abs(derivs.inci(t,Y,p,p$inci$VXa[[i]]))),"\n")
      dY <- dY + derivs.inci(t, Y, p, p$inci$VXa[[i]])
    }
  }

  # adjust demography based on pop data and compensate HIV and TB deaths
  # NOTE 1: HIV and TB deaths are included in dY (as negative contributions) already due to progression)
  # NOTE 2: therefore dY.demog is just an adjustment to pop size after taking into account HIV and TB deaths (as these are included in the data)
  # if (t>=2000 & t <2001){
  #  print(t)
  # }
  dY.HIVTBdeaths <- as.numeric(colsums(dY[!p$ALIVE, ])) # i.e. a number > 0
  dY.demog <- derivs.demography(t, Y, p, dY.HIVTBdeaths)
  dY <- dY + dY.demog

  assert_that(sum(is.nan(dY)) == 0, msg = paste0("NaNs in derivs.deSolve @ t=", t))
  assert <<- F
  # note that we could output the full dY or detailed deaths by not summing over columns

  if (!(p$intervention & t >= p$intervention.start)) {
    dY.BGdeaths <- get.deathrate.allcauses(p, t) * colsums(Y[p$ALIVE, ]) - dY.HIVTBdeaths
    n <- sum(dY.BGdeaths < 0)
    if (n > 0) {
      modlog(level = "DEBUG", msg = paste0("at t=", t, " ", n, " age groups with HIV TB deaths exceeding all cause mortality (UNPOP data) ; capping to ensure background deaths >= 0"))
    }
    dY.BGdeaths <- pmax(0, dY.BGdeaths)
    dY.POPadj <- as.numeric(colsums(dY.demog))
  } else {
    dY.BGdeaths <- get.bgdeathrate(p, t) * colsums(Y[p$ALIVE, ])
    dY.POPadj <- 0 * dY.HIVTBdeaths
  }
  list(
    dY = as.numeric(dY),
    dY.HIVTBdeaths = dY.HIVTBdeaths,
    dY.BGdeaths = dY.BGdeaths,
    dY.POPadj = dY.POPadj
  )
}

run.deSolve <- function(params = NULL) {
  assert <<- T

  ntimesteps <<- 0
  started.at <- proc.time()
  if (params$contacts$defined) {
    params$contacts$M <- update.contact.matrix(params, colSums(params$y.ini[params$ALIVE, ]))
  }
  params$run.params$rescaled.pop <- F
  rawout <- rk(
    y = as.vector(params$y.ini),
    times = params$run.params$simulation$years,
    func = derivs.deSolve,
    parms = params,
    rtol = params$run.params$num.int$rtol,
    atol = params$run.params$num.int$atol,
    method = params$run.params$num.int$method,
    maxsteps = params$run.params$num.int$maxsteps,
    hini = params$run.params$num.int$hini,
    hmin = params$run.params$num.int$hmin,
    events = list(
      func = aging.and.births.and.update.contact.matrix.event,
      time = unique(trunc(params$run.params$simulation$years))[-1]
    )
  )
  if (ntimesteps > (max(params$run.params$simulation$years) - min(params$run.params$simulation$years)) * 50) {
    modlog(level = "WARN", msg = paste0(" ntimesteps = ", ntimesteps, " in ", timetaken(started.at), " which may be excessive ; please reconsider parameter values"))
  } else {
    modlog(level = "INFO", msg = paste0(" ntimesteps = ", ntimesteps, " in ", timetaken(started.at)))
  }

  t <- rawout[, 1]
  out <- vector("list", length(t))
  dHIVTBx <- vector("list", length(t))
  dALLx <- vector("list", length(t))
  dBGx <- vector("list", length(t))
  dfrPOPadj <- vector("list", length(t))
  nr <- params$nVXaSESRISKHIVTB
  nc <- params$nAGES
  N <- nr * nc
  for (i in seq_along(t)) {
    M <- matrix(rawout[i, 2:(N + 1)], nr, nc)
    colnames(M) <- params$AGES
    rownames(M) <- params$VXaSESRISKHIVTB
    out[[i]] <- M
    range <- (N + 2):(N + nc + 1)
    dHIVTBx[[i]] <- as.vector(rawout[i, range])
    names(dHIVTBx[[i]]) <- params$AGES
    dBGx[[i]] <- as.vector(rawout[i, range + nc])
    names(dBGx) <- params$AGES
    P <- as.vector(rawout[i, range + 2 * nc])
    dfrPOPadj[[i]] <- P / colSums(M[params$ALIVE, ])
    names(dfrPOPadj) <- params$AGES
  }
  list(
    times = t, state = out, dHIVTBx = dHIVTBx,
    dBGx = dBGx, dfrPOPadj = dfrPOPadj
  )
}

run.model <- function(model.params = NULL) {
  out <- run.deSolve(model.params)
  modlog(level = "DEBUG", msg = "successful result of run.deSolve()")
  options <- model.params$run.params$output
  stocks <- generate.prevalence.output(t = out$times, state = out$state, fparams = model.params)
  if (options$suppress_zeros_stocks) {
    stocks <- stocks[value > 1e-4, ]
  }
  modlog(level = "DEBUG", msg = "successful generation of stocks output from generate.prevalence.output()")
  flows <- generate.output(incidence.from.model.run(out, model.params), params = model.params)
  if (options$suppress_zeros_flows) {
    flows <- flows[abs(value) > 1e-6, ]
  }
  modlog(level = "DEBUG", msg = "successful generation of flow output from generate.flow.output()")
  if (options$econ.output) {
    demography.output <- demography.from.model.run(out, params = model.params)
    return(list(
      dBGx = demography.output$dBGx,
      dfrBGx = demography.output$dfrBGx,
      dfrPOPadj = demography.output$dfrPOPadj,
      dHIVTBx = demography.output$dHIVTBx,
      population = demography.output$pop,
      stocks = stocks,
      flows = flows
    ))
  } else {
    result <- list(stocks = stocks, flows = flows)
    return(result)
  }
}
