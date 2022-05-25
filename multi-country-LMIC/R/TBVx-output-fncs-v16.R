output.to.array <- function(Y, parms) {
  years <- Y[[1]]
  n <- length(years)
  idxs <- 1:n
  dimZ <- c(n, parms$nVXa, parms$nSES, parms$nRISK, parms$nHIV, parms$nTB, parms$nAGES)
  ndim <- length(dimZ)
  Z <- array(0, dim = dimZ)
  tb <- 1:parms$nTB
  if (ndim == 7) {
    for (i in idxs) {
      for (vxa in 1:parms$nVXa) {
        for (ses in 1:parms$nSES) {
          for (risk in 1:parms$nRISK) {
            for (hiv in 1:parms$nHIV) {
              Z[i, vxa, ses, risk, hiv, , ] <- Y[[2]][[i]][calc.index.by.nr(parms, vxa, ses, risk, hiv, tb), ]
            }
          }
        }
      }
    }
    dimnames(Z) <- list(YEAR = years, VXa = names(parms$VXa), SES = names(parms$SES), RISK = names(parms$RISK), HIV = names(parms$HIV), TB = names(parms$TB), AGE = parms$AGES)
  } else {
    stop("unsupported number of dimensions in output")
  }
  Z
}

generate.filename <- function(fparams, s, path = ".") {
  s1 <- unlist(strsplit(fparams$xmlfile, "/", fixed = T))
  s2 <- unlist(strsplit(s1[length(s1)], ".xml", fixed = T))
  s3 <- unlist(strsplit(fparams$xmlrunfile, "/", fixed = T))
  s4 <- unlist(strsplit(s3[length(s3)], ".xml", fixed = T))
  s5 <- as.character(as.integer(seconds(now())) %% 100000)
  md5run <- digest(fparams$run.params, algo = "sha1")
  md5mdl <- digest(fparams, algo = "sha1")
  sig <- paste(substring(md5mdl, nchar(md5mdl) - 4), "_", substring(md5run, nchar(md5run) - 4), sep = "")
  paste(path, "/output/", s2, "_", s4, "_", sig, "_", s5, s, ".txt", sep = "")
}

generate.output.df <- function(t, state, dim, subjectname, matname, fparams, cols) {
  if (is.na(subjectname)) {
    nrows <- nrow(state[[1]])
    matname <- NA
  } else {
    nrows <- nrow(state[[1]][[subjectname]][[matname]])
  }
  # cat(subjectname,nrows,"\n")
  dim <- rep(dim, nrows)
  subject <- rep(subjectname, nrows)
  flow <- rep(matname, nrows)
  cols <- cbind(cols, dim, subject, flow)
  output <- fparams$run.params$output
  sel <- which(t %in% output$years)
  assert_that(length(sel) > 0, msg = "no overlap between years specified for incidence output and detailed output")
  basic_df <- data.frame(year = rep(0, nrows))
  basic_df <- cbind(basic_df, cols)
  df <- data.frame()
  for (i in sel) {
    year <- rep(t[i], nrows)
    M <- NULL
    if (sum(is.na(output$age.from)) == 0) {
      if (is.na(subjectname)) {
        M <- aggregate.by.age.groups(state[[i]], output$age.from, sumcols = F, avg = F)
      } else {
        M <- aggregate.by.age.groups(state[[i]][[subjectname]][[matname]], output$age.from, sumcols = F, avg = F)
      }
    } else {
      if (is.na(subjectname)) {
        M <- state[[i]]
      } else {
        M <- state[[i]][[subjectname]][[matname]]
      }
    }
    colnames(M) <- paste("A", colnames(M), sep = "")
    row.names(M) <- NULL
    local_df <- cbind(basic_df, M)
    local_df$year <- year
    df <- rbind(df, local_df)
  }
  y <- as.data.frame(melt(setDT(df), measure.vars = patterns("^A\\d+$"), variable.name = "age_from", value.name = "value"))
  z <- cbind(y[, 1:(ncol(y) - 1)], age_thru = as.integer(rep(0, nrow(y))), value = y[, ncol(y)])
  z$age_from <- as.integer(substring(z$age_from, 2))
  agesfrom <- unique(z$age_from)
  agesthru <- c(agesfrom[2:length(agesfrom)], 100) - 1
  for (i in seq_along(agesfrom)) {
    sel <- z$age_from == agesfrom[i]
    z[sel, "age_thru"] <- agesthru[i]
  }
  z
}
generate.prevalence.output <- function(t, state, fparams) {
  VXa <- calc.names.for.dim(fparams, "VXa")
  SES <- calc.names.for.dim(fparams, "SES")
  RISK <- calc.names.for.dim(fparams, "RISK")
  HIV <- calc.names.for.dim(fparams, "HIV")
  TB <- calc.names.for.dim(fparams, "TB")
  cols <- cbind(VXa, SES, RISK, HIV, TB)
  z <- as.data.table(generate.prev.output.df(t, state, fparams, cols))
  output <- rbind(z, z[, .(age_from = min(age_from), age_thru = max(age_thru), value = sum(value)), by = c(names(z)[1:6])])
  output <- cbind(country = fparams$run.params$countrycode, output)
  output
}

generate.prev.output.df <- function(t, state, fparams, cols) {
  nrows <- nrow(state[[1]])
  output <- fparams$run.params$output
  sel <- which(t %in% output$years)
  assert_that(length(sel) > 0, msg = "no overlap between years specified for incidence output and detailed output")
  basic_df <- data.frame(year = rep(0, nrows))
  basic_df <- cbind(basic_df, cols)
  df <- data.frame()
  for (i in sel) {
    year <- rep(t[i], nrows)
    M <- NULL
    if (sum(is.na(output$age.from)) == 0) {
      M <- aggregate.by.age.groups(state[[i]], output$age.from, sumcols = F, avg = F)
    } else {
      M <- state[[i]]
    }
    colnames(M) <- paste("A", colnames(M), sep = "")
    row.names(M) <- NULL
    local_df <- cbind(basic_df, M)
    local_df$year <- year
    df <- rbind(df, local_df)
  }
  y <- as.data.frame(melt(setDT(df), measure.vars = patterns("^A\\d+$"), variable.name = "age_from", value.name = "value"))
  z <- cbind(y[, 1:(ncol(y) - 1)], age_thru = as.integer(rep(0, nrow(y))), value = y[, ncol(y)])
  z$age_from <- as.integer(substring(z$age_from, 2))
  agesfrom <- unique(z$age_from)
  agesthru <- c(agesfrom[2:length(agesfrom)], 100) - 1
  for (i in seq_along(agesfrom)) {
    sel <- z$age_from == agesfrom[i]
    z[sel, "age_thru"] <- agesthru[i]
  }
  z
}
# subjects = subjects[-which(subjects)=="TBdeaths"]

generate.flow.output <- function(t, state, fparams, dim = NA) {
  VXa <- calc.names.for.dim(fparams, "VXa")
  SES <- calc.names.for.dim(fparams, "SES")
  RISK <- calc.names.for.dim(fparams, "RISK")
  HIV <- calc.names.for.dim(fparams, "HIV")
  TB <- calc.names.for.dim(fparams, "TB")
  cols <- cbind(VXa, SES, RISK, HIV, TB)
  subjects <- names(state[[1]])
  matnames <- c("dY.in", "dY.out")
  z <- NULL
  for (subject in subjects) {
    if (subject == "Adj" & !is.null(state[[1]][[subject]])) {
      z <- rbind(z, generate.output.df(t, state, dim, subject, "dY", fparams, cols))
    } else {
      for (matname in matnames) {
        if (!is.null(state[[1]][[subject]])) {
          z <- rbind(z, generate.output.df(t, state, dim, subject, matname, fparams, cols))
        }
      }
    }
  }
  setDT(z)
  # add 0 thru 99 age group
  output <- rbind(z, z[, .(age_from = min(age_from), age_thru = max(age_thru), value = sum(value)), by = c(names(z)[1:9])])
  output <- cbind(country = fparams$run.params$countrycode, output)
  output
}

generate.output <- function(inci, params) {
  if (!is.null(params$run.params$output)) {
    out <- list(prev = NULL, TB = NULL, HIV = NULL, VXa = NULL, DEM = NULL)
    for (dim in names(inci)[-(1:2)]) {
      if (!is.null(inci[[dim]])) {
        out[[dim]] <- generate.flow.output(t = inci$t, state = inci[[dim]], params, dim = dim)
      }
    }
    result <- out$TB
    if (!is.null(out$HIV)) {
      result <- rbind(result, out$HIV)
    }
    if (!is.null(out$VXa)) {
      result <- rbind(result, out$VXa)
    }
    if (!is.null(out$DEM)) {
      result <- rbind(result, out$DEM)
    }

    result$flow <- as.character(result$flow)
    result$flow[result$flow == "dY"] <- "net"
    result$flow[result$flow == "dY.in"] <- "in"
    result$flow[result$flow == "dY.out"] <- "out"
    # out$prev$flow = as.character(out$prev$flow)
    # result = rbind(out$prev,result)
    result$flow <- as.factor(result$flow)
    return(result)
  }
  NULL
}

write.output <- function(data, filename, output.format = "txt") {
  if (output.format == "parquet") {
    filename <- gsub("txt$", "parquet", filename)
    write_parquet(data, filename)
  } else if (output.format == "fst") {
    filename <- gsub("txt$", "fst", filename)
    write.fst(data, path = filename, compress = 100)
  } else {
    write.table(data, file = filename, row.names = F, sep = "\t", quote = F)
  }
  print(filename)
}

add.dY <- function(A = NULL, fn = NULL, include = F) {
  if (include) {
    res <- fn
    if (is.null(A)) {
      A <- list(dY = 0. * res$dY, dY.in = 0. * res$dY, dY.out = 0. * res$dY)
    }
    A$dY <- A$dY + res$dY
    A$dY.in <- A$dY.in + res$dY.in
    A$dY.out <- A$dY.out + res$dY.out
  }
  A
}
demography.from.model.run <- function(out, params) {
  sel <- (out$times %% 1) == 0.5
  t <- out$times[sel]
  indexes <- (1:length(out$state))[sel]
  assert_that(sum(sel) > 0, msg = "no time points at ####.5 found ....")
  Y <- matrix(0, nrow = length(t), ncol = ncol(out$state[[1]]))
  colnames(Y) <- as.integer(colnames(out$state[[indexes[1]]]))
  rownames(Y) <- t
  country <- rep(params$run.params$countrycode, nrow(Y))
  dHIVTBx <- Y # TB HIV deaths (numbers / yr)
  # dALLx       = Y # ALL    deaths (numbers / yr)
  dBGx <- Y # BG     deaths (numbers / yr)
  dfrBGx <- Y # fractional background deaths (fraction of the pop / yr)
  dfrPOPadj <- Y # fractional adj rates (fraction of the pop / yr)

  if (!params$intervention) {
    for (i in seq_along(t)) {
      Y[i, ] <- colSums(out$state[[indexes[i]]][params$ALIVE, ])
      dHIVTBx[i, ] <- out$dHIVTBx[[indexes[i]]]
      # dALLx[i,]     = out$dALLx[[indexes[i]]]
      dBGx[i, ] <- out$dBGx[[indexes[i]]]
      dfrBGx[i, ] <- out$dBGx[[indexes[i]]] / Y[i, ]
      dfrPOPadj[i, ] <- out$dfrPOPadj[[indexes[i]]]
    }
    pop <- cbind(year = t, country = country, as.data.frame(Y, row.names = F))
    dHIVTBx.df <- cbind(year = t, country = country, as.data.frame(dHIVTBx, row.names = F))
    # dALLx.df     = cbind(year=t,country=country,as.data.frame(dALLx,row.names = F))
    dBGx.df <- cbind(year = t, country = country, as.data.frame(dBGx, row.names = F))
    dfrBGx.df <- cbind(year = t, country = country, as.data.frame(dfrBGx, row.names = F))
    dfrPOPadj.df <- cbind(year = t, country = country, as.data.frame(dfrPOPadj, row.names = F))
  } else {
    for (i in seq_along(t)) {
      Y[i, ] <- colSums(out$state[[indexes[i]]][params$ALIVE, ])
      dHIVTBx[i, ] <- out$dHIVTBx[[indexes[i]]]
      dBGx[i, ] <- out$dBGx[[indexes[i]]]
      # dALLx[i,]     = out$dALLx[[indexes[i]]]
    }
    pop <- cbind(year = t, country = country, as.data.frame(Y, row.names = F))
    dHIVTBx.df <- cbind(year = t, country = country, as.data.frame(dHIVTBx, row.names = F))
    dBGx.df <- cbind(year = t, country = country, as.data.frame(dBGx, row.names = F))
    # dALLx.df     = cbind(year=t,country=country,as.data.frame(dALLx,row.names = F))
    dfrBGx.df <- NULL
    dfrPOPadj.df <- NULL
  }
  modlog(level = "DEBUG", msg = "successful call of demography.from.model.run()")
  list(
    pop = pop, dHIVTBx = dHIVTBx.df, # dALLx=dALLx.df,
    dBGx = dBGx.df, dfrBGx = dfrBGx.df, dfrPOPadj = dfrPOPadj.df
  )
}
incidence.from.model.run <- function(out, params) {
  output <- params$run.params$output
  sel <- which(out$t %in% output$years)
  result <- list(t = out$t[sel])
  for (i in seq_along(result$t)) {
    out.index <- sel[i]
    result$PREV[[i]] <- out$state[[out.index]]
    if (params$contacts$defined) {
      params$contacts$M <- update.contact.matrix(params, colSums(result$PREV[[i]][params$ALIVE, ]))
    }
    df.sel <- output$options[output$options$dim == "TB", ]
    result$TB[[i]] <- incidence.from.Y.matrix(t = out$t[out.index], out$state[[out.index]], params, sel.output = df.sel)
    deaths <- result$TB[[i]]$TBdeaths
    result$TB[[i]]$TBdeaths <- NULL
    if (params$DIMLENGTHS["HIV"] > 1) {
      df.sel <- output$options[output$options$dim == "HIV", ]
      result$HIV[[i]] <- incidence.from.Y.matrix(t = out$t[out.index], out$state[[out.index]], params, sel.output = df.sel)
      deaths <- deaths + result$HIV[[i]]$HIVdeaths
      result$HIV[[i]]$HIVdeaths <- NULL
    }
    # now do something clever with deaths and demog
    # result$DEM[[i]] = list(Adj=list(dY=derivs.demography(t=out$t[out.index],out$state[[out.index]],params,colSums(deaths))))
    if (params$DIMLENGTHS["VXa"] > 1) {
      df.sel <- output$options[output$options$dim == "VXa", ]
      result$VXa[[i]] <- incidence.from.Y.matrix(t = out$t[out.index], out$state[[out.index]], params, sel.output = df.sel)
    }
  }
  modlog(level = "DEBUG", msg = "successful call of incidence.from.model.run() [ NOTE: no output on VXa incidence yet ]")
  result
}

incidence.from.Y.matrix <- function(t, Y, p, sel.output = NA) {
  if (sum(is.na(sel.output)) > 0) {
    return(NULL)
  }

  if (sel.output$dim == "TB") {
    out <- list()
    out$Tm <- add.dY(A = NULL, fn = derivs.Tm(t, Y, p, fn = matmul.by.age.group.in.out), include = sel.output$transmission)
    out$TBp <- add.dY(A = NULL, fn = matmul.by.age.group.in.out(p$TBp, Y), include = sel.output$progression)
    out$TBdeaths <- out$Tm$dY.in[!p$ALIVE, ] + out$TBp$dY.in[!p$ALIVE, ]
    for (i in seq_along(p$TBtr)) {
      name <- paste0("TBtr_", names(p$TBtr)[i])
      out[[name]] <- add.dY(A = NULL, fn = derivs.Tr(t, Y, p$TBtr[[i]], fn = matmul.by.age.group.in.out), include = sel.output$treatment)
      out$TBdeaths <- out$TBdeaths + out[[name]]$dY.in[!p$ALIVE, ]
    }
    out$Xi <- NULL
    for (i in seq_along(p$inci$TB)) {
      q <- p$inci$TB[[i]]
      if (q$dim == sel.output$dim) {
        out$Xi <- add.dY(out$Xi, fn = derivs.inci.in.out(t, Y, p, q, abs(t %% 1) < 1e-4), include = sel.output$incidence)
      }
    }
    if (!is.null(out$Xi)) {
      out$TBdeaths <- out$TBdeaths + out$Xi$dY.in[!p$ALIVE, ]
    }
    return(out)
  } else {
    Tiname <- paste0(sel.output$dim, "p")
    Xiname <- paste0(sel.output$dim, "Xi")
    out <- list()
    out[[Tiname]] <- add.dY(A = NULL, fn = matmul.by.age.group.in.out(p$PROGRESSION[[sel.output$dim]], Y = Y), include = sel.output$progression)
    if (sel.output$dim == "HIV") {
      out$HIVdeaths <- out[[Tiname]]$dY.in[!p$ALIVE, ]
      for (i in seq_along(p$HIVtr)) {
        name <- paste0("HIVtr_", names(p$HIVtr)[i])
        out[[name]] <- add.dY(A = NULL, fn = derivs.Tr(t, Y, p$HIVtr[[i]], fn = matmul.by.age.group.in.out), include = sel.output$progression)
        out$HIVdeaths <- out$HIVdeaths + out[[name]]$dY.in[!p$ALIVE, ]
      }
    }
    out[[Xiname]] <- NULL
    for (i in seq_along(p$inci[[sel.output$dim]])) {
      q <- p$inci[[sel.output$dim]][[i]]
      if (q$dim == sel.output$dim) {
        out[[Xiname]] <- add.dY(A = out[[Xiname]], fn = derivs.inci.in.out(t, Y, p, q, abs(t %% 1) < 1e-4), include = sel.output$incidence)
        if (sel.output$dim == "HIV") {
          out$HIVdeaths <- out$HIVdeaths + out[[Xiname]]$dY.in[!p$ALIVE, ]
        }
      }
    }
    return(out)
  }
}
