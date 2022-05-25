### AutoEmulate (Redo)

### Sample usage: Rscript AutoEmulate.R -c AFG -p input.csv -t target.csv -x XMLinput.xml -n 10 -s 0 -m 1000 -f 0.4
### Details of flags:
## -c ISO Country code (Required)
## -p Parameter file including .csv extension (Default input.csv)
## -t Target file including .csv extentson (Default target.csv)
## -x XML file including .xml extension (Default XMLinput.xml)
## -n Maximum number of waves (Default 15)
##
## -m Minimum desired number of full fits (Default 1000)
## -f Proportion of full fits at a wave to trigger full-fit mining (Default 0.4)
## -s Proportion of runs at mining stage that target good runs (Default 0, EXPERIMENTAL)

rm(list = ls())

fileversion = "5.5.3 (Deja Vu)"

library(here)
library(lhs)
library(hmer)
library(log4r)
library(getopt)

### HELPER FUNCTIONS ###
# Determines how many of the outputs for a point are within bounds 
in.bounds <- function(point, targets, out_stat = "sum") {
  fits <- purrr::map_lgl(names(targets), function(i) {
    as.numeric(point[[i]]) <= targets[[i]][2] && as.numeric(point[[i]]) >= targets[[i]][1]
  })
  eval_func <- get(out_stat)
  return(eval_func(fits))
}

# Wrapper for the read.csv options, for brevity
r.csv <- function(filename) {
  df <- read.csv(filename, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE)
  return(df)
}

# Generates a csv containing the varying and non-varying parameters, as well as the outputs
gen.csv <- function(results, input) {
  all.params <- r.csv(here("countries", opts$country, "parameters", input))
  param.names <- all.params$unique.name
  c.params <- setNames(all.params[!all.params$choose, "mean"], all.params[!all.params$choose, "unique.name"])
  for (i in 1:length(c.params)) {
    results[[names(c.params)[i]]] <- c.params[[i]]
  }
  output.names <- names(results)[!names(results) %in% param.names]
  results <- results[,c(param.names, output.names)]
  results$uid <- apply(results, 1, function(x) digest::digest(paste0(x, collapse = ""), 'md5', serialize = FALSE))
  return(results)
}

## Need to check this for compatibility with new model version
# A wrapper for the running of the model; includes functionality to remove 'bad' points
run.model <- function(points, wave, res) {
  all_outputs <- apply(points, 1, function(x) run(model, x, FALSE)$hits)
  check_null <- purrr::map_lgl(all_outputs, ~!is.null(.))
  all_outputs <- all_outputs[check_null]
  points <- points[check_null,]
  which.include <- lapply(all_outputs, nrow) == length(target.bounds)
  all_outputs <- all_outputs[which.include]
  output_values <- do.call('rbind', purrr::map(all_outputs, ~.$model))
  output_hits <- do.call('rbind', purrr::map(all_outputs, ~.$fit))
  nhits <- apply(output_hits, 1, sum)
  res[[wave]] <- cbind(points[which.include,], setNames(data.frame(output_values), names(target.bounds)))
  df_to_write <- cbind(res[[wave]], setNames(data.frame(output_hits), paste0(names(target.bounds), "hit")))
  df_to_write$nhits <- nhits
  is.valid <- apply(output_values, 1, function(x) all(x >= 0))
  res[[wave]] <- res[[wave]][is.valid,]
  df_to_write <- df_to_write[is.valid,]
  write.csv(gen.csv(df_to_write, opts$parameters), file = here("countries", opts$country, "output", paste0(RUNID_TS, "wave", wave-1, ".csv")), row.names = FALSE)
  return(res)
}

# Exchanges RData files
swap.files <- function(which.in, which.out, msg = NULL) {
  if (file.exists(which.out)) file.remove(which.out)
  save.image(file = which.in)
  if (!is.null(msg)) emulator.log(msg = msg)
}

# Initialise the model
model = new.env()
source(here("R", "include-v11.R"), model)
source(here("R", "TBVx-run-v1.R"))

# Get options from command line (see start for option specifics)
opts <- getopt(
  matrix(
    c('country', 'c', 1, 'character',
      'parameters', 'p', 2, 'character',
      'targets', 't', 2, 'character',
      'xml', 'x', 2, 'character',
      'n_waves', 'n', 2, 'integer',
      'seek_percent', 's', 2, 'double',
      'min_fits', 'm', 2, 'integer',
      'mine_trigger', 'f', 2, 'double'
      ),
    byrow = TRUE,
    ncol = 4
  )
)

# For cluster work: finds any backup files to load from
job.ident <- sub("^[1234567890-]*_(.*)", "\\1", Sys.getenv("RUNID_TS"))
backup.list <- grepl(paste0(".*", job.ident, "_backup.RData"), dir(here("countries", opts$country, "output")))
is.backed <- any(backup.list)
if (is.backed) {
  backup.file <- dir(here("countries", opts$country, "output"), full.names = TRUE)[backup.list]
  if (sum(backup.list) > 1)
    backup.file <- backup.file[order(file.info(backup.file)$ctime, decreasing = TRUE)][1]
  else backup.file <- backup.file[1]
  load(backup.file)
  OLD_RUNID_TS <- RUNID_TS
  is.backed <- TRUE
}

RUNID_TS <- Sys.getenv("RUNID_TS")
if (RUNID_TS == "")
  RUNID_TS <- sprintf("%s_%s_%s", format.Date(Sys.time(), format = "%Y-%m-%d-%H%M", tz = "UTC"), "LOCAL", opts$country)

if (is.backed) file.remove(backup.file.name)
backup.file.name <- here("countries", opts$country, "output", paste0(RUNID_TS, "_backup.RData"))
failure.file.name <- here("countries", opts$country, "output", paste0(RUNID_TS, "_failed.RData"))
complete.file.name <- here("countries", opts$country, "output", paste0(RUNID_TS, "_complete.RData"))

if (is.backed) save.image(file = backup.file.name)

dir.create(here("countries", opts$country, "logs"), showWarnings = FALSE)
dir.create(here("countries", opts$country, "output"), showWarnings = FALSE)

logr.emulator <- create.logger(logfile = here("countries", opts$country, "logs", paste0(RUNID_TS, "_emulator.log")), level = "INFO")
emulator.log <- function(level = "INFO", msg = NULL) levellog(logr.emulator, level = level, message = msg)
emulator.log(msg = paste("This is AutoEmulate version", fileversion))

tfile <- if(is.null(opts$targets)) "target.csv" else opts$targets
pfile <- if(is.null(opts$parameters)) "input.csv" else opts$parameters
xfile <- if(is.null(opts$xml)) "XMLinput.xml" else opts$xml

model$paths <- model$set.paths(countrycode = opts$country, xml = xfile, parameters = pfile, targets = tfile)

# Gets the data from the input.csv and target.csv files for use in emulation
tryCatch(
  {
    param.data <- subset(r.csv(here("countries", opts$country, "parameters", pfile)), choose == TRUE)
    ranges <- setNames(purrr::map(1:nrow(param.data), ~c(param.data[.,'min'], param.data[.,'max'])), param.data$unique.name)
    target.data <- subset(r.csv(here("countries", opts$country, "parameters", tfile)), err != -1)
    target.bounds <- setNames(purrr::map(1:nrow(target.data), ~c(target.data[.,'lo'], target.data[.,'hi'])), paste0(target.data[,'name'], target.data[,'year']))
  },
  error = function(e) {
    emulator.log("FATAL", "Could not read parameter and/or target file - missing or misspecified.")
    stop("Error reading parameter and/or target csvs.")
  }
)

n_points <- 20*length(ranges)
em.ranges <- ranges
em.targets <- target.bounds

# First runs: generate an LHS and run the model for the first time.
if (!is.backed) {
  emulator.log(msg = "Finished preliminary setup. Beginning first model runs.")
  results.list <- list()
  emulators.list <- c()
  targets.list <- c()
  initial.points <- setNames(data.frame(t(apply(lhs::maximinLHS(5*n_points/4, length(ranges)), 1, function(x) {
    purrr::map_dbl(em.ranges, ~.[1]) + x * purrr::map_dbl(em.ranges, diff)
  }))), names(ranges))
  tryCatch(
    results.list <- run.model(initial.points, 1, results.list),
    error = function(e) {
      emulator.log(level = "FATAL", msg = paste("Model runs failed:", e))
      stop("Model runs failed. See emulator log for failure details.")
    }
  )
} else {
  emulator.log(msg = paste("Restarted from save state", OLD_RUNID_TS, "- see corresponding logs."))
  emulator.log(msg = paste("Backup file", backup.file, "deleted and replaced with", backup.file.name))
}

if (nrow(results.list[[1]]) > n_points) results.list[[1]] <- results.list[[1]][sample(1:nrow(results.list[[1]]), n_points),]

# Initialisation of options for emulation
kicked_out <- FALSE
n_waves <- if(is.null(opts$n_waves)) 15 else opts$n_waves
minimum_points <- if (is.null(opts$min_fits)) 1000 else opts$min_fits
seek_percent <- if (is.null(opts$seek_percent)) 0 else opts$seek_percent
mine_trigger <- if (is.null(opts$mine_trigger)) 0.4 else opts$mine_trigger

if (n_waves >= length(results.list)) {
  i <- length(results.list)
  while (i <= n_waves) {
    emulator.log(msg = paste("Wave", i, "starting. Training emulators."))
    # Sets up the ranges: if first wave, then use the ranges. If not, then find the minimum enclosing hyperrectangle of the current runs (plus/minus 5%)
    if (i != 1) {
      e.ranges <- setNames(purrr::map(names(ranges), ~c(max(ranges[[.]][1], min(results.list[[i]][,.]) - 0.05 * diff(range(results.list[[i]][,.]))), min(ranges[[.]][2], max(results.list[[i]][,.]) + 0.05 * diff(range(results.list[[i]][,.]))))), names(ranges))
    } else {
      e.ranges <- ranges
    }
    # Sample points and train the emulators
    tryCatch(
      {
        ## This tries to include some full fit points, if they exist, in the training set (but not too many!)
        all_hits <- c(which(apply(results.list[[i]], 1, in.bounds, target.bounds) == length(target.bounds)), use.names = FALSE)
        if (length(all_hits) > 0) all_hits <- sample(all_hits, ceiling(length(all_hits)/2))
        if (length(all_hits) > max(length(ranges)*5, ceiling(n_points/4))) all_hits <- all_hits[sample(length(all_hits)), max(length(ranges)*5, ceiling(n_points/4))]
        if (length(all_hits) > 0) samp <- sample((1:nrow(results.list[[i]]))[-all_hits], max(length(ranges)*10, ceiling(n_points/2))-length(all_hits))
        else samp <- sample(1:nrow(results.list[[i]]), max(length(ranges)*10, ceiling(n_points/2)))
        train <- results.list[[i]][c(all_hits, samp),]
        valid <- results.list[[i]][-c(all_hits, samp),]
        # Train the emulators using train as the dataset
        ems <- emulator_from_data(train, names(em.targets), e.ranges)
      },
      error = function(e) {
        swap.files(failure.file.name, backup.file.name, paste("Emulator training failed. Check by hand.", e))
        kicked_out <<- TRUE
        stop("Emulator training failed. Error details are in emulator log.")
      }
    )
    if (kicked_out) break
    # This just ensures that the emulator variance hasn't fallen off a cliff (simple regularisation)
    for (j in 1:length(ems)) {
      if (ems[[j]]$u_sigma < 1e-8) ems[[j]] <- ems[[j]]$set_sigma(sqrt(1e-7))
    }
    # Debugging flag: if there's an emulator with no dependence on parameters, then something has likely gone weird (but might not have)
    if (any(purrr::map_dbl(ems, ~length(.$basis_f)) == 1))
      emulator.log("WARN", "One or more trained emulators have no dependence on parameters. This could be due to anomalous model points.")
    # Diagnostics
    emulator.log(msg = paste("Wave", i, "emulators trained. Performing diagnostics."))
    # Check misclassifications, and inflate variance until no such misclassifications exist
    for (j in 1:length(ems)) {
      misclass <- nrow(classification_diag(ems[[j]], em.targets, valid, plt = FALSE))
      while(misclass > 0) {
        ems[[j]] <- ems[[j]]$mult_sigma(1.1)
        misclass <- nrow(classification_diag(ems[[j]], em.targets, valid, plt = FALSE))
      }
    }
    bad.ems <- c()
    # Compare emulated to simulated output: emulators are dropped from a wave if they do not agree on at least 75% of the validation points
    for (j in 1:length(ems)) {
      tryCatch(
        {
          bad.model <- nrow(comparison_diag(ems[[j]], em.targets, valid, plt = FALSE))
          if (bad.model > floor(nrow(valid)/4)) {
            bad.ems <- c(bad.ems, j)
            emulator.log("WARN", paste("Emulator for output", ems[[j]]$output_name, "does not pass comparison diagnostics and will not be used at this wave."))
          }
        },
        error = function(e) {
          bad.ems <- c(bad.ems, j)
          emulator.log("WARN", paste("Emulator for output", ems[[j]]$output_name, "does not pass comparison diagnostics and will not be used at this wave."))
        }
      )
    }
    ems <- ems[!seq_along(ems) %in% bad.ems]
    if (length(ems) == 0) {
      swap.files(failure.file.name, backup.file.name, paste("All emulators failed diagnostics at wave", i))
      stop("All emulators failed diagnostics.")
    }
    # This comes into play later, where we terminate early if the emulators are as certain as they're going to get
    uncertainty.products <- purrr::map_dbl(ems, ~6*sqrt(.$u_sigma^2 + diff(em.targets[[.$output_name]])^2/36)/diff(em.targets[[.$output_name]]))
    emulator.log(msg = paste("Emulator diagnostics complete.", length(ems), "targets will be matched to at this wave."))
    # Helpful to look at what proportion of previous points are viewed as acceptable at this wave
    accept.rate <- sum(nth_implausible(ems, results.list[[i]], em.targets, cutoff = 3))/nrow(results.list[[i]])
    emulator.log(msg = paste("Estimated acceptance rate for wave", i, "emulators considering wave", i-1, "points is", signif(accept.rate, 5)))
    emulator.log(msg = paste("Proposing new points from emulators at wave", i))
    # Point generation: most of the guts of this live in the generate_new_runs function now.
    tryCatch(
      new.points <- generate_new_runs(c(ems, emulators.list), n_points, em.targets),
      error = function(e) {
        swap.files(failure.file.name, backup.file.name, paste("Point generation failed", e))
        kicked_out <<- TRUE
      }
    )
    if (kicked_out) break
    # Diagnostic to log if the emulators had to propose at implausibility higher than 3
    max_imp <- signif(max(nth_implausible(c(ems, emulators.list), new.points, em.targets)), 4)
    emulator.log(msg = paste("Proposed points have implausibility no greater than", max_imp))
    emulator.log(msg = "Running the model for these new points.")
    # Run the new points!
    tryCatch(
      results.list <- run.model(new.points, i+1, results.list),
      error = function(e) {
        emulator.log("FATAL", paste("Model runs at wave", i, "failed:", e))
        kicked_out <<- TRUE
        stop(paste("Model runs failed at wave", i))
      }
    )
    if (kicked_out) break
    emulators.list <- c(ems, emulators.list)
    swap.files(backup.file.name, backup.file.name)
    # Full fit mining
    # If the emulators are super-certain about their outputs and if the last wave did emulate every output,
    # OR if these emulators proposed a large number of full fit points, then we stop training emulators and
    # simply propose from this wave.
    # This behaviour is superceded by the maximum number of waves (or else you can end up hunting in vain for
    # full fits where none exist; eg GUY)
    if ((all(uncertainty.products < 1.1) && length(ems) == length(target.bounds) && max_imp <= 3) || sum(apply(results.list[[length(results.list)]], 1, in.bounds, target.bounds) == length(target.bounds))/nrow(results.list[[length(results.list)]]) > mine_trigger) {
      emulator.log(msg = "Final wave (emulators have minimal uncertainty or producing many full fits) - looking for full fit points.")
      total.runs <- do.call('rbind', results.list)
      good.runs <- total.runs[which(apply(total.runs, 1, in.bounds, target.bounds) == length(target.bounds)),]
      index <- 1
      # It will continue to mine until it has the desired number of points
      while (nrow(good.runs) < minimum_points) {
        emulator.log(msg = paste("Currently", nrow(good.runs), "full fits have been found."))
        if (nrow(good.runs) != 0) {
          which.hits <- setNames(data.frame(t(apply(good.runs, 1, in.bounds, target.bounds, 'c'))), paste0(names(em.targets), "hit"))
          good.points <- cbind(good.runs, which.hits)
          good.points$nhits <- length(target.bounds)
          out.csv <- gen.csv(good.points, pfile)
          write.csv(out.csv, file = here("countries", opts$country, "output", paste0(RUNID_TS, "_inputPoints.csv")), row.names = FALSE)
        }
        how_many_extra <- 250
        more.points <- generate_new_runs(emulators.list, how_many_extra, em.targets, seek = floor(seek_percent * how_many_extra))
        results.list <- run.model(more.points, i+1+index, results.list)
        index <- index + 1
        total.runs <- do.call('rbind', results.list)
        good.runs <- total.runs[which(apply(total.runs, 1, in.bounds, target.bounds) == length(target.bounds)),]
      }
      which.hits <- setNames(data.frame(t(apply(good.runs, 1, in.bounds, target.bounds, 'c'))), paste0(names(em.targets), "hit"))
      good.points <- cbind(good.runs, which.hits)
      good.points$nhits <- length(target.bounds)
      out.csv <- gen.csv(good.points, pfile)
      write.csv(out.csv, file = here("countries", opts$country, "output", paste0(RUNID_TS, "_inputPoints.csv")), row.names = FALSE)
      break
    }
    i <- i + 1
  }
}

# Ending: if we didn't terminate early, then we write the results to file. If we did kick out early, then
# a failure RData is produced.
if (!kicked_out) {
  swap.files(complete.file.name, backup.file.name)
  emulator.log(msg = paste(i, "waves completed. Check inputPoints.csv for points."))
  if (!file.exists(here("countries", opts$country, "output", paste0(RUNID_TS, "_inputPoints.csv")))) {
    total.runs <- do.call('rbind', results.list)
    good.runs <- total.runs[which(apply(total.runs, 1, in.bounds, target.bounds) == length(target.bounds)),]
    if (nrow(good.runs) == 0) good.runs <- results.list[[length(results.list)]]
    which.hits <- setNames(data.frame(t(apply(good.runs, 1, in.bounds, target.bounds, 'c'))), paste0(names(target.bounds), "hit"))
    good.points <- cbind(good.runs, which.hits)
    good.points$nhits <- apply(good.runs, 1, in.bounds, target.bounds)
    out.csv <- gen.csv(good.points, pfile)
    write.csv(out.csv, file = here("countries", opts$country, "output", paste0(RUNID_TS, "_inputPoints.csv")), row.names = FALSE)
  }
} else {
  swap.files(failure.file.name, backup.file.name)
}