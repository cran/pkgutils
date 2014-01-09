swd <- function(x) UseMethod("swd")

swd.NULL <- function(x) {
  if (!exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  message(getwd())
}

swd.character <- function(x) {
  if (identical(wd.index <- DIRS$WD_INDEX, 0L) && !exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  for (dir.name in x)
    setwd(dir.name)
  message(new.wd <- getwd())
  old.wd <- DIRS[[sprintf("WD_%i", wd.index)]]
  if (!identical(old.wd, new.wd)) {
    wd.index <- DIRS$WD_INDEX <- wd.index + 1L
    DIRS[[sprintf("WD_%i", wd.index)]] <- new.wd
  }
  invisible(NULL)
}

swd.numeric <- function(x) {
  if (L(x <- as.integer(x)) > 0L) {
    y <- getwd()
    for (i in seq.int(1L, x))
      y <- dirname(y)
  } else {
    wd.index <- DIRS$WD_INDEX + x
    y <- DIRS[[sprintf("WD_%i", wd.index)]]
  }
  swd.character(y)
}

listwd <- function(x) UseMethod("listwd")

listwd.NULL <- function(x) {
  listwd.numeric(10)
}

listwd.numeric <- function(x) {
  if (!exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  y <- -seq.int(pmin(L(x <- as.integer(x)) - 1L, DIRS$WD_INDEX), 0L)
  x <- unlist(mget(sprintf("WD_%i", DIRS$WD_INDEX + y), DIRS))
  message(paste(sprintf("% 3i", y), x, collapse = "\n"))
  invisible(x)
}

