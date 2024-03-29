.N <- function(var) {
  sum(!is.na(var))
}

.Nmiss <- function(var) {
  sum(is.na(var))
}

.mean <- function(var) {
  ret <- base::mean(var, na.rm = TRUE)
  if (is.nan(ret)) {
    return(NA_real_)
  } else {
    return(ret)
  }
}

.sd <- function(var) {
  stats::sd(var, na.rm = TRUE)
}

.median <- function(var) {
  stats::median(var, na.rm = TRUE)
}

.Q1 <- function(var) {
  stats::quantile(var,
    probs = 0.25,
    na.rm = TRUE,
    type = 2
  )
}

.Q3 <- function(var) {
  stats::quantile(var,
    probs = 0.75,
    na.rm = TRUE,
    type = 2
  )
}

.IQR <- function(var) {
  stats::quantile(var,
    probs = 0.75,
    na.rm = TRUE,
    type = 2
  ) -
    stats::quantile(var,
      probs = 0.25,
      na.rm = TRUE,
      type = 2
    )
}

.min <- function(var) {
  if (any(!is.na(var))) {
    min(var, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.max <- function(var) {
  if (any(!is.na(var))) {
    max(var, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.mode <- function(var) {
  ux <- unique(var)
  ux[which.max(tabulate(match(var, ux)))]
}

.skew <- function(var) {
  base::mean((var - .mean(var))^(3)) / (stats::sd(var))^(3 / 2)
}

.kurtosis <- function(var) {
  base::mean((var - .mean(var))^(4)) / (stats::sd(var))^(2) - 3
}

.factorN <- function(var){
  sum(var!="(Missing)")
}
.factorNmiss <- function(var){
  sum(var=="(Missing)")
}

.factormean <- function(var) {
  var <- var[var!="(Missing)"]
  ret <- var %>%
    as.character() %>%
    as.numeric() %>%
    mean(na.rm = TRUE)
  if (is.nan(ret)) {
    return(NA_real_)
  } else {
    return(ret)
  }
}

.factorsd <- function(var) {
  var <- var[var!="(Missing)"]
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::sd(na.rm = TRUE)
}

.factormedian <- function(var) {
  var <- var[var!="(Missing)"]
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::median(na.rm = TRUE)
}

.factorQ1 <- function(var) {
  var <- var[var!="(Missing)"]
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::quantile(
      probs = 0.25,
      na.rm = TRUE,
      type = 2
    )
}

.factorQ3 <- function(var) {
  var <- var[var!="(Missing)"]
  var %>%
    as.character() %>%
    as.numeric() %>%
    stats::quantile(
      probs = 0.75,
      na.rm = TRUE,
      type = 2
    )
}

.factormin <- function(var) {
  var <- var[var!="(Missing)"]
  var_num <- var %>%
    as.character() %>%
    as.numeric()
  if (any(!is.na(var))) {
    min(var_num, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.factormax <- function(var) {
  var <- var[var!="(Missing)"]
  var_num <- var %>%
    as.character() %>%
    as.numeric()
  if (any(!is.na(var))) {
    max(var_num, na.rm = TRUE)
  } else {
    NA_real_
  }
}

.meanCIlower <- function(var) {
  if (sum(!is.na(var)) > 1 && (length(table(var[!is.na(var)])) > 1)) {
    stats::t.test(var)$conf.int[1]
  } else {
    return(NA_real_)
  }
}

.meanCIupper <- function(var) {
  if (sum(!is.na(var)) > 1 && (length(table(var[!is.na(var)])) > 1)) {
    stats::t.test(var)$conf.int[2]
  } else {
    return(NA_real_)
  }
}

.factor_firstlevel_CIlower <- function(var) {
  var <- var[!is.na(var) & !(var=="(Missing)")]
  var <- factor(var,
                levels = setdiff(levels(var), "(Missing)"),
                labels = setdiff(levels(var), "(Missing)"))
  if (length(var)>1) {
    var <- var[!is.na(var) | !(var=="(Missing)")]
    stats::prop.test(sum(var == levels(var)[1]), length(var))$conf.int[1]
  } else {
    NA_real_
  }
}

.factor_firstlevel_CIupper <- function(var) {
  var <- var[!is.na(var) & !(var=="(Missing)")]
  var <- factor(var,
                levels = setdiff(levels(var), "(Missing)"),
                labels = setdiff(levels(var), "(Missing)"))

  if (length(var)>1) {
    stats::prop.test(sum(var == levels(var)[1]), length(var))$conf.int[2]
  } else {
    NA_real_
  }
}

.factor_lastlevel_CIlower <- function(var) {
  var <- var[!is.na(var) & !(var=="(Missing)")]
  var <- factor(var,
                levels = setdiff(levels(var), "(Missing)"),
                labels = setdiff(levels(var), "(Missing)"))
  if (length(var)>1) {
    stats::prop.test(sum(var == levels(var)[length(levels(var))]), length(var))$conf.int[1]
  } else {
    NA_real_
  }
}

.factor_lastlevel_CIupper <- function(var) {
  var <- var[!is.na(var) & !(var=="(Missing)")]
  var <- factor(var,
                levels = setdiff(levels(var), "(Missing)"),
                labels = setdiff(levels(var), "(Missing)"))
  if (length(var)>1) {
    stats::prop.test(sum(var == levels(var)[length(levels(var))]), length(var))$conf.int[2]
  } else {
    NA_real_
  }
}

.HLCIlower <- function(var) {
  if (!is.numeric(var)){
    var <- var %>%
      as.character() %>%
      as.numeric()
  }
  if (any(!is.na(var))) {
    var <- var[!is.na(var)]
    conds <- list()
    ret <- withCallingHandlers(
      {
        stats::wilcox.test(var, conf.int = TRUE)$conf.int[1]
      },
      condition = function(cond) {
        conds <<- append(conds, cond)
        if (inherits(cond, "warning")) {
          tryInvokeRestart("muffleWarning")
        }
      }
    )
    for (cond in conds[names(conds) == "message"]) {
      if (cond == "requested conf.level not achievable") {
        warning(cond)
        return(NA_real_)
      }
    }
    return(ret)
  } else {
    NA_real_
  }
}

.HLCIupper <- function(var) {
  if (!is.numeric(var)){
    var <- var %>%
      as.character() %>%
      as.numeric()
  }
  if (any(!is.na(var))) {
    var <- var[!is.na(var)]
    conds <- list()
    ret <- withCallingHandlers(
      {
        stats::wilcox.test(var, conf.int = TRUE)$conf.int[2]
      },
      condition = function(cond) {
        conds <<- append(conds, cond)
        if (inherits(cond, "warning")) {
          tryInvokeRestart("muffleWarning")
        }
      }
    )
    for (cond in conds[names(conds) == "message"]) {
      if (cond == "requested conf.level not achievable") {
        warning(cond)
        return(NA_real_)
      }
    }
    return(ret)
  } else {
    NA_real_
  }
}
