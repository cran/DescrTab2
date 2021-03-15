## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(tidyverse)
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, message=FALSE------------------------------------------------
options(print_format = "html") # or = "word" or "tex", depending on your document type

## -----------------------------------------------------------------------------
dat <- iris[, c("Species", "Sepal.Length")]
dat %<>% mutate(animal= c("Mammal", "Fish") %>% rep(75) %>% factor())
dat %<>% mutate(food= c("fries", "wedges") %>% sample(150, TRUE) %>% factor())

## ---- echo=TRUE---------------------------------------------------------------
library(DescrTab2)

## ---- results='asis', echo=F--------------------------------------------------
descr(dat)

## -----------------------------------------------------------------------------
my_table <- descr(dat)

## -----------------------------------------------------------------------------
my_table$variables$Sepal.Length$results$Total$mean

## ---- results='asis'----------------------------------------------------------
my_table <- descr(dat) %>% print(silent=TRUE)

## ---- results='asis'----------------------------------------------------------
descr(dat, "Species")

## ---- results='asis'----------------------------------------------------------
descr(dat, "Species", group_labels=list(setosa="My custom group label"), var_labels = list(Sepal.Length = "My custom variable label"))

## ---- results='asis'----------------------------------------------------------
descr(dat, "animal")

## ---- results='asis'----------------------------------------------------------
descr(dat %>% select(-"Species"), "animal", test_options = list(exact=TRUE, nonparametric=TRUE))

## ---- results='asis'----------------------------------------------------------
descr(dat %>% select(c("Species", "Sepal.Length")), "Species", test_options = list(nonparametric=TRUE))

## ---- results='asis'----------------------------------------------------------
descr(dat %>% mutate(animal = fct_recode(animal, Before="Fish", After="Mammal")) %>% select(-"Species"), "animal", test_options = list(paired=TRUE, indices=rep(1:75, each=2)))

descr(dat %>% mutate(animal = fct_recode(animal, Before="Fish", After="Mammal"), idx = rep(1:75, each=2)) %>% select(-"Species"), "animal", test_options = list(paired=TRUE, indices="idx" ))

## ---- results='asis'----------------------------------------------------------
descr(dat, "Species", format_summary_stats = list(mean=function(x)formatC(x, digits = 4)) )

## ---- results='asis'----------------------------------------------------------
descr(dat, "Species", summary_stats_cont = list(N = DescrTab2:::.N, Nmiss = DescrTab2:::.Nmiss, mean =
    DescrTab2:::.mean, sd = DescrTab2:::.sd, median = DescrTab2:::.median, min = DescrTab2:::.min, max =
    DescrTab2:::.max))

## ---- results='asis'----------------------------------------------------------
# Create example dataset
dat2 <- iris
dat2$cat_var <- c(1,2) %>% sample(150, TRUE) %>% factor()
dat2 <- dat2[, c("Species", "cat_var")]

descr(dat2, "Species", summary_stats_cat=list(mean=DescrTab2:::.factormean))


## ---- results='asis'----------------------------------------------------------
descr(dat, "Species", format_options = c(combine_mean_sd=TRUE))

## ---- results='asis'----------------------------------------------------------
descr(dat, "animal", format_options = list(print_p = FALSE))

## ---- results='asis'----------------------------------------------------------
descr(dat, "animal", format_options = list(print_CI = FALSE))

## -----------------------------------------------------------------------------
capture.output(print(descr(dat, "Species"))) %>%  knitr::raw_html() # or knitr::raw_tex() for tex

## ---- results='asis'----------------------------------------------------------
descr(iris, "Species", var_options = list(Sepal.Length = list(
  format_summary_stats = list(
    mean = function(x)
      formatC(x, digits = 4)
  ),
  test_options = c(nonparametric = TRUE)
)))

