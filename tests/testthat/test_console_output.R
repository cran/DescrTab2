library(magrittr, quietly = TRUE, warn.conflicts = FALSE)

test_that("numeric output does not produce errors.", {
  expect_error(descr(iris) %>% print(silent = TRUE, print_format = "console"), NA)
  expect_error(
    descr(
      iris,
      "Species",
      group_labels = list(setosa = "My custom group label"),
      var_options = list(Sepal.Length = list(label = "My custom variable label"))
    ) %>% print(silent = TRUE, print_format = "console"),
    NA
  )
})


verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/print_console_single.txt"),
  descr(iris) %>% print(print_format = "console")
)

verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/print_console_group.txt"),
  descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(print_format = "console")
)

DescrPrintObj <- print(descr(iris), silent = TRUE)
test_that("console is default print argument", {
  expect_error(print(descr(iris), print_format = NULL, silent = TRUE), NA)
  expect_error(print(DescrPrintObj, print_format = NULL, silent = TRUE), NA)
})
