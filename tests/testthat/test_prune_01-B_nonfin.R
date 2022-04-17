
# declarations ------------------------------------------------------------

id_new <- "notfin"



# tests -------------------------------------------------------------------

test_that("prune_notfin",
  {
    eg <- data.frame(
      alpha = 1:5,
      beta = c(1, NA_real_, NaN, -Inf, Inf),
      prune_id = factor(NA_character_)
    )
    # cat("\n", crayon::bold$yellow("eg"), "\n")
    # print(eg)


    target <- eg
    target$prune_id <- factor(c(NA_character_,
                                id_new, id_new, id_new, id_new))
    # cat("\n", crayon::bold$yellow("target"), "\n")
    # print(target)

    test <- prune_notfin(eg, var = "beta", is_na = TRUE, is_inf = TRUE)
    # cat("\n", crayon::bold$yellow("test"), "\n")
    # print(test)

    expect_identical(test, target)


    target <- eg
    target$prune_id <- factor(c(NA_character_,
                                id_new, id_new,
                                NA_character_, NA_character_))

    test <- prune_notfin(eg, var = "beta", is_na = TRUE, is_inf = FALSE)

    expect_identical(test, target)


    target <- eg
    target$prune_id <- factor(c(NA_character_,
                                NA_character_, NA_character_,
                                id_new, id_new))

    test <- prune_notfin(eg, var = "beta", is_na = FALSE, is_inf = TRUE)


    expect_identical(test, target)

    }
  )

test_that("prune_notfin: Error all not finite",
          {

            eg <- data.frame(
              alpha = 1:5,
              beta = c(NA_real_, NA_real_, NaN, -Inf, Inf),
              prune_id = factor(NA_character_)
            )

            expect_error(prune_notfin(eg, var = "beta"),
                         class = "prune_notfin_error")


          })
