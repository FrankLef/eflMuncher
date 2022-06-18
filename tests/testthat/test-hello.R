test_that("Hello returns a message to console",
          {
            skip("hello")
            expect_message(object = hello(), regexp = "^Hello.*")
          })

test_that("Hello returns a string invisibly",
          {
            # see example in testthat::expect_invisible() documentation
            out <- expect_invisible(call = hello())
            skip("hello")
            expect_match(object = out, regexp = "^Hello.*")
          })

test_that("Hello returns an error",
          {
            skip("hello")
            expect_error(object = hello("wrong"), class = "hello_error")
          })
