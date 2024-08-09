
test_that("ExportFrequentistJson() binary data regression test", {
  binary_ma <- rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv") |>
    CleanData() |>
    WrangleUploadData() |>
    Long2Wide() |>
    SwapTrt(CONBI = "binary", trt = "BEVA") |>
    FreqPair("OR", "binary", "both")
  
  readr::write_file(json, "data/frequentist_binary_or.json")
  
  expect_equal(
    as.character(json),
    stringr::str_trim(
      stringr::str_replace_all(
        readr::read_file("data/frequentist_binary_or.json"),
        "\r\n",
        "\n"
      )
    )
  )
})

test_that("ExportFrequentistJson() continuous data regression test", {
  continuous_ma <- rio::import("data/AntiVEGF_Continuous_Pairwise_Long.csv") |>
    CleanData() |>
    WrangleUploadData() |>
    Long2Wide() |>
    SwapTrt(CONBI = "continuous", trt = "BEVA") |>
    FreqPair("MD", "continuous", "both")
  
  json <- ExportFrequentistJson(continuous_ma, "random", "MD")
  
  expect_equal(
    as.character(json),
    stringr::str_trim(
      stringr::str_replace_all(
        readr::read_file("data/frequentist_continuous_md.json"),
        "\r\n",
        "\n"
      )
    )
  )
})
test_that("ExportBayesianJson() binary data regression test", {
  binary_ma <- rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv") |>
    CleanData() |>
    WrangleUploadData() |>
    Long2Wide() |>
    SwapTrt(CONBI = "binary", trt = "BEVA") |>
    FreqPair("OR", "binary", "both")
  
  json <- ExportBayesianJson(binary_ma, "random", "OR")
  
  expect_equal(
    as.character(json),
    stringr::str_trim(
      stringr::str_replace_all(
        readr::read_file("data/bayesian_binary_or.json"),
        "\r\n",
        "\n"
      )
    )
  )
})

test_that("ExportBayesianJson() continuous data regression test", {
  continuous_ma <- rio::import("data/AntiVEGF_Continuous_Pairwise_Long.csv") |>
    CleanData() |>
    WrangleUploadData() |>
    Long2Wide() |>
    SwapTrt(CONBI = "continuous", trt = "BEVA") |>
    FreqPair("MD", "continuous", "both")
  
  json <- ExportBayesianJson(continuous_ma, "random", "MD")
  
  expect_equal(
    as.character(json),
    stringr::str_trim(
      stringr::str_replace_all(
        readr::read_file("data/bayesian_continuous_md.json"),
        "\r\n",
        "\n"
      )
    )
  )
})
