
test_that("Data extracted from example binary file", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "binaryEx")
    
    expect_equal(data(), rio::import("data/AntiVEGF_Binary_Pairwise.csv"))
  })
})

test_that("Data extracted from example continuous file", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "continuousEx")
    
    expect_equal(data(), rio::import("data/AntiVEGF_Continuous_Pairwise.csv"))
  })
})

test_that("Binary data matches between .csv and .xlsx files", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise.csv"))
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise.xlsx"))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Continuous data matches between .csv and .xlsx files", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise.csv"))
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise.xlsx"))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Server returns wrangled binary data", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "binaryEx")
    
    expect_equal(session$returned()$data, WrangleUploadData(data()))
  })
})

test_that("Server returns wrangled continuous data", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "continuousEx")
    
    expect_equal(session$returned()$data, WrangleUploadData(data()))
  })
})
