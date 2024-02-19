
test_that("Data extracted from example binary file", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "binaryEx", sort_data = FALSE)
    
    expect_equal(data(), rio::import("data/AntiVEGF_Binary_Pairwise.csv"))
  })
})

test_that("Data extracted from example continuous file", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "continuousEx", sort_data = FALSE)
    
    expect_equal(data(), rio::import("data/AntiVEGF_Continuous_Pairwise.csv"))
  })
})

test_that("Binary data matches between .csv and .xlsx files", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise.csv"), sort_data = FALSE)
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise.xlsx"))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Continuous data matches between .csv and .xlsx files", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise.csv"), sort_data = FALSE)
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise.xlsx"))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Server returns wrangled binary data", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "binaryEx", sort_data = FALSE)
    
    expect_equal(session$returned()$data, WrangleUploadData(data()))
  })
})

test_that("Server returns wrangled continuous data", {
  testServer(dataPageServer, {
    session$setInputs(ChooseExample = "continuousEx", sort_data = FALSE)
    
    expect_equal(session$returned()$data, WrangleUploadData(data()))
  })
})

test_that("Should sort binary data", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_unsorted.csv"), sort_data = FALSE)
    unsorted_studies <- unique(data()$Study)
    
    session$setInputs(sort_data = TRUE)
    sorted_studies <- unique(data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("Berg 2015", "Biswas 2011", "Biswas 2011a", "Chakravarthy 2013", "Kodjikian 2013", "Krebs 2013", "Martin 2011", "Schauwvlieghe 2016", "Subramanian 2010"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort continuous data", {
  testServer(dataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_unsorted.csv"), sort_data = FALSE)
    unsorted_studies <- unique(data()$Study)
    
    session$setInputs(sort_data = TRUE)
    sorted_studies <- unique(data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      unique(data()$Study),
      !!c("DRCRnet_2015", "Ekinci_2014", "Nepomuceno_2013", "Wiley_2016"),
      label = format_vector_to_string(colnames(unique(data()$Study)))
    )
  })
})
