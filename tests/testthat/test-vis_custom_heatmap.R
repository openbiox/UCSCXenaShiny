test_that("parse_custom_groups works with valid input", {
  # Test basic parsing
  group_text <- "Group1: Sample1,Sample2,Sample3
Group2: Sample4,Sample5,Sample6"
  
  result <- parse_custom_groups(group_text)
  
  expect_true(is.data.frame(result))
  expect_equal(colnames(result), c("sample", "group"))
  expect_equal(nrow(result), 6)
  expect_true(all(c("Sample1", "Sample2", "Sample3") %in% result$sample))
  expect_equal(sum(result$group == "Group1"), 3)
  expect_equal(sum(result$group == "Group2"), 3)
})

test_that("parse_custom_groups filters by available_samples", {
  group_text <- "Group1: Sample1,Sample2,Sample3
Group2: Sample4,Sample5,Sample6"
  
  available <- c("Sample1", "Sample2", "Sample4")
  result <- parse_custom_groups(group_text, available_samples = available)
  
  expect_equal(nrow(result), 3)
  expect_true(all(result$sample %in% available))
})

test_that("parse_custom_groups handles invalid input", {
  # Empty input
  expect_null(parse_custom_groups(""))
  expect_null(parse_custom_groups(NULL))
  
  # No valid groups
  expect_warning(result <- parse_custom_groups("invalid line without colon"))
  expect_null(result)
})

test_that("parse_custom_groups handles whitespace", {
  group_text <- "  Group1  :  Sample1  ,  Sample2  
  Group2  :  Sample3  ,  Sample4  "
  
  result <- parse_custom_groups(group_text)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  # Check that whitespace was trimmed
  expect_true(all(result$group %in% c("Group1", "Group2")))
})


test_that("prepare_heatmap_data works with basic input", {
  data <- data.frame(
    sample = rep(c("S1", "S2", "S3"), each = 3),
    gene = rep(c("TP53", "KRAS", "PTEN"), 3),
    value = c(1:9)
  )
  
  result <- prepare_heatmap_data(data)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 9)
  expect_true(all(c("sample", "gene", "value") %in% colnames(result)))
})

test_that("prepare_heatmap_data removes NA values", {
  data <- data.frame(
    sample = rep(c("S1", "S2", "S3"), each = 3),
    gene = rep(c("TP53", "KRAS", "PTEN"), 3),
    value = c(1, NA, 3, 4, NA, 6, 7, 8, 9)
  )
  
  result <- prepare_heatmap_data(data)
  
  expect_equal(nrow(result), 7)
  expect_false(any(is.na(result$value)))
})

test_that("prepare_heatmap_data limits features", {
  data <- data.frame(
    sample = rep("S1", 100),
    gene = paste0("Gene", 1:100),
    value = 1:100
  )
  
  result <- prepare_heatmap_data(data, max_features = 10)
  
  expect_equal(length(unique(result$gene)), 10)
})

test_that("prepare_heatmap_data merges with groups", {
  data <- data.frame(
    sample = rep(c("S1", "S2", "S3"), each = 3),
    gene = rep(c("TP53", "KRAS", "PTEN"), 3),
    value = 1:9
  )
  
  groups <- data.frame(
    sample = c("S1", "S2", "S3"),
    group = c("A", "A", "B")
  )
  
  result <- prepare_heatmap_data(data, groups = groups)
  
  expect_true("group" %in% colnames(result))
  expect_equal(unique(result$group), c("A", "B"))
})

test_that("prepare_heatmap_data validates input", {
  # Not a data frame
  expect_error(prepare_heatmap_data("not a data frame"))
  
  # Missing required columns
  data <- data.frame(sample = "S1", value = 1)
  expect_error(prepare_heatmap_data(data))
  
  # Invalid groups
  data <- data.frame(sample = "S1", gene = "TP53", value = 1)
  expect_error(prepare_heatmap_data(data, groups = "not a data frame"))
})


test_that("generate_custom_heatmap validates input", {
  # Skip if tidyHeatmap not available
  skip_if_not_installed("tidyHeatmap")
  
  # Not a data frame
  expect_error(generate_custom_heatmap("not a data frame"))
  
  # Missing required columns
  data <- data.frame(sample = "S1", value = 1)
  expect_error(generate_custom_heatmap(data))
  
  # Empty data
  data <- data.frame(sample = character(), gene = character(), value = numeric())
  expect_error(generate_custom_heatmap(data))
})

test_that("generate_custom_heatmap creates basic heatmap", {
  # Skip if tidyHeatmap not available
  skip_if_not_installed("tidyHeatmap")
  
  data <- data.frame(
    sample = rep(paste0("S", 1:5), each = 5),
    gene = rep(paste0("Gene", 1:5), 5),
    value = rnorm(25, mean = 5, sd = 2)
  )
  
  result <- generate_custom_heatmap(data)
  
  # Check that result is not NULL and is some kind of heatmap object
  expect_true(!is.null(result))
})

test_that("generate_custom_heatmap works with groups", {
  # Skip if tidyHeatmap not available
  skip_if_not_installed("tidyHeatmap")
  
  data <- data.frame(
    sample = rep(paste0("S", 1:6), each = 5),
    gene = rep(paste0("Gene", 1:5), 6),
    value = rnorm(30, mean = 5, sd = 2),
    group = rep(c("A", "B"), each = 15)
  )
  
  result <- generate_custom_heatmap(data)
  
  # Check that result is not NULL
  expect_true(!is.null(result))
})


test_that("vis_custom_heatmap works end-to-end", {
  # Skip if tidyHeatmap not available
  skip_if_not_installed("tidyHeatmap")
  
  # Create sample data
  data <- data.frame(
    sample = rep(paste0("Sample", 1:10), each = 5),
    gene = rep(paste0("Gene", 1:5), 10),
    value = rnorm(50, mean = 5, sd = 2)
  )
  
  # Without groups
  result <- vis_custom_heatmap(data)
  expect_true(!is.null(result))
  
  # With groups as string
  group_text <- "Group1: Sample1,Sample2,Sample3,Sample4,Sample5
Group2: Sample6,Sample7,Sample8,Sample9,Sample10"
  
  result <- vis_custom_heatmap(data, groups = group_text)
  expect_true(!is.null(result))
  
  # With groups as data frame
  groups_df <- data.frame(
    sample = paste0("Sample", 1:10),
    group = rep(c("A", "B"), each = 5)
  )
  
  result <- vis_custom_heatmap(data, groups = groups_df)
  expect_true(!is.null(result))
})
