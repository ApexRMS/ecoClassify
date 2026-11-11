## Unit tests for src/functions/0.2-training-and-prediction.r

test_that("getSensSpec computes confusion-matrix rates", {
  source_ecoclassify_r("0.2-training-and-prediction.r")

  probs <- c(0.9, 0.8, 0.3, 0.2, 0.7, 0.1)
  actual <- c(1, 1, 1, 0, 0, 0)
  # threshold 0.5 -> predicted 1,1,0,0,1,0 : TP=2 FN=1 TN=2 FP=1
  m <- getSensSpec(probs, actual, threshold = 0.5)

  expect_equal(unname(m["sens"]), 2 / 3)
  expect_equal(unname(m["spec"]), 2 / 3)
  expect_equal(unname(m["prec"]), 2 / 3)
  expect_equal(unname(m["acc"]), 4 / 6)
  expect_equal(unname(m["bal"]), 2 / 3)
})

test_that("getSensSpec drops NA pairs and returns NA when nothing is left", {
  source_ecoclassify_r("0.2-training-and-prediction.r")

  m <- getSensSpec(c(0.9, NA, 0.1), c(1, 1, NA), threshold = 0.5)
  expect_equal(unname(m["sens"]), 1)

  allNa <- getSensSpec(c(NA, NA), c(1, 0), threshold = 0.5)
  expect_true(all(is.na(allNa)))
})

test_that("reclassifyRaster thresholds probabilities to 0/1", {
  source_ecoclassify_r("0.2-training-and-prediction.r")

  r <- make_test_raster(nrows = 1, ncols = 4)
  terra::values(r) <- c(0.1, 0.5, 0.49, 0.9)

  out <- reclassifyRaster(r, threshold = 0.5)
  expect_equal(as.vector(terra::values(out)), c(0, 1, 0, 1))
})

test_that("getOptimalThreshold picks a threshold that separates a clean random forest fit", {
  source_ecoclassify_r("0.2-training-and-prediction.r")
  skip_if_not_installed("ranger")

  set.seed(3)
  n <- 200
  x <- c(stats::rnorm(n / 2, 0), stats::rnorm(n / 2, 4))
  presence <- factor(rep(c("absence", "presence"), each = n / 2),
                     levels = c("absence", "presence"))
  df <- data.frame(x = x, presence = presence)

  model <- list(model = ranger::ranger(presence ~ x, data = df, probability = TRUE,
                                       num.trees = 50))

  for (objective in c("Youden", "Accuracy", "Balanced")) {
    th <- getOptimalThreshold(model, df, "Random Forest", objective)
    expect_true(th > 0 && th < 1)

    preds <- predict(model$model, df)$predictions[, "presence"]
    acc <- mean((preds >= th) == (df$presence == "presence"))
    expect_gt(acc, 0.9)
  }
})

test_that("getOptimalThreshold rejects unknown model types and objectives", {
  source_ecoclassify_r("0.2-training-and-prediction.r")

  df <- data.frame(x = 1:3, presence = factor(c(0, 1, 0)))
  expect_error(getOptimalThreshold(list(), df, "Not a model"), "not recognized")
  expect_error(getOptimalThreshold(list(), df, "Random Forest", "NotAnObjective"))
})

test_that("splitTrainTest returns train and test sets with both classes", {
  source_ecoclassify_r("0.2-training-and-prediction.r")

  set.seed(4)
  pair <- make_separable_pair(nrows = 60, ncols = 60)

  split <- suppressWarnings(suppressMessages(
    splitTrainTest(list(pair$image), list(pair$groundTruth), nObs = 400)
  ))

  expect_named(split, c("train", "test", "samplingInfo"), ignore.order = TRUE)
  expect_gt(nrow(split$train), 0)
  expect_gt(nrow(split$test), 0)
  expect_true(all(c("band_1", "band_2", "band_3", "presence") %in% names(split$train)))
  expect_setequal(unique(as.character(split$train$presence)),
                  unique(as.character(split$test$presence)))
  expect_length(unique(split$train$presence), 2)
})
