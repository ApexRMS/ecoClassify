## -------------------------------
## ecoClassify - CNN Functions
## ApexRMS, November 2024
## -------------------------------

#' Compute CNN Input Standardization ----
#'
#' @description
#' Mean and standard deviation of each numeric predictor in the training
#' data. The CNN sees `(x - center) / scale`, so predictors in large units
#' (reflectance, elevation) do not dominate training, and first-layer weights
#' (used for variable importance) are comparable across predictors.
#' Constant predictors get a scale of 1.
#'
#' @param X_num Numeric matrix of training predictors (columns named).
#'
#' @return A list with named numeric vectors `center` and `scale`.
#'
#' @noRd
computeCnnScaling <- function(X_num) {
  center <- colMeans(X_num)
  scale <- apply(X_num, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  list(center = center, scale = scale)
}

#' Standardize CNN Numeric Inputs ----
#'
#' @description
#' Applies the training-time standardization saved on the model
#' (`num_center`, `num_scale`). Models trained before standardization was
#' added have neither and are returned unchanged, so they predict as before.
#'
#' @param X_num Numeric matrix whose columns are `model$num_vars`.
#' @param model A CNN model list.
#'
#' @return The standardized matrix.
#'
#' @noRd
standardizeCnnInputs <- function(X_num, model) {
  if (is.null(model$num_center) || ncol(X_num) == 0) {
    return(X_num)
  }
  vars <- colnames(X_num)
  X_num <- sweep(X_num, 2, model$num_center[vars], "-")
  sweep(X_num, 2, model$num_scale[vars], "/")
}

#' Build the ecoClassify CNN Network ----
#'
#' @description
#' The single definition of the network used for training (`getCNNModel()`)
#' and for rebuilding a saved model (`loadCNNModel()`): an embedding per
#' categorical predictor (sized levels + 2 for the NA/unknown buckets), a
#' 16-unit ReLU layer and a 2-class output. Defined inside a function so
#' torch is only needed when a CNN is used. Layer names must not change, or
#' saved weights will no longer load.
#'
#' @param nNum Number of numeric predictors.
#' @param catLevelLengths Integer vector: number of levels per categorical
#'   predictor (may be empty).
#' @param embeddingDims Integer vector: embedding size per categorical
#'   predictor.
#'
#' @return An initialized torch module.
#'
#' @noRd
buildCnnNetwork <- function(nNum, catLevelLengths, embeddingDims) {
  CNNWithEmbeddings <- nn_module(
    "CNNWithEmbeddings",
    initialize = function(n_num, cat_level_lengths, embedding_dims) {
      self$has_cat <- length(cat_level_lengths) > 0
      if (self$has_cat) {
        self$embeddings <- nn_module_list(
          mapply(
            function(l, d) {
              nn_embedding(num_embeddings = l + 2, embedding_dim = d)
            },
            cat_level_lengths,
            embedding_dims,
            SIMPLIFY = FALSE
          )
        )
        embed_dim <- sum(embedding_dims)
      } else {
        embed_dim <- 0
        self$embeddings <- NULL
      }
      self$fc1 <- nn_linear(embed_dim + n_num, 16)
      self$fc2 <- nn_linear(16, 2)
    },
    forward = function(x_num, x_cat) {
      if (self$has_cat && length(x_cat)) {
        embeds <- lapply(
          seq_along(x_cat),
          function(i) self$embeddings[[i]](x_cat[[i]])
        )
        x_cat_emb <- torch_cat(embeds, dim = 2)
        x <- torch_cat(list(x_num, x_cat_emb), dim = 2)
      } else {
        x <- x_num
      }
      x <- nnf_relu(self$fc1(x))
      self$fc2(x)
    }
  )
  CNNWithEmbeddings(nNum, catLevelLengths, embeddingDims)
}

#' Train a Convolutional Neural Network (CNN) ----
#'
#' @description
#' Trains a CNN on numeric and categorical predictors, using embedding layers for factors.
#'
#' @param allTrainData Data frame with predictors and 'presence' column.
#' @param nCores Number of threads to use.
#' @param isTuningOn Logical; use more epochs and batch size if TRUE.
#'
#' @return A list with model, variable importance, and training metadata.
#'
#' @import torch
#' @noRd
getCNNModel <- function(allTrainData, nCores, isTuningOn) {
  torch_set_num_threads(nCores)

  # Check for minimum data size and class balance
  if (nrow(allTrainData) < 2) {
    stop("Insufficient training data (< 2 rows). Check presence balance or NA filtering.")
  }
  if (length(unique(allTrainData$presence)) < 2) {
    stop("Training data must include at least one presence (1) and one absence (0).")
  }

  # Identify categorical and numeric variables
  predictors <- allTrainData[, grep("presence|kfold", colnames(allTrainData), invert = TRUE)]
  cat_vars <- names(predictors)[sapply(predictors, is.factor)]
  num_vars <- setdiff(names(predictors), cat_vars)

  # Validate numeric predictors
  X_num <- as.matrix(predictors[num_vars])
  storage.mode(X_num) <- "double"
  if (anyNA(X_num)) stop("NA values detected in numeric predictors. Ensure complete cases before training.")
  if (nrow(X_num) < 2) stop("Numeric input matrix has fewer than 2 samples after filtering.")

  # Standardize with training statistics (saved with the model and applied
  # identically at every prediction step)
  numScaling <- computeCnnScaling(X_num)
  X_num <- standardizeCnnInputs(
    X_num,
    list(num_center = numScaling$center, num_scale = numScaling$scale)
  )

  # Optional warning for zero-variance predictors
  zero_var <- apply(X_num, 2, function(x) var(x, na.rm = TRUE) == 0)
  if (any(zero_var)) {
    warn_vars <- names(zero_var)[zero_var]
    warning(sprintf("The following numeric predictors have zero variance: %s", paste(warn_vars, collapse = ", ")))
  }

  # Validate and encode categorical variables
  for (v in cat_vars) {
    if (!is.factor(predictors[[v]])) {
      stop(sprintf("Categorical variable '%s' is not a factor in training data.", v))
    }
  }
  # Map NA to the "unknown" bucket at length(levels) + 1, matching the index
  # used for unseen/NA levels at prediction time (see
  # cnnPresenceProbability). Embedding tables are sized length(levels) + 2.
  cat_indices <- if (length(cat_vars)) {
    lapply(predictors[cat_vars], function(x) {
      v <- as.integer(x)
      v[is.na(v)] <- length(levels(x)) + 1L
      v
    })
  } else {
    list()
  }
  cat_level_lengths <- if (length(cat_vars)) {
    lapply(predictors[cat_vars], function(x) length(levels(x)))
  } else {
    list()
  }
  cat_levels <- if (length(cat_vars)) {
    lapply(predictors[cat_vars], function(x) levels(x))
  } else {
    list()
  }
  embedding_dims <- if (length(cat_level_lengths)) {
    lapply(cat_level_lengths, function(l) min(50, floor(l / 2) + 1))
  } else {
    list()
  }
  cat_indices <- as.data.frame(cat_indices)

  y_raw <- allTrainData$presence
  y_int <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.integer(y_raw)

  # Combine categorical and numeric tensors in custom dataset
  ds <- dataset(
    initialize = function(X_num, X_cat, y) {
      self$n     <- nrow(X_num)
      self$X_num <- torch_tensor(X_num, dtype = torch_float())

      if (length(X_cat)) {
        # store each categorical column as a 1-D [n] long tensor
        self$X_cat <- lapply(seq_along(X_cat), function(i) {
          # NA already mapped to the "unknown" bucket when building cat_indices
          v <- as.integer(X_cat[[i]])
          torch_tensor(unname(v), dtype = torch_long())$view(c(self$n))
        })
      } else {
        self$X_cat <- list()
      }

      self$y <- torch_tensor(y + 1L, dtype = torch_long())
    },
    .getitem = function(i) {
      # one sample only
      x_num <- self$X_num[i, ]$view(c(-1))  # [p]

      x_cat <- if (length(self$X_cat)) {
        lapply(self$X_cat, function(x) x[i])  # each is 0-D long (scalar)
      } else {
        list()
      }

      y <- self$y[i]  # 0-D long

      list(x_num = x_num, x_cat = x_cat, y = y)
    },
    .length = function() self$n
  )(X_num, cat_indices, y_int)

  batch_size <- if (isTuningOn) 64 else 32
  epochs <- if (isTuningOn) 100 else 20

  collate_cnn <- function(batch) {
    B <- length(batch)

    # x_num: each element is [p]; stack to [p, B] then transpose -> [B, p]
    x_num <- torch::torch_stack(lapply(batch, `[[`, "x_num"), dim = 2)$t()

    # x_cat: for each cat var j, stack scalars and force shape [B]
    if (length(batch[[1]]$x_cat)) {
      K <- length(batch[[1]]$x_cat)
      x_cat <- lapply(seq_len(K), function(j) {
        torch::torch_stack(lapply(batch, function(s) s$x_cat[[j]]))$view(c(B))
      })
    } else {
      x_cat <- list()
    }

    # y: stack scalars and force shape [B]
    y <- torch::torch_stack(lapply(batch, `[[`, "y"))$view(c(B))

    list(x_num = x_num, x_cat = x_cat, y = y)
  }

  dl <- torch::dataloader(
    ds,
    batch_size = if (isTuningOn) 64L else 32L,
    shuffle    = TRUE,
    # drop_last  = TRUE,      # keep; avoids ragged batch
    # num_workers = 0,        # keep; Windows-safe
    collate_fn = collate_cnn
  )

  net <- buildCnnNetwork(
    nNum = ncol(X_num),
    catLevelLengths = unlist(cat_level_lengths),
    embeddingDims = unlist(embedding_dims)
  )

  device <- torch_device("cpu")
  net <- net$to(device = device)
  opt <- optim_adam(net$parameters, lr = 1e-3)
  lossf <- nn_cross_entropy_loss()

  for (ep in seq_len(epochs)) {
    net$train()
    coro::loop(
      for (b in dl) {
        opt$zero_grad()
        x_num <- b$x_num$to(device = device)
        x_cat <- lapply(b$x_cat, function(x) x$to(device = device))
        y <- b$y$to(device = device)

        logits <- net(x_num, x_cat)
        loss <- lossf(logits, y)
        loss$backward()
        opt$step()
      }
    )
  }

  # Compute variable importance
  vimp <- numeric(length(num_vars) + length(cat_vars))
  names(vimp) <- c(num_vars, cat_vars)

  # For numeric vars: use sum of absolute weights from fc1. Inputs are
  # standardized, so the weights are on a comparable scale across predictors.
  if (length(num_vars)) {
    weights <- net$fc1$weight$data()[, 1:length(num_vars)]$abs()$sum(dim = 1)
    vimp[num_vars] <- as.numeric(weights)
  }

  # For categorical vars: use average norm of embedding weights
  if (length(cat_vars)) {
    emb_norms <- sapply(net$embeddings, function(e) {
      torch_mean(torch_norm(e$weight$data(), p = 2, dim = 2))$item()
    })
    vimp[cat_vars] <- emb_norms
  }

  class(net) <- c("torchCNN", class(net))
  list(
    model = net,
    vimp = vimp,
    feature_names = names(predictors),
    cat_vars = cat_vars,
    num_vars = num_vars,
    cat_level_lengths = cat_level_lengths,
    cat_levels = cat_levels,
    embedding_dims = embedding_dims,
    num_center = numScaling$center,
    num_scale = numScaling$scale
  )
}

#' Predict CNN Presence Probability for a Data Frame ----
#'
#' @description
#' The one place CNN inputs are prepared: numeric predictors are standardized
#' with the training statistics (`standardizeCnnInputs()`), and categorical
#' predictors are coded by their training levels, with NA and unseen levels
#' mapped to the "unknown" index `length(levels) + 1` (embedding tables are
#' sized `length(levels) + 2`).
#'
#' @param model A CNN model list (from `getCNNModel()` or `loadCNNModel()`).
#' @param data Data frame of predictors.
#' @param device torch device to run on.
#' @param warnContext If not NULL, log a warning naming this context when
#'   categorical values were not seen in training.
#'
#' @return Numeric vector of P(presence).
#'
#' @noRd
cnnPresenceProbability <- function(model, data, device = torch_device("cpu"),
                                   warnContext = NULL) {
  num_vars <- model$num_vars
  cat_vars <- model$cat_vars
  cat_levels <- model$cat_levels

  missing_vars <- setdiff(num_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "Missing numeric predictors: %s", paste(missing_vars, collapse = ", ")
    ))
  }

  X_num <- as.matrix(data[, num_vars, drop = FALSE])
  storage.mode(X_num) <- "double"
  X_num <- standardizeCnnInputs(X_num, model)

  X_cat_tensor <- lapply(seq_along(cat_vars), function(i) {
    var <- cat_vars[i]
    levels_train <- cat_levels[[i]]
    # Always re-level to the training order: a factor from terra carries its
    # own level order, so as.integer() would otherwise use the wrong codes.
    x <- factor(as.character(data[[var]]), levels = levels_train)

    if (!is.null(warnContext)) {
      unseen <- sum(is.na(x))
      if (unseen > 0) {
        updateRunLog(sprintf(
          paste0(
            "%s: Variable '%s' contains %d unseen level(s) not present ",
            "during training. They will be handled as a special 'unknown' ",
            "category."
          ),
          warnContext, var, unseen
        ), type = "warning")
      }
    }

    idx <- as.integer(x)
    idx[is.na(idx)] <- length(levels_train) + 1
    torch_tensor(idx, dtype = torch_long())$to(device = device)
  })

  X_num_tensor <- torch_tensor(X_num, dtype = torch_float())$to(
    device = device
  )
  mdl <- model$model$to(device = device)$eval()

  probs_t <- with_no_grad({
    nnf_softmax(mdl(X_num_tensor, X_cat_tensor), dim = 2)$to(
      device = torch_device("cpu")
    )
  })

  # Column 2 is the presence class
  as.matrix(probs_t)[, 2]
}

#' Predict CNN Probabilities from Raster or Tabular Data ----
#'
#' @description
#' Predicts presence probabilities using a trained CNN model, from a data
#' frame (threshold selection and test statistics) or a SpatRaster (maps,
#' processed block by block with `terra::predict()`).
#'
#' @param model A CNN model list.
#' @param newdata A SpatRaster or data frame of predictors.
#' @param isRaster Logical; TRUE if input is raster.
#' @param filename Output file for raster predictions ("" = in memory).
#' @param memfrac Memory fraction for terra's block size.
#'
#' @return A SpatRaster (raster input) or a numeric vector of probabilities.
#'
#' @noRd
predictCNN <- function(model, newdata, isRaster = TRUE, filename = "",
                       memfrac = 0.5, ...) {
  if (!isRaster) {
    df <- as.data.frame(newdata, stringsAsFactors = TRUE)
    drop <- intersect(c("presence", "kfold"), names(df))
    if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]
    if (nrow(df) < 2) {
      stop("Too few rows in numeric predictor matrix (n < 2)")
    }

    dev <- if (cuda_is_available()) torch_device("cuda") else
      torch_device("cpu")
    return(cnnPresenceProbability(model, df, dev, warnContext = "PredictCNN"))
  }

  # Rasters run on the CPU in blocks, to avoid GPU memory issues
  dev <- torch_device("cpu")
  predictFn <- function(m, data, ...) {
    cnnPresenceProbability(model, data, dev)
  }

  predictionRaster <- terra::predict(
    newdata,
    model,
    fun = predictFn,
    na.rm = TRUE,
    cores = 1,  # torch handles its own threading
    memfrac = memfrac,
    filename = filename
  )

  names(predictionRaster) <- "presence"
  return(predictionRaster)
}

#' Load a CNN from Saved Files ----
#'
#' @description
#' Loads a CNN model from weights and metadata files.
#'
#' @param weights_path Path to saved .pt file.
#' @param metadata_path Path to RDS file with metadata.
#'
#' @return A list containing the loaded CNN model and metadata.
#'
#' @noRd
loadCNNModel <- function(weights_path, metadata_path) {
  # Read metadata saved at training time (everything except the torch model)
  metadata <- readRDS(metadata_path)

  # Derive numeric lengths for each categorical variable
  if (!is.null(metadata$cat_level_lengths)) {
    cat_len <- unlist(metadata$cat_level_lengths)
  } else if (!is.null(metadata$cat_levels)) {
    # fallback: count levels if only names were saved
    cat_len <- lengths(metadata$cat_levels)
  } else {
    cat_len <- integer(0)
  }

  # Ensure embedding dims are numeric vector, same order/length as cat_len
  emb_dims <- if (!is.null(metadata$embedding_dims)) unlist(metadata$embedding_dims) else numeric(0)

  # Align by cat_vars ordering if names are available
  if (length(metadata$cat_vars)) {
    # reorder cat_len and emb_dims to match cat_vars if they are named
    if (!is.null(names(cat_len))) {
      cat_len <- cat_len[metadata$cat_vars]
    }
    if (!is.null(names(emb_dims))) {
      emb_dims <- emb_dims[metadata$cat_vars]
    }
  }

  # Basic validation
  if (length(cat_len) != length(emb_dims)) {
    stop(sprintf("Metadata mismatch: %d categorical vars but %d embedding dims.",
                 length(cat_len), length(emb_dims)))
  }

  # Rebuild the architecture used at training, then load its weights
  net <- buildCnnNetwork(
    nNum = length(metadata$num_vars),
    catLevelLengths = as.integer(cat_len),
    embeddingDims = as.integer(emb_dims)
  )
  class(net) <- c("torchCNN", class(net))

  # Load weights (CPU by default)
  sd <- torch_load(weights_path)
  net$load_state_dict(sd)

  # Return the same structure you used elsewhere: model + metadata
  result <- list(
    model = net,
    vimp = metadata$vimp,
    feature_names = metadata$feature_names,
    cat_vars = metadata$cat_vars,
    num_vars = metadata$num_vars,
    cat_level_lengths = metadata$cat_level_lengths,
    cat_levels = metadata$cat_levels,
    embedding_dims = metadata$embedding_dims,
    num_center = metadata$num_center,
    num_scale = metadata$num_scale,
    preprocessing = metadata$preprocessing
  )

  return(result)
}
