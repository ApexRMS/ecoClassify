## -------------------------------
## ecoClassify - CNN Functions
## ApexRMS, November 2024
## -------------------------------

#' Install and load the torch package ----
#'
#' @description
#' 'install_and_load_torch' installs the `torch` R package if not present,
#' loads it, and ensures the backend is initialized. If loading fails,
#' it retries a configurable number of times and reinstalls the backend if necessary.
#'
#' @param max_attempts The number of retry attempts to load the backend (default = 3).
#' @param wait_seconds Seconds to wait between attempts (default = 5).
#' @return None. Loads `torch` package and initializes the backend.
#'
#' @details
#' Used during package initialization to ensure deep learning models
#' using `torch` can be executed reliably.
#' @noRd
install_and_load_torch <- function(max_attempts = 3, wait_seconds = 5) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    install.packages("torch", repos = 'http://cran.us.r-project.org')
  }

  Sys.setenv(TORCH_INSTALL = "1")

  if (!torch::torch_is_installed()) {
    torch::install_torch()
  }

  # Try loading backend up to `max_attempts` times
  attempt <- 1
  while (attempt <= max_attempts) {
    try(
      {
        suppressPackageStartupMessages(library(torch))
        torch::torch_tensor(1) # forces backend to load
        return(invisible(TRUE))
      },
      silent = TRUE
    )

    updateRunLog(
      sprintf(
        "Attempt %d failed to load torch backend. Retrying in %d seconds...",
        attempt,
        wait_seconds
      ),
      type = "info"
    )
    Sys.sleep(wait_seconds)
    attempt <- attempt + 1
  }

  updateRunLog("Torch backend still not ready. Reinstalling...", type = "info")
  unlink(
    get("torch_home", envir = asNamespace("torch"))(),
    recursive = TRUE,
    force = TRUE
  )
  torch::install_torch()
  suppressPackageStartupMessages(library(torch))
  torch::torch_tensor(1)
}

install_and_load_torch()

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

  # Identify categorical and numeric variables
  predictors <- allTrainData[, grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = T
  )]
  cat_vars <- names(predictors)[sapply(predictors, is.factor)]
  num_vars <- setdiff(names(predictors), cat_vars)

  # Map factor levels to integers
  cat_indices <- lapply(predictors[cat_vars], function(x) as.integer(x))
  cat_levels <- if (length(cat_vars))
    lapply(predictors[cat_vars], function(x) length(levels(x))) else list()
  embedding_dims <- if (length(cat_levels))
    lapply(cat_levels, function(l) min(50, floor(l / 2) + 1)) else list()
  cat_indices <- as.data.frame(cat_indices)
  X_num <- as.matrix(predictors[num_vars])

  y_raw <- allTrainData$presence
  y_int <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.integer(y_raw)

  # Combine categorical and numeric tensors in custom dataset
  ds <- dataset(
    initialize = function(X_num, X_cat, y) {
      self$X_num <- torch_tensor(X_num, dtype = torch_float())
      self$X_cat <- lapply(
        X_cat,
        function(x) torch_tensor(x, dtype = torch_long())
      )
      self$y <- torch_tensor(y + 1L, dtype = torch_long())
    },
    .getitem = function(i) {
      cat_feats <- lapply(self$X_cat, function(x) x[i])
      list(x_num = self$X_num[i, ], x_cat = cat_feats, y = self$y[i])
    },
    .length = function() self$X_num$size()[1]
  )(X_num, cat_indices, y_int)

  batch_size <- if (isTuningOn) 64 else 32
  epochs <- if (isTuningOn) 100 else 20
  dl <- dataloader(ds, batch_size = batch_size, shuffle = TRUE)

  net <- nn_module(
    "CNNWithEmbeddings",
    initialize = function(n_num, cat_levels, embedding_dims) {
      self$has_cat <- length(cat_levels) > 0
      if (self$has_cat) {
        self$embeddings <- nn_module_list(
          mapply(
            function(l, d)
              nn_embedding(num_embeddings = l + 2, embedding_dim = d),
            cat_levels,
            embedding_dims,
            SIMPLIFY = FALSE
          )
        )
        embed_dim <- sum(unlist(embedding_dims))
      } else {
        embed_dim <- 0
        self$embeddings <- NULL
      }
      self$fc1 <- nn_linear(embed_dim + n_num, 16)
      self$fc2 <- nn_linear(16, 2)
    },
    forward = function(x_num, x_cat) {
      if (self$has_cat) {
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
  )(ncol(X_num), unlist(cat_levels), unlist(embedding_dims))

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

  # For numeric vars: use sum of absolute weights from fc1
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
    cat_levels = cat_levels,
    embedding_dims = embedding_dims
  )
}

#' Predict Using a CNN Model ----
#'
#' @description
#' Predicts presence probabilities using a trained CNN model on raster or tabular data.
#'
#' @param model A torchCNN object.
#' @param newdata A SpatRaster or dataframe of predictors.
#' @param isRaster Logical; TRUE for raster input.
#'
#' @return A SpatRaster or numeric vector of predicted probabilities.
#'
#' @noRd
predictCNN <- function(model, newdata, isRaster = TRUE, ...) {
  if (isRaster) {
    df_full <- as.data.frame(newdata, xy = FALSE, cells = FALSE, na.rm = FALSE)
    valid_idx <- complete.cases(df_full)
    df <- df_full[valid_idx, , drop = FALSE]
  } else if (is.data.frame(newdata) || is.matrix(newdata)) {
    df <- as.data.frame(newdata, stringsAsFactors = TRUE)
    drop <- intersect(c("presence", "kfold"), names(df))
    if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]
    valid_idx <- rep(TRUE, nrow(df))
  } else {
    stop(
      "`newdata` must be a SpatRaster or a data.frame / matrix of predictors"
    )
  }

  # Extract numeric and categorical variables
  num_vars <- model$num_vars
  cat_vars <- model$cat_vars
  cat_levels <- model$cat_levels

  X_num <- as.matrix(df[, num_vars, drop = FALSE])
  storage.mode(X_num) <- "double"

  # Convert categorical variables to integer indices
  X_cat <- lapply(seq_along(cat_vars), function(i) {
    f <- factor(df[[cat_vars[i]]], levels = seq_len(cat_levels[[i]]))
    x <- as.integer(f)
    x[is.na(x)] <- cat_levels[[i]] + 1
    x
  })

  # Prepare tensors
  X_num_tensor <- torch_tensor(X_num, dtype = torch_float())
  X_cat_tensor <- lapply(
    X_cat,
    function(x) torch_tensor(x, dtype = torch_long())
  )

  # Predict
  dev <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")
  mdl <- model$model$to(device = dev)$eval()

  X_num_tensor <- X_num_tensor$to(device = dev)
  X_cat_tensor <- lapply(X_cat_tensor, function(x) x$to(device = dev))

  probs_t <- with_no_grad({
    logits <- mdl(X_num_tensor, X_cat_tensor)
    nnf_softmax(logits, dim = 2)$to(device = torch_device("cpu"))
  })

  mat <- as.matrix(probs_t)
  colnames(mat) <- c("absent", "presence")

  if (isRaster) {
    outR <- newdata[[1]]
    pred_vals <- rep(NA, terra::ncell(outR))
    pred_vals[valid_idx] <- mat[, "presence"]
    terra::values(outR) <- pred_vals
    names(outR) <- "presence"
    return(outR)
  } else {
    return(mat[, "presence"])
  }
}

#' Save CNN Model as RDS ----
#'
#' @description
#' Converts a CNN model to plain R objects and saves it.
#'
#' @param model A trained torch model.
#' @param path File path to save the RDS.
#'
#' @noRd
saveTorchCNNasRDS <- function(model, path) {
  # 1a) pull out the state dict (a named list of torch_tensors)
  sd <- model$state_dict()
  # 1b) move everything to CPU and convert to plain R arrays
  sd_r <- lapply(sd, function(x) as.array(x$to(device = torch_device("cpu"))))
  # 1c) write to disk
  saveRDS(sd_r, path)
}

#' Load CNN Model from RDS
#'
#' @description
#' Loads a CNN model saved using `saveTorchCNNasRDS`.
#'
#' @param path File path to the RDS object.
#'
#' @return A CNN model restored with weights.
#'
#' @noRd
loadTorchCNNfromRDS <- function(path, n_feat, hidden_chs = 16, device = "cpu") {
  # 2a) read the plain‐R list of arrays
  sd_r <- readRDS(path)

  # 2b) rebuild your module skeleton
  model <- OneByOneCNN(n_feat = n_feat, hidden_chs = hidden_chs)

  # 2c) turn the arrays back into torch_tensors
  sd_t <- lapply(sd_r, function(a) torch_tensor(a, dtype = torch_float()))
  # ensure names line up
  names(sd_t) <- names(sd_r)

  # 2d) load into the model
  model$load_state_dict(sd_t)

  # 2e) move to desired device & eval mode
  dev <- if (device == "cpu") torch_device("cpu") else torch_device(device)
  model <- model$to(device = dev)$eval()

  return(model)
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
  # Define CNNWithEmbeddings inside the function scope
  CNNWithEmbeddings <- nn_module(
    "CNNWithEmbeddings",
    initialize = function(n_num, cat_levels, embedding_dims) {
      self$has_cat <- length(cat_levels) > 0
      if (self$has_cat) {
        self$embeddings <- nn_module_list(
          mapply(
            function(l, d)
              nn_embedding(num_embeddings = l + 2, embedding_dim = d),
            cat_levels,
            embedding_dims,
            SIMPLIFY = FALSE
          )
        )
        embed_dim <- sum(unlist(embedding_dims))
      } else {
        embed_dim <- 0
        self$embeddings <- NULL
      }
      self$fc1 <- nn_linear(embed_dim + n_num, 16)  # <-- Match here
      self$fc2 <- nn_linear(16, 2)                  # <-- Match here
    },
    forward = function(x_num, x_cat) {
      if (self$has_cat) {
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
      x <- self$fc2(x)
    }
  )

  # Load metadata (cat_levels, feature_names, etc.)
  metadata <- readRDS(metadata_path)

  # Reconstruct model architecture
  embedding_dims <- metadata$embedding_dims
  net <- CNNWithEmbeddings(
    n_num = length(metadata$num_vars),
    cat_levels = unlist(metadata$cat_levels),
    embedding_dims = unlist(embedding_dims)
  )
  class(net) <- c("torchCNN", class(net))

  # Load weights
  net$load_state_dict(torch_load(weights_path))

  # Return full modelOut object
  c(list(model = net), metadata)
}

#' Predict CNN from Dataframe ----
#'
#' @description
#' Generates predictions for CNN models using tabular input.
#'
#' @param model Trained CNN model.
#' @param newdata A dataframe of input features.
#' @param return Either "class" or "prob".
#'
#' @return A numeric vector of predicted classes or probabilities.
#'
#' @noRd
predict_cnn_dataframe <- function(model, newdata, return = c("class", "prob")) {
  return <- match.arg(return)

  if (!inherits(model$model, "nn_module")) {
    stop("model$model must be a torch nn_module object")
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame")
  }

  # Extract model metadata
  num_vars <- model$num_vars
  cat_vars <- model$cat_vars
  cat_levels <- model$cat_levels

  # Handle numeric input
  X_num <- as.matrix(newdata[, num_vars, drop = FALSE])
  if (anyNA(X_num)) stop("NA values in numeric predictors not allowed.")
  input_num <- torch_tensor(X_num, dtype = torch_float())

  # Handle categorical input
  input_cat <- lapply(cat_vars, function(var) {
    levels_train <- cat_levels[[var]]
    x <- newdata[[var]]
    if (!is.factor(x)) x <- factor(x, levels = levels_train)
    idx <- as.integer(factor(x, levels = levels_train))
    idx[is.na(idx)] <- length(levels_train) + 1  # handle unknowns
    torch_tensor(idx, dtype = torch_long())
  })

  # Predict using model$model
  net <- model$model
  net$eval()
  with_no_grad({
    output <- net(input_num, input_cat)
  })

  if (return == "prob") {
    probs <- output$softmax(dim = 2)
    return(as_array(probs))
  } else {
    classes <- output$argmax(dim = 2)$to(dtype = torch_int())$cpu()
    return(as_array(classes))
  }
}
