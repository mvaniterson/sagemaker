context("test-train")

setup_prefix <- function(prefix) {

  ## test algorithm locally

  input_path <- file.path(prefix, 'input/data')
  training_path <- file.path(input_path, 'train')
  output_path <- file.path(prefix, 'output')
  model_path <- file.path(prefix, 'model')
  param_file <- file.path(prefix, 'input/config/hyperparameters.json')
  input_config_file <-
    file.path(prefix, 'input/config/inputdataconfig.json')

  # delete prefix if exists
  unlink(dirname(prefix), recursive = TRUE)

  # create directories
  dir.create(training_path, recursive = TRUE)
  dir.create(output_path, recursive = TRUE)
  dir.create(model_path, recursive = TRUE)
  dir.create(dirname(param_file), recursive = TRUE)

  # copy data
  file.copy(file.path(path.package("sagemaker"), "extdata", "iris.csv"),
            training_path)

  # generate hyperparameters
  write_json(
    list(
      target = "Sepal.Length",
      degree = 2
    ),
    param_file,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # write inputdataconfig
  write_json(list(
    train = list(ContentType = "trainingContentType",
                 TrainingInputMode = "File")
  ),
  input_config_file,
  auto_unbox = TRUE,
  pretty = TRUE)
}


test_that("train returns success", {

  prefix <- tempdir() # NOTE: requires write access TEMP

  setup({
    setup_prefix(prefix)
    })

  print(dir(prefix))

  set.seed(12345)

  train(prefix)

  output_path <- file.path(prefix, 'output')

  expect_true(file.exists(file.path(output_path, 'success')))

  teardown({
    unlink(prefix, recursive=TRUE)
  })

})


