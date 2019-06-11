# Copyright 2017 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"). You may not use this file except in compliance with the License. A copy of the License is located at
#
#     http://aws.amazon.com/apache2.0/
#
# or in the "license" file accompanying this file. This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

#' train mars model
#'
#' @param prefix input path
#'
#' @return writes output to disk
#' @export
#' @importFrom jsonlite read_json
#' @importFrom mda mars
#' @importFrom stats model.matrix
#' @importFrom utils read.csv write.csv
train <- function(prefix = '/opt/ml') {

    input_path <- file.path(prefix, 'input','data')
    output_path <- file.path(prefix, 'output')
    model_path <- file.path(prefix, 'model')
    param_path <- file.path(prefix, 'input', 'config', 'hyperparameters.json')

    # Channel holding training data
    channel_name = 'train'
    training_path <- file.path(input_path, channel_name)

    # Read in hyperparameters
    training_params <- read_json(param_path)

    target <- training_params$target

    if (!is.null(training_params$degree))
        degree <- as.numeric(training_params$degree)
    else
        degree <- 2

    # Bring in data
    training_files <- list.files(path=training_path, full.names=TRUE)
    training_data <- do.call(rbind, lapply(training_files, read.csv))

    # Convert to model matrix
    training_X <- model.matrix(~., data = training_data[, colnames(training_data) != target])

    # Save factor levels for scoring
    factor_levels <- lapply(training_data[, sapply(training_data, is.factor), drop=FALSE],
                            function(x) {levels(x)})

    # Run multivariate adaptive regression splines algorithm
    model <- mars(x=training_X, y=training_data[, target], degree=degree)

    # Generate outputs
    mars_model <- model[!(names(model) %in% c('x', 'residuals', 'fitted.values'))]
    attributes(mars_model)$class <- 'mars'

    print(summary(mars_model))

    save(mars_model, factor_levels,
         file=file.path(model_path, 'mars_model.RData'))

    write.csv(model$fitted.values,
              file = file.path(output_path, 'data', 'fitted_values.csv'),
              row.names=FALSE)

    write('success',
          file=file.path(output_path, 'success'))
}



#' mda scoring function
#'
#' @param prefix input path
#'
#' @return
#' @export
#' @import plumber
serve <- function(prefix = '/opt/ml') {
    app <- plumb(paste(prefix, 'plumber.R', sep='/'))
    app$run(host='0.0.0.0', port=8080)}

