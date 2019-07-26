#' Parameter documentation for all pipelines
#'
#'@param db Location of database file.
#'@param species Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. If blank will return all major species.
#'@param pop The three-letter code of population as listed in the \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. For data owners with multiple populations (e.g. NIOO, UAN) where a single
#'  pipeline is used for many populations this argument is used to extract data from
#'  individual populations. For other pipelines that contain only one population
#'  this argument can be ignored.
#'@param path Location where output csv files will be saved.
#'@param debug For internal use when editing pipelines. If TRUE, pipeline
#'  generates a summary of pipeline data. This includes: a) Histogram of
#'  continuous variables with mean/SD b) unique values of all categorical
#'  variables.
#'
#'@name pipeline_params
NULL

############################################################################
