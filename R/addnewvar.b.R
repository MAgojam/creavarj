
addnewvarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "addnewvarClass",
    inherit = addnewvarBase,
    private = list(
        #### Member variables ----
        .finalData = NA,
        .newVar = NA,
        .percRank = NA,

        #### Init + run functions ----
        .init = function() {
            private$.finalData <- NULL
            private$.newVar <- NULL
            private$.percRank <- NULL

            if ( is.null(self$options$dep) || is.null(self$options$factor) )
                return()

            #### to do output anything else you need ----

        }, 
        .run=function() {

            dep <- self$options$dep
            factor <- self$options$factor

            if ( is.null(dep) || is.null(factor) )
                return()

            data <- self$finalData

            errorMessage <- .("Column '{name}' contains unused levels (possible only when rows with missing values are excluded)")
            for (name in colnames(data)) {
                column <- data[[name]]
                if (is.factor(column) && any(table(column) == 0))
                    reject(errorMessage, name=name)
            }

            dataB64 <- lapply(data, function(x) {
                if (is.factor(x))
                    levels(x) <- toB64(levels(x))
                return(x)
            })

            private$.errorCheck(dataB64)
            private$.populateOV()

            #### to do output anything else you need ----

        },
        .errorCheck = function(data) {

            dep <- self$options$dep
            factor <- self$options$factor

            if (is.factor(data[[dep]]))
                reject(.('Dependent variable must be numeric'))

            lvls <- base::levels(data[[factor]])
            if (length(lvls) == 1) {
                reject(.("Factor '{factorName}' contains only a single level"), factorName=factorName)
            } else if (length(lvls) == 0) {
                reject(.("Factor '{factorName}' contains no data"), factorName=factorName)
            }

        },
        .populateOV=function() {
            if (self$options$newvarOV && self$results$newvarOV$isNotFilled()) {
                self$results$newvarOV$setRowNums(rownames(self$finalData))
                self$results$newvarOV$setValues(self$newVar)
            }
        }
    ),

    #### Active bindings ----
    active = list(
        ready=function() {
            dep <- self$options$dep
            factor <- self$options$factor

            return( ! is.null(dep)
                    && length(factor) > 0
                    && nrow(self$data) > 0)
        },
        finalData=function() {

            if (self$ready && is.null(private$.finalData)) {

                dep <- self$options$dep
                factor <- self$options$factor

                data <- self$data

                if ( ! is.null(dep))
                    data[[dep]] <- jmvcore::toNumeric(data[[dep]])

                if ( ! is.null(factor)) {
                    data[[factor]] <- as.factor(data[[factor]])
                    if (length(levels(data[[factor]])) <= 1)
                        stop(jmvcore::format(.("Factor '{factor}' needs to have at least 2 levels"), 
                                             factor=factor))
                }

                private$.finalData <- na.omit(data)
            }

            private$.finalData
        },
        percRank=function() {

            if ( self$ready && is.null(private$.percRank) ) {

                dep <- self$options$dep
                factor <- self$options$factor

                data <- self$finalData

                # calculate percentile rank of dep values grouped by factor
                df <- data.frame(dv=data[[dep]], gr=data[[factor]])

                df <- df %>%
                        dplyr::group_by(gr) %>%
                        dplyr::mutate(pr = rank(dv)/length(dv))

                private$.percRank <- df$pr

            }
            private$.percRank
        },
        newVar=function() {
            if (self$ready && is.null(private$.newVar))
                private$.newVar <- self$percRank
            private$.newVar
        }
    )
)
