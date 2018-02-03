
joClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "joClass",
    inherit = joBase,
    private = list(
        .run = function() {

            if (is.null(self$options$x))
                return()
            
            x <- jmvcore::toNumeric(self$data[[self$options$x]])
            n <- length(x)
            
            data <- list('x' = x, 'N' = n)
            
            model <- '
                model {
                    for (i in 1:N) {
                        x[i] ~ dnorm(mu, tau)
                    }
                    mu ~ dnorm(0, .0001)
                    tau <- pow(sigma, -2)
                    sigma ~ dunif(0, 100)
                }'

            
            model <- rjags::jags.model(file = textConnection(model),
                                data = data,
                                n.chains = 4,
                                n.adapt = 100)
            
            update(model, self$options$samples)
            
            samples <- rjags::jags.samples(model, c('mu', 'tau'), self$options$samples)
            
            self$results$mu$setState(samples$mu)
            self$results$tau$setState(samples$tau)

        },
        .plotTau=function(image, ...) {
            if (is.null(image$state))
                return(FALSE)
            hist(image$state, main='tau', xlab='tau')
            TRUE
        },
        .plotMu=function(image, ...) {
            if (is.null(image$state))
                return(FALSE)
            hist(image$state, main='mu', xlab='mu')
            TRUE
        })
)
