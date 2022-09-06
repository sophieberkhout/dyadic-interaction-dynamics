.meanVar1 <- function(coefs_y, coefs_x) {
    I <- diag(2)
    P <- matrix(c(coefs_y$phi, coefs_y$beta,
                  coefs_x$beta, coefs_x$phi),
        nrow = 2, byrow = TRUE
    )
    C <- matrix(c(coefs_y$alpha, coefs_x$alpha))
    M <- solve(I - P) %*% C
    # m <- m[1, 1]
    return(M)
}