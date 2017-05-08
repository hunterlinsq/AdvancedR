combn2 <- function (x, m) 
{
        n <- length(x)
        e <- 0
        h <- m
        a <- seq_len(m)
        
        count <- as.integer(round(choose(n, m)))
        out <- matrix(x[a], nrow = m, ncol = count)
        
        if (m > 0) {
                i <- 2L
                nmmp1 <- n - m + 1L
                while (a[1L] != nmmp1) {
                        if (e < n - h) {
                                h <- 1L
                                e <- a[m]
                                j <- 1L
                        }
                        else {
                                e <- a[m - h]
                                h <- h + 1L
                                j <- 1L:h
                        }
                        a[m - h + j] <- e + j
                        r <-  x[a]
                        out[, i] <- r
                        i <- i + 1L
                }
        }
        
        out
}

