#### Artificial example with 3 or 4 true components :
library(sca)

## Better Sig (eigen space), but *still* BLAS/Lapack version etc platform diffs
##
mvrnorm <- MASS::mvrnorm # (and we see MASS in sessionInfo)
Sig <- function(p, rho) toeplitz(rho^(seq_len(p)-1L))
rmvN <- function(n,p, rho) mvrnorm(n, mu=rep(0,p), Sigma = Sig(p, rho))

## platform details
sessionInfo()
str(.Machine)

set.seed(253)
## random matrix
mr <- matrix(rnorm(1000), 50, 20)

.proctime00 <- proc.time()

(scr0 <- sca(cor(mr)))
if(FALSE)
dput(sapply(scr0$allcrit[1:5], signif))
acrit5 <-
    list(varpc = c(0.118292, 0.0944164, 0.0913502, 0.0875232, 0.0762098),
         varsc = c(B1 = 0.0472054, D2 = 0.109121, D3 = 0.0858094,
                   D4 = 0.0842553, D5 = 0.0775506),
         cumpc = 0.467791, cumsc = 0.394491, opt = 0.843305)
all.equal(acrit5, scr0$allcrit[1:5], tolerance = 0)
## ^^ to gauge this:
with(scr0,
     stopifnot(
         nblock == 1, ndiff == 4,
         all.equal(acrit5, scr0$allcrit[1:5], tolerance = .001)))


scr <-  sca(cor(mr), q = 5, corblocks = 0.12)
stopifnot(all.equal(scr, scr0))



##- Nr. 1 --- p = 3+2+4 = 9 ------------------

set.seed(1324) # re-setting random seed - still difference 32bit - 64bit
m3b <- cbind(rmvN(512, 3, 0.7),
             rmvN(512, 2, 0.9),
             rmvN(512, 4, 0.8))
## Show near block-structure of cor. matrix :
C3b <- cor(m3b)
symnum(C3b, lower.tri = FALSE) # (already small platform differ.!!)

b01 <- Matrix::bdiag(matrix(TRUE, 3,3),
                     matrix(TRUE, 2,2),
                     matrix(TRUE, 4,4))
stopifnot(identical(b01, as(C3b > 0.4, "CsparseMatrix")))

sc3b <- sca(C3b)
sc3b
## TODO: check for "parts"

sc3c.1 <- sca(C3b, corblocks = 0.1)
## -> gives the 3 "true" block components
stopifnot(all.equal(sc3b, sc3c.1))


##- Nr. 2 --- p = 12+6+2+10 = 30 ------------------

set.seed(21262)
m4b <- cbind(rmvN(500, 12, 0.7),
             rmvN(500,  6, 0.9),
             rmvN(500,  2, 0.9),
             rmvN(500, 10, 0.8))
C4 <- cor(m4b)
## Show near block-structure of cor. matrix
symnum(C4, lower.tri = FALSE) # even here, M1mac differs slightly !
C4[3,6]
sc4b <- sca(C4)
sc4b
## TODO: check for "parts"
stopifnot(with(sc4b, nblock == 5, ndiff == 0))

## Different than sc4b :
sc4c.1 <- sca(C4, corblocks = 0.1)
## -> gives the 4 "true" block components
sc4c.1
## TODO: check for "parts"
stopifnot(with(sc4c.1, nblock == 4, ndiff == 1))
str(sc4c.1)

sc4d <- sca(C4, corblocks = 0.1, invertsigns=TRUE)
##                               ^^^^^^^^^^^^^^^^^ (==> quite a bit worse !)
## TODO: check for "parts"
stopifnot(with(sc4d, nblock == 1, ndiff == 4))
sc4d


cat('Time elapsed: ',proc.time() - .proctime00,'\n')
