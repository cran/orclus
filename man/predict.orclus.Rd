\name{predict.orclus}
\alias{predict.orclus}
\title{Arbitrarily ORiented projected CLUSter generation}
\description{Assigns clusters and distances to cluster centers in the corresponding subspaces for new data according to a subspace clustering model of class \code{\link{orclus}}.}
\usage{
predict.orclus(object, newdata, ...)
}
\arguments{
  \item{object}{Model resulting from a call of \code{\link{orclus}}.}
  \item{newdata}{A matrix or data frame to be clustered by the given model.}
  \item{\dots}{Currently not used.}        
}

\value{
  \item{distances}{A matrix where coloumns are the distances to all cluster centers in the corresponding subspaces for the new data.}
  \item{cluster}{The resulting cluster labels for the new data.}
}
\references{
Aggarwal, C. and Yu, P. (2000): \emph{Finding generalized projected clusters in high dimensional spaces}, 
Proceedings of ACM SIGMOD International Conference on Management of Data, pp. 70-81.
}

\author{Gero Szepannek}
\seealso{\code{\link{orclus}}}

\examples{
# generate simple artificial example of two clusters
clus1.v1 <- runif(100)
clus2.v1 <- runif(100) 
xample <- rbind(cbind(clus1.v1, 0.5 - clus1.v1), cbind(clus2.v1, -0.5 + clus2.v1))

orclus.res <- orclus(x = xample, k = 2, l = 1, k0 = 8, a = 0.5)

# generate new data and predict it using the 
newclus1.v1 <- runif(100)
newclus2.v1 <- runif(100) 
true.clusterids  <- rep(1:2, each = 100)
xample2 <- rbind(cbind(newclus1.v1, 0.5 - newclus1.v1), cbind(newclus2.v1, -0.5 + newclus2.v1))

orclus.prediction <- predict(orclus.res, xample2)
table(orclus.prediction$cluster, true.clusterids)

}
\keyword{cluster}
\keyword{classif}
\keyword{multivariate}
