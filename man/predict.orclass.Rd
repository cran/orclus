\name{predict.orclass}
\alias{predict.orclass}
\title{Subspace clustering based local classification using ORCLUS.}
\description{Assigns clusters and distances and classes for new data according to the intrinsic subspace clusters of an \code{\link{orclass}} classification model.}
\usage{
\method{predict}{orclass}(object, newdata, type = "nearest", ...)
}
\arguments{
  \item{object}{Model resulting from a call of \code{\link{orclass}}.}
  \item{newdata}{A matrix or data frame to be clustered by the given model.}
  \item{type}{Default \code{"nearest"} computes relative class frequencies of nearest cluster as class posterior probabilities.}
  \item{\dots}{Currently not used.}        
}
\details{
For prediction the class distribution of the \code{"nearest"}" cluster is used.
If \code{type = "fuzzywts"} cluster memberships (see e.g. Bezdek, 1981) are computed based on the cluster distances of cluster assignment by  \code{\link{predict.orclus}}. For orclass prediction the class distributions of the clusters are weigthed using the cluster memberships of an observation.
}
\value{
  \item{class}{Vector of predicted class levels.}
  \item{posterior}{Matrix where coloumns contain class posterior probabilities.}
  \item{distances}{A matrix where coloumns are the distances to all cluster centers in the corresponding subspaces for the new data.}
  \item{cluster}{The resulting cluster labels for the new data.}
}
\references{
Aggarwal, C. and Yu, P. (2000): \emph{Finding generalized projected clusters in high dimensional spaces}, 
Proceedings of ACM SIGMOD International Conference on Management of Data, pp. 70-81.

Bezdek, J. (1981): \emph{Pattern recognition with fuzzy objective function algorithms}, Kluwer Academic, Norwell, MA. 
}

\author{Gero Szepannek}
\seealso{\code{\link{orclass}}, \code{\link{orclus}}, \code{\link{predict.orclus}}}

\examples{
# definition of a function for parameterized data simulation
sim.orclus <- function(k = 3, nk = 100, d = 10, l = 4, sd.cl = 0.05, sd.rest = 1, locshift = 1){
  ### input parameters for data generation
  # k           number of clusters
  # nk          observations per cluster
  # d           original dimension of the data
  # l           subspace dimension where the clusters are concentrated
  # sd.cl       (univariate) standard deviations for data generation (within cluster subspace concentration) 
  # sd.rest     standard deviations in the remaining space 
  # locshift    parameter of a uniform distribution to sample different cluster means  

  x <- NULL
  for(i in 1:k){
  # cluster centers
  apts <- locshift*matrix(runif(l*k), ncol = l)  
  # sample points in original space
  xi.original <- cbind(matrix(rnorm(nk * l, sd = sd.cl), ncol=l) + matrix(rep(apts[i,], nk), ncol = l, byrow = TRUE),
                       matrix(rnorm(nk * (d-l), sd = sd.rest), ncol = (d-l)))  
  # subspace generation
  sym.mat <- matrix(nrow=d, ncol=d)
  for(m in 1:d){
    for(n in 1:m){
      sym.mat[m,n] <- sym.mat[n,m] <- runif(1)  
      }
    } 
  subspace <- eigen(sym.mat)$vectors    
  # transformation
  xi.transformed <- xi.original \%*\% subspace
  x <- rbind(x, xi.transformed)
  }  
  clids <- rep(1:k, each = nk)
  result <- list(x = x, cluster = clids)
  return(result)
  }

# simulate data of 2 classes where class 1 consists of 2 subclasses
simdata <- sim.orclus(k = 3, nk = 200, d = 15, l = 4, sd.cl = 0.05, sd.rest = 1, locshift = 1)

x <- simdata$x
y <- c(rep(1,400), rep(2,200))

res <- orclass(x, y, k = 3, l = 4, k0 = 15, a = 0.75)
prediction <- predict(res, x)

# compare results
table(prediction$class, y)

}
\keyword{cluster}
\keyword{classif}
\keyword{multivariate}
