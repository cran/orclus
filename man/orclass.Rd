\name{orclass}
\alias{orclass}
\alias{orclass.default}
\alias{orclass.formula}
\alias{print.orclass}
\title{Subspace clustering based local classification using ORCLUS.}
\description{Function to perform local classification where the subclasses are concentrated in different subspaces of the data.}
\usage{
orclass(x, ...)
\method{orclass}{default}(x, grouping, k, l, k0, a = 0.5, prior = NULL, inner.loops = 1, 
                          predict.train = "nearest", verbose = TRUE, ...)
\method{orclass}{formula}(formula, data = NULL, ...)

}
\arguments{
  \item{x}{A matrix or data frame containing the explanatory variables. The method is restricted to numerical data.}
  \item{grouping}{A factor specifying the class for each observation.}
  \item{formula}{A formula of the form \code{grouping ~ x1 + x2 + ...} That is, the response is the grouping factor and the right hand side specifies the (non-factor) discriminators.}
  \item{data}{Data frame from which variables specified in formula are to be taken.}  \item{k}{Prespecifies the final number of clusters.}  
  \item{l}{Prespecifies the dimension of the final cluster-specific subspaces (equal for all clusters).}
  \item{k0}{Initial number of clusters (that are computed in the entire data space). Must be greater than \code{k}. 
            The number of clusters is iteratively decreased by factor \code{a} until the final number of \code{k} clusters is reached.}  
  \item{a}{Prespecified factor for the cluster number reduction in each iteration step of the algorithm.}
  \item{prior}{Argument for optional specification of class prior probabilities if different from the relative class frequencies.}
  \item{inner.loops}{Number of repetitive iterations (i.e. recomputation of clustering and cluster-specific subspaces) 
           while the number of clusters and the subspace dimension are kept constant.}  
  \item{predict.train}{Character pecifying whether prediction of training data should be pursued. If \code{"nearest"} the class distribution in \code{orclus} cluster assignment is used for classification.}           
  \item{verbose}{Logical indicating whether the iteration process sould be displayed.}
  \item{\dots}{Currently not used.}
}
\details{
For each cluster the class distribution is computed.
}
\value{
  Returns an object of class \code{orclass}.  
  \item{orclus.res}{Object of class \code{orclus} containing the resulting clusters.}
  \item{cluster.posteriors}{Matrix of clusterwise class posterior probabilities where clusters are rows and classes are coloumns.}
  \item{cluster.priors}{Vector of relative cluster frequencies weighted by class priors.}
  \item{purity}{Statistics indicating the discriminability of the identified clusters.}
  \item{prior}{Vector of class prior probabilities.}
  \item{predict.train}{Prediction of training data if specified.}
  \item{orclass.call}{(Matched) function call.}
}
\references{
Aggarwal, C. and Yu, P. (2000): \emph{Finding generalized projected clusters in high dimensional spaces}, 
Proceedings of ACM SIGMOD International Conference on Management of Data, pp. 70-81.
}

\author{Gero Szepannek}
\seealso{\code{\link{predict.orclass}}, \code{\link{orclus}}, \code{\link{predict.orclus}}}

\examples{
# definition of a function for parameterized data simulation
sim.orclus <- function(k = 3, nk = 100, d = 10, l = 4, 
                       sd.cl = 0.05, sd.rest = 1, locshift = 1){
  ### input parameters for data generation
  # k           number of clusters
  # nk          observations per cluster
  # d           original dimension of the data
  # l           subspace dimension where the clusters are concentrated
  # sd.cl       (within cluster subspace) standard deviations for data generation 
  # sd.rest     standard deviations in the remaining space 
  # locshift    parameter of a uniform distribution to sample different cluster means  

  x <- NULL
  for(i in 1:k){
  # cluster centers
  apts <- locshift*matrix(runif(l*k), ncol = l)  
  # sample points in original space
  xi.original <- cbind(matrix(rnorm(nk * l, sd = sd.cl), ncol=l) + matrix(rep(apts[i,], nk), 
                              ncol = l, byrow = TRUE),
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
simdata <- sim.orclus(k = 3, nk = 200, d = 15, l = 4, 
                      sd.cl = 0.05, sd.rest = 1, locshift = 1)

x <- simdata$x
y <- c(rep(1,400), rep(2,200))

res <- orclass(x, y, k = 3, l = 4, k0 = 15, a = 0.75)
res

# compare results
table(res$predict.train$class, y)
}
\keyword{cluster}
\keyword{classif}
\keyword{multivariate}
