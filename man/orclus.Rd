\name{orclus}
\alias{orclus}
\alias{orclus.default}
\alias{print.orclus}
\title{Arbitrarily ORiented projected CLUSter generation}
\description{Function to perform subspace clustering where the clusters are concentrated in different cluster specific subspaces of the data.}
\usage{
orclus(x, ...)
\method{orclus}{default}(x, k, l, k0, a = 0.5, inner.loops = 1, verbose = TRUE, ...)
}
\arguments{
  \item{x}{A matrix or data frame containing the explanatory variables. The method is restricted to numerical data.}
  \item{k}{Prespecifies the final number of clusters.}  
  \item{l}{Prespecifies the dimension of the final cluster-specific subspaces (equal for all clusters).}
  \item{k0}{Initial number of clusters (that are computed in the entire data space). Must be greater than \code{k}. 
            The number of clusters is iteratively decreased by factor \code{a} until the final number of \code{k} clusters is reached.}  
  \item{a}{Prespecified factor for the cluster number reduction in each iteration step of the algorithm.}  
  \item{inner.loops}{Number of repetitive iterations (i.e. recomputation of clustering and cluster-specific subspaces) 
           while the number of clusters and the subspace dimension are kept constant.}  
  \item{verbose}{Logical indicating whether the iteration process sould be displayed.}
  \item{\dots}{Currently not used.}
}
\details{
The function performs ORCLUS subspace clustering (Aggarwal and Yu, 2000). 
Simultaneously both cluster assignments as well as cluster specific subspaces are computed.
Cluster assignments have minimal euclidean distance from the cluster centers in the corresponding subspaces. 
As an extension to the originally proposed algorithm initialization in the full data space is done by calling \code{\link{kmeans}} 
for \code{k0} clusters. Further, by \code{inner.loops} a number of repetitions during the iteration process 
for each number of clusters and subspace dimension can be specified. An outlier option has not been implemented.  
Even though increasing the initialzation parameter \code{k0} most strongly effects the computation time 
it should be chosen as large as possible (at least several times greater then \code{k}).
}
\value{
  Returns an object of class \code{orclus}. Its structure is similar to objects resulting from calling \code{\link{kmeans}}. 
  
  \item{cluster}{Returns the final cluster labels.}
  \item{centers}{A matrix where each row corresponds to a cluster center (in the original space).}
  \item{size}{The final number of observations in each cluster.}
  \item{subspaces}{List of matrices for projection of the data onto the cluster-specific supspaces by post-multiplication.}
  \item{subspace.dimension}{Dimension of the final subspaces.}
  \item{within.projenss}{Corresponds to \code{withinss} of \code{kmeans} objects: projected within cluster energies for each cluster.}
  \item{sparsity.coefficient}{Sparsity coefficient of the clustering result. 
        If its value is close to 1 the subspace dimension may have been chosen too large. 
        A small value close to 0 can be interpreted as a hint that a strong cluster structure has been found.}
  \item{orclus.call}{(Matched) function call.}
}
\references{
Aggarwal, C. and Yu, P. (2000): \emph{Finding generalized projected clusters in high dimensional spaces}, 
Proceedings of ACM SIGMOD International Conference on Management of Data, pp. 70-81.
}

\author{Gero Szepannek}
\seealso{\code{\link{predict.orclus}}}

\examples{
# generate simple artificial example of two clusters
clus1.v1 <- runif(100)
clus2.v1 <- runif(100) 
xample <- rbind(cbind(clus1.v1, 0.5 - clus1.v1), cbind(clus2.v1, -0.5 + clus2.v1))
plot(xample, col=rep(1:2, each=100))

# try standard kmeans clustering
kmeans.res <- kmeans(xample, 2)
plot(xample, col = kmeans.res$cluster)

# use orclus instead 
orclus.res <- orclus(x = xample, k = 2, l = 1, k0 = 8, a = 0.5)
plot(xample, col = orclus.res$cluster)

# show data in cluster-specific subspaces
par(mfrow=c(1,2))
for(i in 1:length(orclus.res$size)) plot(xample \%*\% orclus.res$subspaces[[i]], col = orclus.res$cluster, ylab = paste("Identified subspace for cluster",i))


### second 'more multivariate' example to play with...

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

# simulate data, you can play with different parameterizations...
simdata <- sim.orclus(k = 3, nk = 200, d = 15, l = 4, sd.cl = 0.05, sd.rest = 1, locshift = 1)

# apply kmeans and orclus
kmeans.res2 <- kmeans(simdata$x, 3)
orclus.res2 <- orclus(x = simdata$x, k = 3, l = 4, k0 = 15, a = 0.75)
cat("SC: ", orclus.res2$sparsity.coefficient, "\n")

# compare results
table(kmeans.res2$cluster, simdata$cluster)
table(orclus.res2$cluster, simdata$cluster)
}
\keyword{cluster}
\keyword{classif}
\keyword{multivariate}
