\name{flexscan}
\alias{flexscan}
\title{Flexible Scan Statistics}
\usage{
flexscan(map,case,pop,nsim,k,alpha,isplot,col)
}
\description{
An easy way to conduct flexible scan. Monte-Carlo method is used to test the spatial clusters given the cases, population, and shapefile. A table with formal style and a map with clusters are included in the result report. The method can be referenced at: Toshiro Tango and Kunihiko Takahashi (2005) <doi:10.1186/1476-072X-4-11>.
}
\arguments{
  \item{map}{spatial object, typically a shapefile read in using 'rgdal::readOGR'}
  \item{case}{numeric, a vector of number of cases for each region of 'map'; it is noteworthy that the order of regions in 'case' is corresponding to that in 'map'}
  \item{pop}{numeric, a vector of number of population for each region of 'map'; it is noteworthy that the order of regions in 'pop' is corresponding to that in 'map'}
  \item{nsim}{numeric, the number of simulations for Monte Carlo test; the default is 999}
  \item{k}{numeric, the maximum number of regions allowed for clusters; the default is 10}
  \item{alpha}{numeric, the significance level of flexible scan test; the default is 0.05}
  \item{isplot}{logical, wether to plot the results; the default is 0.05}
  \item{col}{color vector, two colors for most likely cluster and secondary cluster; the default is c("red","blue")}
}
\value{
  \item{data.frame}{a data.frame containing 8 variables as follows:}
  \item{Cluster Type}{most likely cluster or secondary cluster}
  \item{Region ID}{region id for each cluster; it is noteworthy that the 'ID' is the order of regions in 'map'}
  \item{Observed Cases}{observed cases for each cluster}
  \item{Expected Cases}{expected cases for each cluster}
  \item{SR}{standardized ratio of observed to expected cases}
  \item{RR}{relative risk for each cluster}
  \item{LLR}{loglikelihood ratio for each cluster}
  \item{P Value}{p value of likelihood ratio test for each cluster}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Reference:

Tango, T. & Takahashi, K. A Flexibly Shaped Spatial Scan Statistic for Detecting Clusters. INT J HEALTH GEOGR. 4, 11 (2005).

Updates:

Updates is coming.

}
\examples{
  data(map)
  data(sample)
  # simple example for checks; turn the warnings back on using 'options(warn=0)'
  options(warn=-1)
  flexscan(map[111:121,],case=sample$case[111:121],pop=sample$pop[111:121],k=3,isplot=FALSE,nsim=10)
\dontrun{
 flexscan(map,case=sample$case,pop=sample$pop)
}
}
