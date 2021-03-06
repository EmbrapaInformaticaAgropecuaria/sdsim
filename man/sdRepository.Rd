% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{sdRepository}
\alias{sdRepository}
\title{sdsim Models Repository List}
\usage{
sdRepository()
}
\description{
Show the complete list of models stored in the package sdsim repository.
Use the function \code{\link{sdLoadModel}} with \code{file = 'modelID'} and 
\code{repository = TRUE} to load a model from the sdsim repository.
}
\details{
Models ID and description:
\describe{
\item{\code{Arenstorf}}{The Arenstorf problem, from Astronomy, describes the 
movement of a small body orbiting regularly around two larger objects, such 
as a spacecraft going between the Earth and the Moon.

The two large bodies have mass m1 and m2 and move in a circular rotation 
(coordinates y1 and y2) in a plane, while the third body has negligible mass 
and is moving in the same plane.

It was necessary to solve this problem in order to determine the path that 
the Apollo spacecraft had to take in its journey between the Earth and the 
Moon. The problem was solved by Arenstorf and now it is an often used test 
problem for non-stiff solvers.

Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
R - Springer(2012)}
\item{\code{BouncingBall}}{Simulates a bouncing ball specified by its 
position above the ground (height). The ball is thrown vertically, from the 
ground (height(0) = 0 meter), with initial velocity (speed) of 10 meter / 
second.

As the ball hits the ground, it bounces. This causes a sudden change in the 
value of the ball's velocity (a sign-reversal and reduction of its magnitude 
directly proportional to the coefficient of restitution (k)).

Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
R - Springer(2012)}
\item{\code{Customer}}{In order to demonstrate how a system dynamics model is 
constructed, a one-stock model of an organization's customer base is modeled.

Given that the customer base is an accumulation, it can be modeled as a 
stock. The inflow is recruits, and the outflow are losses, also known as the 
churn rate. The goal of organizations is to limit the losses and maximize the 
recruits, in order to maintain increasing customers levels, and therefore 
support company growth.

Source: Jim Duggan, System Dynamics Modeling with R (2016).}
\item{\code{GrowthConstrained}}{The model captures the growth and decline 
dynamics of a company discovering a new oil field, where the stock of oil 
could potentially last for up to 200 years.

Source: Jim Duggan, System Dynamics Modeling with R (2016).}
\item{\code{RigidBody}}{A standard test problem for non-stiff solvers, as 
proposed by Krogh. It describes the Euler equations of a rigid body without 
external forces.
The three dependent 
variables (x , y , z) are the coordinates of the rotation 
vector, while I1, I2 and I3 are the principal moments of inertia.

Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
R-Springer(2012)}
}
}
\examples{
## Show the models ID present in the sdsim repository
sdRepository()

## Load the Arenstorf model from the sdsim repository 
arenstorf <- sdLoadModel(file = "Arenstorf", repository = TRUE) 

# validate the model, simulate it and plot the coordinates result
arenstorf$verifyModel(verbose = TRUE)
outaren <- sdSimulate(arenstorf)
outaren$plot("y1 ~ y2")
}
