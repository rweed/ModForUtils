This utility contains implementations of the Brent ZEROIN root finder and an
alternate solver based on an algorithm by T.R. Chandrupatla described in his paper

  T.R. Chandrupatla, " A new hybrid quadratic/bisection algorithm for
  finding the zero of a nonlinear function without derivatives," Advances in
  Engineering Software, Vol 28, 1997, pp. 145-149.

A series of test functions from this paper are implemented along with a test program. There are two implementations of each root finder. The first ones use an abstract class to pass the required function. The second implementations use a procedure argument defined by a different abstract interface. 
