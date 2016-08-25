stat-sampling
=====

Haskell library for monadic composition of probabilistic functions 
and construction of stochastic processes.

Resulting computations can then be run to generate data.

BLog [post](http://kevinl.io/posts/2016-08-17-sampling-monad.html) has some along with examples.
(work in progress).

TODO
-----
[ ] Improve performance
[ ] Upload charts with profiled tests and memory usage.
[ ] Finish README
[ ] Finish blog post
[x] ~~Implement Monte Carlo monad~~ (just used type synonym with WriterT)
[ ] Generate documentation
[ ] Implement learning for generative models
[ ] Extend StochProcess to allow for ARCH models
