## Package 'debug': installation instructions

Hello! I now live in the R-universe. You can install me, plus my dependencies, like this:

```
# Tell R where I live
options(repos = unique( c(
    mvb = 'https://markbravington.r-universe.dev',
    getOption( 'repos')[ 'CRAN']
)))
install.packages( "debug")
```
