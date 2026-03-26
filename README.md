
# SignalCancellation : An R package for

The library `SignalCancellation` offers the `scrof`, `SCROF`, or `SCRoF`
function for Signal cancellation recovery of factors (Achim, 2024).

<!-- (NEST; Achim, 2017; 2020) to determine the number of dimensions in exploratory factor analysis. It provides a main function `nest()` to carry the analysis, a `plot()` function a many utilit. It has been showed to amongst the best stopping rule to determine the nuber of factor in factor analysis [@Achim21;@Caron25;@Bran24]. -->
<!-- There is many examples of correlation matrices available with the packages and other stopping rules as well, such as `PA()` for parallel analysis or `MAP()` for minimum average partial correlation. -->
<!-- As of version `1.0`, `Rnest` is compatible with the `tidyverse` and the `%>%`. -->

# Installation

The development version can be accessed through GitHub:

``` r
remotes::install_github(repo = "quantmeth/SignalCancellation")
library(SignalCancellation)
```

The CRAN package will be available eventually.

``` r
# installed.packages("SignalCancellation")
# library(SignalCancellation)
```

# Examples

Here is an example using the `ex_4factors_corr` correlation matrix from
the `Rnest` library (Caron, 2025). The factor structure is

<p align="center">
<img src="inst/ex_4factors_corr.png" alt="" width="50%" height="50%" style="display: block; margin: auto;" />
</p>

and the correlation matrix is

$$\begin{bmatrix}
1&.810&.270&.567&.567&.189&&&&&& \\
.810&1&.270&.567&.567&.189&&&&&& \\
.270&.270&1&.189&.189&.063&&&&&& \\
.567&.567&.189&1&.810&.270&&&&&& \\
.567&.567&.189&.810&1&.270&&&&&& \\
.189&.189&.063&.270&.270&1&&&&&& \\
&&&&&&1&.810&.270&.567&.567&.189 \\
&&&&&&.810&1&.270&.567&.567&.189 \\
&&&&&&.270&.270&1&.189&.189&.063 \\
&&&&&&.567&.567&.189&1&.810&.270 \\
&&&&&&.567&.567&.189&.810&1&.270 \\
&&&&&&.189&.189&.063&.270&.270&1 \\
\end{bmatrix}$$

From `ex_4factors_corr`, we can easily generate random data using the
`MASS` packages (Venables & Ripley, 2002).

``` r
set.seed(2)
mydata <- MASS::mvrnorm(n = 240,
                        mu = rep(0, ncol(Rnest::ex_4factors_corr)),
                        Sigma = Rnest::ex_4factors_corr)
```

We can then carry signal cancellation factor analysis (SCFA).

``` r
res <- scfa(mydata)
res$nfactors
```

    ## [1] 4

This output yields the number of factors.

Further development will follow.

<!-- The first output tells hom many factors NEST suggests. We can also consult the summary with -->
<!-- ```{r summarynest} -->
<!-- summary(res) -->
<!-- ``` -->
<!-- We can visualize the results using the generic function `plot()` using the `nest()` output. -->
<!-- ```{r plot, fig.cap="Scree plot of NEST", imgcenter='center'} -->
<!-- plot(res) -->
<!-- ``` -->
<!-- The above figure shows the empirical eigenvalues in blue and the 95^th^ percentile of the sampled eigenvalues. -->
<!-- It is also possible to use a correlation matrix directly. A sample size, `n` must be supplied. -->
<!-- ```{r nest2} -->
<!-- nest(ex_4factors_corr, n = 240) -->
<!-- ``` -->
<!-- The `nest()` function can use with many $\alpha$ values and presents parallel analysis results if desired. -->
<!-- ```{r plot2, fig.cap="Scree plot of NEST with many $\\alpha$", imgcenter='center'} -->
<!-- res <- nest(ex_4factors_corr, n = 120, alpha = c(.01,.025,.05)) -->
<!-- plot(res, p
a = TRUE) -->
<!-- ``` -->
<!-- # Recommended usage -->
<!-- Recommended usage : fiml estimation for correlation matrix and removing unique variables. -->
<!-- ```{r tidynest, warning = FALSE, message = FALSE} -->
<!-- library(dplyr) -->
<!-- ex_3factors_doub_unique %>% -->
<!--   genr8(n = 200) %>%        # to generate simulated data for the example -->
<!--   cor_nest() %>%   -->
<!--   remove_unique() %>%        -->
<!--   nest() %>%  -->
<!--   plot(pa = TRUE) -->
<!-- ``` -->

# How to cite

Caron, P.-O. (2025). *SignalCancellation*.
<https://github.com/quantmeth/SignalCancellation>

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-Achim24" class="csl-entry">

Achim, A. (2024). *Signal cancellation factor analysis*.
<https://doi.org/10.31234/osf.io/h7qwg>

</div>

<div id="ref-Caron25" class="csl-entry">

Caron, P.-O. (2025). A comparison of the next eigenvalue sufficiency
test to other stopping rules for the number of factors in factor
analysis. *Educational and Psychological Measurement*.
<https://doi.org/10.1177/00131644241308528>

</div>

<div id="ref-MASS" class="csl-entry">

Venables, W. N., & Ripley, B. D. (2002). *Modern applied statistics with
S*. Springer. <https://www.stats.ox.ac.uk/pub/MASS4/>

</div>

</div>
