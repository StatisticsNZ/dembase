
# dembase

Data structures and functions for demographic data.

`dembase` provides tools for working with cross-classified data on populations.  The package is still under development.

The main functions are:

* `Counts` and `Values`: create demographic arrays.

* `dplot`: visualise demographic arrays.

* `collapse*`: functions for collapsing demographic arrays in various ways.

* `dbind`: combine demographic arrays.

* `subarray`: extract a part of a demographic array.


`dembase` is a dependency for packages `demest` and `demlife`.

Install `dembase` from github with:
```{r, echo = FALSE}
library(devtools)
install_github("statisticsnz/dembase", build_vignettes = TRUE)
```

---
__Copyright and Licensing__

The package is Crown copyright (c) 2016, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the MIT License (see LICENSE file).

<br /><a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This document is Crown copyright (c) 2016, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
