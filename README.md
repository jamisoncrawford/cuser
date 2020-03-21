# Package 'cuser'

R package to streamline (pre)processing tasks for Syracuse-specific, social sector data.

<br>

## Functions

The following functions are currently available in `cuser`.

<br>

### Clean, Filter, & Order DSS Temporary Assistance Data

Function `filter_ta()` processes tables of class `data.frame` by filtering data 
exlusively to the 55 census tracts within Syracuse, NY, and ordering values 
according to Federal Information Processing Standard Publication 6-4 (FIPS) GEOID.

Lastly, `filter_ta()` presevres census tracts which exclude Temporary Assistance 
(TA) data that are not reported by Onondaga County Department of Social Services (DSS).

<br>

### Detect & Validate Syracuse, NY Census Tract Format Variants

Function `expand_ct()` automatically detects the format of any array of Syracuse, NY's 
55 census tracts and returns a table of class `data.frame` with the original data, 
in addition to four alternative formats, as well as providing the option to alphanumeric 
ordering, removal of missing values, and removal of duplicate values.

<br>

## Installation

In order to install and load the `cuser` package in R, use function `install_github()` 
from package `devtools`. The following code should work:

```

if(!require(devtools)){install.packages("devtools")}
library(devtools)

install_github("jamisoncrawford/cuser")
library(cuser)

```

<br>

## Documentation

Use function `help()` or the `?` operator to read internal package documentation for
package `cuser` functions. For example:

```

help(filter_ta)

?expand_ct

```

<br>

## Versions

Package `cuser` version `1.0.0` was officially published on 2020-03-21. 

The following provides new versions, patch notes, and record dates:

* `NA`

<br>

## Contributors

[**Jamison Crawford, MPA**](linkedin.com/in/jamisoncrawford is a data science 
consultant for the [**Central New York Community Foundation**](cnycf.org) and 
is the developer and maintainer of `cuser`.

[**Contact Jamison**](mailto:jamisoncrawford@gmail.com).
