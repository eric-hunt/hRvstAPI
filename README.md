# hRvstAPI

## Introduction

I wrote this wrapper as a companion package for a [Shiny application](https://github.com/eric-hunt/hRvst), and it has served as a nice learning opportunity along the way. This wrapper is heavily reliant on the [`keyring`](https://r-lib.github.io/keyring/index.html)[^2], [`httr2`](https://httr2.r-lib.org/)[^1], and [`RSQLite`](https://rsqlite.r-dbi.org/)[^3] packages, so many thanks go to the authors of these great tools.

[^1]: Csárdi G (2022). _keyring: Access the System Credential Store from R_.
  https://r-lib.github.io/keyring/index.html,
  https://github.com/r-lib/keyring#readme.
[^2]: Wickham H (2022). _httr2: Perform HTTP Requests and Process the
  Responses_. https://httr2.r-lib.org, https://github.com/r-lib/httr2.
[^3]: Müller K, Wickham H, James DA, Falcon S (2022). _RSQLite: SQLite
  Interface for R_. https://rsqlite.r-dbi.org,
  https://github.com/r-dbi/RSQLite.

I'd also like to acknowledge that there certainly are other API wrappers for Harvest out there, written in many different languages, and probably much more mature than this one. In the R world, the [`harvestR`](https://github.com/propellerpdx/harvestR) package seems like it could be a great choice if you need to make a lot of queries on the regular, as it leverages parallel processing -- I have not personally used it, however, so this is just a general observation.

This wrapper doesn't use parallel processing for queries, but instead adopts the philosophy of a big download up front (if it is needed) followed by a lot of little updates as time goes on. If this better fits your workflow, give `hRvstAPI` a try.

## Installing this package

You can install the development version of hRvstAPI from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eric-hunt/hRvstAPI")
```

---

<!-- badges: start -->
<!-- badges: end -->

## About this package

The foremost function of this package is to act as a *convenient* wrapper
around the Harvest REST API v2.

To that end, the package handles credentials either through the system keyring
using the `keyring` package (preferred), or through environment variables
written to a local `.Renviron` file. Credential handling can be initiated
explicitly, but otherwise happens automatically upon making an API request.

This package tries to make accessing the Harvest API as easy as possible.
While it does allow some tweaking of the actual HTTP request via function
arguments, the primary method for accessing API data is to simply select
which resource to query (e.g. "users" will download all users data). *Note
that the default is to only download "active" records.* The reason for this
mode of access is that a request usually isn't very big, and the typical user
will probably want to manipulate the data in a more sophisticated manner for 
some downstream visualization or analysis anyway.

Building on this premise, the secondary function of this package is to
manage the long term storage of API data to minimize remote requests. This
is achieved by storing API data in a local SQLite database, for which this
package also provides a straightforward way of accessing and updating with
new data.

There is a companion [Shiny application (`hRvst`)](https://github.com/eric-hunt/hRvst) which utilizes this wrapper
for Harvest API data access, and perhaps slightly enhances this package.

---

<!--
## Using this package

-->

<!--
## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hRvstAPI)
## basic example code
```
