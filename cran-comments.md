## News (Emergency Update!)

While v0.3.1 has just been released on CRAN (2023-03-03),
I found a SERIOUS BUG regarding the use of `data_wordvec_load()`
and have now fixed it (specifically, the `normalized` attribute).
I'd like to update v0.3.2 as soon as possible so that
users will not have problems when they use the package.

## Test environments

-   Windows 11 (local installation), R 4.2.2
-   Mac OS 11.5 (check_mac_release), R 4.2.1

## Package check results

passing `devtools::check_win_devel()`

## R CMD check results

passing (0 errors | 0 warnings | 0 notes)
