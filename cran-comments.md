## News

This is the latest resubmission to CRAN (Aug-21).

I have fixed all issues that Benjamin Altmann proposed, removed all 503 error URLs in README.md, and removed all possibly misspelled words in DESCRIPTION.

1.  Changed the package title to "Word Embedding Research Framework for Psychological Science".
2.  Added all references of the methods used in this package to the DESCRIPTION file.
3.  Removed unnecessary \dontrun{}, but for `data_transform()` and `data_wordvec_load()`, \dontrun{} remains because they are not designed to download data files but require the users to have a file on their disk. So the example code cannot run and thus needs \dontrun{} rather than \donttest{}. I also explained this in the examples as "please first manually download the xxx file".
4.  Added the `verbose` argument for related functions to allow users to control the printing. In some functions, for simplicity, I used `message()` rather than `cat()` or `print()` for this aim.

## Test environments

-   Windows 11 (local installation), R 4.2.1
-   Mac OS 11.5 (check_mac_release), R 4.2.1

## Package check results

passing `devtools::check_win_devel()`

## R CMD check results

passing (0 errors \| 0 warnings \| 0 notes)
