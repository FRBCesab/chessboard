## Test environments

* Local
  * Arch Linux 6.5.6-arch2-1, R 4.3.1
  * Windows 11 22H2, R 4.3.1
* GitHub Actions
  * macOS 12.7 21G816, R-release (R 4.3.1)
  * Windows Server 2022 10.0.20348, R-release (R 4.3.1)
  * Ubuntu 22.04.3 LTS, R-devel, R-release (R 4.3.1), R-oldrel
* WinBuilder
  * R-devel
  * R-release
  * R-oldrel
* R-hub
  * Windows Server 2022, R-devel
  * Ubuntu 20.04.1 LTS, R-release, GCC
  * Fedora, R-devel, clang, gfortran

## R CMD check results

0 error | 0 warning | 2 notes

* Notes:
  * Maintainer: 'Nicolas Casajus <nicolas.casajus@fondationbiodiversite.fr>' - New submission
  * Possibly misspelled words in DESCRIPTION: undirected (32:5)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmit comments

* `DESCRIPTION` file: use single quote to mention the package
* `DESCRIPTION` file: replace "It makes easier the creation of..." by 
"It makes the creation of... easier"
* `DESCRIPTION` file: add missing references for methods

* Function `append_edge_lists()`: add missing value (return of the function)
