## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.2 - 3.4
* win-builder (devel and release)
* rhub:
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - Ubuntu Linux 16.04 LTS, R-release, GCC
    - Fedora Linux, R-devel, clang, gfortran
    

On win-builder and rhub windows I get a warning that pandoc cannot
find the README badges. Otherwise, the tests passes.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
