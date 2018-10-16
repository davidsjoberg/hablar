## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.1
* win-builder 

## R CMD check results Mac and win builder:
There were no ERRORs or WARNINGs or NOTEs.

## R CMD check results Ubuntu:
There were no ERRORs and 1 WARNINGs and no NOTEs.

The warnings on Ubuntu comes from an example in the documentation where base R returns a warning, in contrast to the package's alternative function that does not give a warning.



