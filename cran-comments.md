## Submission

This is the first submission of MEWOC.

## Test environments

* Local: Windows 11, R 4.5.2 (`R CMD check --as-cran`, including the PDF manual)
* Local: Windows 11, R 4.5.2, only strong dependencies installed (`_R_CHECK_DEPENDS_ONLY_=true`)
* win-builder: R-devel and R-release (to add)
* macOS builder: R-release (to add)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

Words that may be flagged as misspelled in DESCRIPTION (Babb, Zacks, Rogatko,
Sweeting, Mander, EWOC) are author names and the method's established acronym
(escalation with overdose control).

## System requirements

The package imports 'rjags', which needs JAGS (>= 4.0.0); this is stated in
SystemRequirements. Examples, tests and the vignette use small MCMC settings and
at most 2 cores.
