## Notes

### Third submission

> Please always write package names, software names and API
> (application programming interface) names in single quotes in title and description.
> e.g: --> 'RESOURCECODE Hindcast'

This is modified to 'RESOURCECODE' hindcast, since 'hindcast' is
a common word in oceanography and meteorology.

> Please unwrap the examples if they are executable in < 5 sec,
> or replace dontrun{} with \donttest{}. 

The test are modified to run only if the additionnal package
'resourcecodedatabase' is installed.

> Please make sure that you do not change the user's options. -> R/zzz.R 

The '.onLoad()' function do not change the user's options anymore.

> Please always make sure to reset to user's options(), working directory or par()
> -> man/compute_sea_state_1d_spectrum.Rd,
> man/compute_sea_state_2d_spectrum.Rd,
> man/convert_spectrum_2d1d.Rd 

The examples mentionned have been modified in order to reset to user's 'options()'.

### Second submission

>   Size of tarball: 6826272 bytes
> Pls reduce to less than 5 MB for the standard threshold of a CRAN package. 

The size of the archive have been reduced by removing the snapshots from testing.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* The size of the archive have been reduced by removing the snapshots from testing.

## Reverse dependencies

None
