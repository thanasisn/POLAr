
# POLAr


A package for R language with some functions to read data from Polar watch files.
Parses files used by "Polar Pro Trainer" software.

Things can do:
- Parse `.pdd` exercise files
- Read the user file `.ppd`

Also can aggregate some data by time span. Probably `data.table` is better for that, I do not need to use these functions anymore.

It was written many years ago and my R coding style was very poor and inefficient.
Although, I still use it as intended, as I still keep my data in that old format.

## Request

It will be nice if it could be expanded to be able to read heart rate files `.hrm`
Some work have been done already in `read_polar_hrm.R`.

Still I haven't found an existing way to combine Polar data (`.gpx` and `.hrm`) to other formats like `.tcx`.
If you have any suggestion on this, I would love to hear it.

*Suggestions and improvements are always welcome.*

*I use those regular, but they have their quirks, may broke and maybe superseded by other tools.*
