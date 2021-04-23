# SIFT-R (siftr) 📦

Tools for streamlining the use of SIFT-MS data in WACL.

To install, just write the following into your R console.

```
devtools::install_github("jack-davison/siftr")
```

## Important Functions

Currently, the only feature of the package is simplifying the reading-in of SIFT data. The key function is `read_sift`, which simply reads in SIFT data and returns a list of data frames, e.g.:

```
siftr::read_sift("~/Van sampling-drive 1-20210118-121331.csv")
```

If you have multiple SIFT files and are only interested in one of the tables, you can use the `read_many_sift` function with a vector of file paths and a table (one of "time", "meta", "prep_phase", "sample_phase", "phase_mean_values", "intensity_corrected", "time_vs_mass", "concentrations", "analytes" or "summary", defaulting to "concentrations").

```
siftr::read_many_sift(my_files, table = "summary")
```
