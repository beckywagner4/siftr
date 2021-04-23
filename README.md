# SIFT-R (siftr) 📦

Tools for streamlining the use of SIFT-MS data in WACL.

To install, just write the following into your R console.

```
devtools::install_github("jack-davison/siftr")
```

Currently, the only function is `read_sift`, which simply reads in SIFT data and returns a list of data frames, e.g.:

```
siftr::read_sift("~/Van sampling-drive 1-20210118-121331.csv")
```
