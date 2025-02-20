# Set environment variables to point to installations of geospatial libraries ----

## Amend 'LD_LIBRARY_PATH' ----

# Get the existing value of 'LD_LIBRARY_PATH'
old_ld_path <- Sys.getenv("LD_LIBRARY_PATH")

# Append paths to GDAL and PROJ to 'LD_LIBRARY_PATH'
Sys.setenv(LD_LIBRARY_PATH = paste(
  old_ld_path,
  "/usr/gdal34/lib",
  "/usr/proj81/lib",
  sep = ":"
))

rm(old_ld_path)

## Specify additional proj path in which pkg-config should look for .pc files ----

Sys.setenv("PKG_CONFIG_PATH" = "/usr/proj81/lib/pkgconfig")

## Specify the path to GDAL data ----

Sys.setenv("GDAL_DATA" = "/usr/gdal34/share/gdal")

dyn.load("/usr/gdal34/lib/libgdal.so")
dyn.load("/usr/geos310/lib64/libgeos_c.so", local = FALSE)

# readr options ----
# Use lazy reading for CSVs (should be faster)
# https://www.tidyverse.org/blog/2021/11/readr-2-1-0-lazy/
options(readr.read_lazy = TRUE)
# Don't print col types on read
options(readr.show_col_types = FALSE)

# ggrepel options ----
options("ggrepel.max.overlaps" = 12)
