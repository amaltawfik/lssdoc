# lssdoc 0.1.1

* `read_lss()` now validates that the input begins with an XML tag before
  parsing. A non-XML or empty file fails with a clear `lssdoc_invalid_xml`
  error instead of, on some platforms (recent libxml2 builds), aborting the
  R session. Fixes the test ERROR seen on
  `r-devel-linux-x86_64-fedora-gcc`.

# lssdoc 0.1.0

* Initial CRAN release.
