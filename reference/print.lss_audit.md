# Print an `lss_audit` object

Pretty-printed audit summary on the console, capped at the first `n`
findings. Severity-based bullet symbols (errors, warnings, notes) mirror
what is shown in the audit table inside the rendered `.docx`.

## Usage

``` r
# S3 method for class 'lss_audit'
print(x, ..., n = 20L)
```

## Arguments

- x:

  An `lss_audit` object returned by
  [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md).

- ...:

  Currently ignored.

- n:

  Maximum number of findings to print. Defaults to `20`. Set to `Inf` to
  print every finding. The remaining count, when any, is summarized at
  the bottom with a hint to use
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for the
  full list.

## Value

The audit object, invisibly.
