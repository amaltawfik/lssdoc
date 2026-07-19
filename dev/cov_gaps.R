# Report the still-uncovered lines from the most recent dev/run_cov.R run.
#
# Reads dev/_cov.rds (written by dev/run_cov.R) and prints, per source file,
# the exact line numbers missing coverage -- collapsed into ranges -- so gaps
# are easy to locate. Run dev/run_cov.R first.
#
# Run from the package root:
#   Rscript dev/cov_gaps.R

cov <- readRDS("dev/_cov.rds")
tally <- covr::tally_coverage(cov)
agg <- aggregate(value ~ filename, tally, FUN=function(v) c(n=length(v), miss=sum(v==0)))
m <- data.frame(file=basename(agg$filename), n=agg$value[,"n"], miss=agg$value[,"miss"])
tot_n <- sum(m$n); tot_cov <- sum(m$n-m$miss)
cat(sprintf("TOTAL relevant=%d covered=%d pct=%.2f  missing=%d\n\n",
            tot_n, tot_cov, 100*tot_cov/tot_n, tot_n-tot_cov))
z <- covr::zero_coverage(cov); z$bn <- basename(z$filename)
rng <- function(f){ ln<-sort(unique(z$line[z$bn==f])); if(!length(ln)) return(invisible())
  brk<-c(0,which(diff(ln)!=1),length(ln))
  cat("== ", f, " (", length(ln), ")\n", paste(sapply(seq_len(length(brk)-1),function(i){
    a<-ln[brk[i]+1];b<-ln[brk[i+1]];if(a==b)as.character(a) else paste0(a,"-",b)}),collapse=", "), "\n", sep="")}
for (f in unique(m$file[m$miss>0][order(-m$miss[m$miss>0])])) rng(f)
