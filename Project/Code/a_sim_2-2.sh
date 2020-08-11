#!/usr/bin/env Rscript

echo "Program about to run"

Rscript --vanilla simulation2.R 500 0.01 0 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.03 0 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.05 0 "homozygous" "homozygous"

Rscript --vanilla simulation2.R 500 0.01 0 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.03 0 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.05 0 "heterozygous" "homozygous"

Rscript --vanilla simulation2.R 500 0 3 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0 3 "heterozygous" "heterozygous"
Rscript --vanilla simulation2.R 500 0 3 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0 3 "homozygous" "heterozygous"

Rscript --vanilla simulation2.R 500 0.1 1 "homozygous" "heterozygous"
Rscript --vanilla simulation2.R 500 0.3 1 "homozygous" "heterozygous"

echo "Finished"
