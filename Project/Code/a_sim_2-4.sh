#!/usr/bin/env Rscript

echo "Program about to run"

Rscript --vanilla simulation2.R 500 0.1 1 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.3 1 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.5 1 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.05 2 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.15 2 "homozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.25 2 "homozygous" "homozygous"

Rscript --vanilla simulation2.R 500 0.1 1 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.3 1 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.5 1 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.05 2 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.15 2 "heterozygous" "homozygous"
Rscript --vanilla simulation2.R 500 0.25 2 "heterozygous" "homozygous"


echo "Finished"
