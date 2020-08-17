#!/usr/bin/env Rscript

echo "Program about to run"

Rscript --vanilla simulation4.R 500 0.5 1 "homozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.05 2 "homozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.15 2 "homozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.25 2 "homozygous" "heterozygous"

Rscript --vanilla simulation4.R 500 0.1 1 "heterozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.3 1 "heterozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.5 1 "heterozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.05 2 "heterozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.15 2 "heterozygous" "heterozygous"
Rscript --vanilla simulation4.R 500 0.25 2 "heterozygous" "heterozygous"


echo "Finished a_sim_4-3"
