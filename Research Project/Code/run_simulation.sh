#!/usr/bin/env Rscript

echo "Program about to run"
#no migration
Rscript --vanilla simulation.R 500 0 0 "heterozygous" "heterozygous"
Rscript --vanilla simulation.R 500 0 0 "homozygous" "homozygous"

Rscript --vanilla simulation.R 500 0 0 "homozygous" "heterozygous"
Rscript --vanilla simulation.R 500 0 0 "heterozygous" "homozygous"

#migration
Rscript --vanilla simulation.R 500 0.01 0 "heterozygous" "heterozygous"
Rscript --vanilla simulation.R 500 0.1 1 "heterozygous" "heterozygous"

Rscript --vanilla simulation.R 500 0.01 0 "heterozygous" "homozygous"
Rscript --vanilla simulation.R 500 0.1 1 "homozygous" "heterozygous"

Rscript --vanilla simulation.R 500 0.03 0 "heterozygous" "heterozygous"
Rscript --vanilla simulation.R 500 0.3 1 "heterozygous" "heterozygous"

Rscript --vanilla simulation.R 500 0.03 0 "heterozygous" "homozygous"
Rscript --vanilla simulation.R 500 0.3 1 "homozygous" "heterozygous"

Rscript --vanilla simulation.R 500 0.05 0 "heterozygous" "heterozygous"
Rscript --vanilla simulation.R 500 0.5 1 "heterozygous" "heterozygous"

Rscript --vanilla simulation.R 500 0.05 0 "heterozygous" "homozygous"
Rscript --vanilla simulation.R 500 0.5 1 "homozygous" "heterozygous"

echo "Finished"
