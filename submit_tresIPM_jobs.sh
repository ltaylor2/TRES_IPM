#!/bin/bash
#SBATCH -p scavenge
#SBATCH -c 1
#SBATCH --mem-per-cpu 6000
#SBATCH -t 1:00:00
#SBATCH -J tresIPM
#SBATCH -o sysOut_tresIPM.txt
#SBATCH --mail-type ALL

#
# Logistics
#
cd /home/lut2/project/TRES_IPM

FINALYEAR=2011
JOBTXT=""

for ENDYEAR in $(seq 1997 $FINALYEAR)
do
    JOBTXT+="module load miniconda; source activate tres; cd /home/lut2/project/TRES_IPM; Rscript Scripts/Run_TRES.R --endYear ${ENDYEAR} \n"
done

echo -e $JOBTXT > tresIPM_joblist.txt

module load dSQ
dsq --job-file tresIPM_joblist.txt -c 3 --mem-per-cpu 7000 -t 1-00:00:00 --mail-type ALL --submit
