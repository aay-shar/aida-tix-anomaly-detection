# =====================================================================
# Run order. The pipeline pauses twice for external C++ binaries.
#
#   PHASE 1  01 -> 02 (export)      then run the AIDA binary
#   PHASE 2  02 (re-import) -> 03   then run the TIX binary
#   PHASE 3  03 (re-import) -> 06
#
# Everything runs in ONE session -- each script uses objects the
# previous one leaves behind. Do not restart R between phases.
# =====================================================================

source("R/01_preprocessing.R")
source("R/02_aida.R")
# STOP -- run the AIDA binary, set RUN_AIDA_REIMPORT <- TRUE, re-source 02
source("R/03_tix.R")
# STOP -- run the TIX binary, re-source 03
source("R/04_evaluation.R")
source("R/05_figures.R")
source("R/06_export.R")