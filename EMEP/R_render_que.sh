#!/bin/bash
#
#SBATCH --job-name=EMEP_QAQC
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem-per-cpu=8000M
#SBATCH --partition=atmospheric


set -euo pipefail


# ----------------------------------------------------------------------
# QAQC setup
# ----------------------------------------------------------------------

# Location of the shared EMEP QAQC source code on Polar.

QAQC_SOURCE_DIR='/home/tomlis65/QAQC_SOURCE_CODE/EMEP'


# ----------------------------------------------------------------------
# User input
# ----------------------------------------------------------------------

# Supply the QAQC user input file as the first argument to sbatch, e.g.:
#
#   sbatch R_render_que.sh user_input_files/emep_qaqc_user_input_example.R
#
# The path may be absolute or relative to the directory from which the
# job is submitted.

if [ "$#" -ne 1 ]; then
  echo "Usage:"
  echo "  sbatch $0 /path/to/emep_qaqc_user_input.R"
  exit 1
fi

USER_INPUT_ARG="$1"

if [[ "${USER_INPUT_ARG}" = /* ]]; then

  # Absolute path supplied.

  export EMEP_QAQC_USER_INPUT="${USER_INPUT_ARG}"

else

  # Relative path supplied from the directory where sbatch was called.

  export EMEP_QAQC_USER_INPUT="${SLURM_SUBMIT_DIR:-$PWD}/${USER_INPUT_ARG}"

fi


# ----------------------------------------------------------------------
# Validate paths
# ----------------------------------------------------------------------

if [ ! -f "${EMEP_QAQC_USER_INPUT}" ]; then
  echo "User input file not found:"
  echo "  ${EMEP_QAQC_USER_INPUT}"
  exit 1
fi

if [ ! -d "${QAQC_SOURCE_DIR}" ]; then
  echo "QAQC source directory not found:"
  echo "  ${QAQC_SOURCE_DIR}"
  exit 1
fi

if [ ! -f "${QAQC_SOURCE_DIR}/Renderer_EMEP.R" ]; then
  echo "EMEP QAQC renderer not found:"
  echo "  ${QAQC_SOURCE_DIR}/Renderer_EMEP.R"
  exit 1
fi


# ----------------------------------------------------------------------
# Run QAQC
# ----------------------------------------------------------------------

echo "Using user input file:"
echo "  ${EMEP_QAQC_USER_INPUT}"
echo ""
echo "Using EMEP QAQC source code:"
echo "  ${QAQC_SOURCE_DIR}"
echo ""

cd "${QAQC_SOURCE_DIR}"

echo "Rendering EMEP QAQC report..."

Rscript Renderer_EMEP.R

echo "EMEP QAQC workflow complete."
