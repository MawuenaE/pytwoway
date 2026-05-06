# scripts/00_test_python_env.R

library(reticulate)

# ------------------------------------------------------------
# Configuration
# ------------------------------------------------------------

envname <- "r-pytwoway"

python311 <- "C:/Users/Hp/AppData/Local/Programs/Python/Python311/python.exe"

# Mets TRUE si tu veux supprimer et recréer l'environnement.
# Mets FALSE si l'environnement existe déjà et que tu veux juste installer/tester.
recreate_env <- FALSE

packages_to_install <- c(
  "numpy==1.26.4",
  "pandas==2.2.2",
  "scipy==1.13.1",
  "matplotlib",
  "bipartitepandas",
  "pytwoway"
)

# ------------------------------------------------------------
# Checks
# ------------------------------------------------------------

if (!file.exists(python311)) {
  stop(
    "Python 3.11 introuvable ici :\n",
    python311,
    "\nVérifie le chemin exact de python.exe."
  )
}

# ------------------------------------------------------------
# Create or reuse virtual environment
# ------------------------------------------------------------

if (recreate_env) {
  cat("\nRemoving existing virtual environment if it exists...\n")
  try(virtualenv_remove(envname, confirm = FALSE), silent = TRUE)
  
  cat("\nCreating virtual environment with Python 3.11...\n")
  virtualenv_create(
    envname = envname,
    python = python311
  )
} else {
  existing_envs <- virtualenv_list()
  
  if (!(envname %in% existing_envs)) {
    cat("\nVirtual environment does not exist. Creating it with Python 3.11...\n")
    
    virtualenv_create(
      envname = envname,
      python = python311
    )
  } else {
    cat("\nVirtual environment already exists. Reusing it.\n")
  }
}

# ------------------------------------------------------------
# Activate virtual environment
# ------------------------------------------------------------

use_virtualenv(envname, required = TRUE)

cat("\n--- Python configuration before package installation ---\n")
print(py_config())

# ------------------------------------------------------------
# Install packages
# ------------------------------------------------------------

cat("\n--- Installing Python packages ---\n")

py_install(
  packages = packages_to_install,
  envname = envname,
  pip = TRUE
)

cat("\nPackage installation step finished.\n")

# ------------------------------------------------------------
# Test imports
# ------------------------------------------------------------

cat("\n--- Testing Python imports ---\n")

py_run_string("
import numpy as np
import pandas as pd
import scipy
import matplotlib
import bipartitepandas as bpd
import pytwoway as tw

print('numpy:', np.__version__)
print('pandas:', pd.__version__)
print('scipy:', scipy.__version__)
print('matplotlib:', matplotlib.__version__)
print('bipartitepandas imported successfully')
print('pytwoway imported successfully')
")

cat("\nPython environment works.\n")