#!/bin/bash
set -e


python testing/integration/reinstallPackage.py \
  --meta "testing/integration/metadata.xml" \
  --folder ./src \
  --reinstall \
  --packagemanager "/c/Program Files/SyncroSim/SyncroSim.PackageManager.exe"


  
python testing/integration/testTemplateLibrary.py testing/integration/metadata.xml --console "/c/Program Files/SyncroSim/SyncroSim.Console.exe" --tempdir scratch/