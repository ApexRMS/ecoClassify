#!/bin/bash
set -e


python testing/reinstallPackage.py \
  --meta "testing/metadata.xml" \
  --folder ./src \
  --reinstall \
  --packagemanager "/c/Program Files/SyncroSim/SyncroSim.PackageManager.exe"


  
python testing/testTemplateLibrary.py testing/metadata.xml --console "/c/Program Files/SyncroSim/SyncroSim.Console.exe" --tempdir scratch/