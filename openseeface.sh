#!/bin/bash
readonly SRCDIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)
INSTALL=0
if [ ! -d "${SRCDIR}/OpenSeeFace" ]; then
   git clone --depth 1 https://github.com/emilianavt/OpenSeeFace
   python -m venv "${SRCDIR}/OpenSeeFace/venv"
   INSTALL=1
fi
cd "${SRCDIR}/OpenSeeFace"
source "venv/bin/activate"
if [ "$INSTALL" == 1 ]; then
    pip install wheel
    pip install onnxruntime opencv-python pillow numpy
fi
exec python "facetracker.py" "$@"
