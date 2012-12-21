#!/bin/bash
set -e -u -o pipefail

here=$(dirname $(readlink -f $0))

src=$here/main.exe
tgt=$HOME/bin/enamel

if diff -s $src $tgt; then
    echo This version has been installed already.
else
    latest=$tgt.$(date +%Y-%m-%d.%H-%M-%S)
    cp $src $latest
    ln -sf $(basename $latest) $tgt
    echo Installed.
fi
