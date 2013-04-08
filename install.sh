#!/bin/bash
set -e -u -o pipefail

here=$(dirname $(readlink -f $0))

prj=enamel
src=$here/main.exe
tgt=$HOME/bin/$prj

if diff -s $src $tgt; then
    echo This version has been installed already.
else
    latest=$HOME/bin/sink/$prj.$(date +%Y-%m-%d.%H-%M-%S)
    cp $src $latest
    ln -sf sink/$(basename $latest) $tgt
    echo Installed.
fi
