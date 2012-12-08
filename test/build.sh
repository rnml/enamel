#!/bin/bash

cd $(dirname $(readlink -f $0))/..

function build {
  ocamlfind ocamlopt \
    -syntax camlp4o \
    -package comparelib.syntax \
    -package sexplib.syntax \
    -package core \
    -thread \
    -linkpkg \
    -w YSPUZF \
    -warn-error YSPUZ \
    -c \
    test/"$1"
}

build systemf_desired.ml
