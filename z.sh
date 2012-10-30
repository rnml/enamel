#!/bin/bash
set -e -u -o pipefail

function int  { echo "(path (name int))"; }
function app  { echo "(wrap (app (mod $1) (mod $2)))"; }
function pair { app "$(app "(path (name pair))" "$1")" "$2"; }
function fun  { app "$(app "(path (name fun))" "$1")" "$2"; }

./main.exe check-type <<EOF
$(pair "$(int)" "$(int)")
EOF

./main.exe check-type <<EOF
$(fun "$(int)" "$(int)")
EOF

./main.exe check-expr <<EOF
(path (name 0))
EOF

#./main.exe check-expr <<EOF
#(path (app (name cons) (type (path (name int)))))
#EOF

./main.exe elaborate <<EOF
(app (name cons) (type (path (name int))))
EOF
