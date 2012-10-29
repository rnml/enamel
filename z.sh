#!/bin/bash

function app  { echo "(fix (app (mod $1) (mod $2)))"; }
function pair { app "$(app "(name pair)" "$1")" "$2"; }
function fun  { app "$(app "(name fun)" "$1")" "$2"; }

./main.exe check-type <<EOF
$(pair "(name int)" "(name int)")
EOF

./main.exe check-type <<EOF
$(fun "(name int)" "(name int)")
EOF

./main.exe check-expr <<EOF
(path (name 0))
EOF

./main.exe check-expr <<EOF
(path (app (name cons) (type (path (name int)))))
EOF
