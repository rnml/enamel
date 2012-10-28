#!/bin/bash

function app {
  echo "(fix (app (mod $1) (mod $2)))"
}

function pair {
  app "$(app "(name pair)" "$1")" "$2"
}

./main.exe <<EOF
$(pair "(name int)" "(name int)")
EOF
