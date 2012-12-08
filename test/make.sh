#!/bin/bash

cd $(dirname $(readlink -f $0))

../main.exe unbound-gen systemf.unbound >systemf.ml
