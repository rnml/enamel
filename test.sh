#!/bin/bash

echo 'checking an F type ...'
./main.exe check-f-type <<EOF
(Forall ((a *)) (Fun a a))
EOF

echo 'checking an F type ...'
./main.exe check-f-type <<EOF
(Exists ((a *)) a)
EOF

echo 'checking an F type ...'
./main.exe check-f-type <<EOF
(Lambda ((a *) (b *) (c *)) (Fun a b c))
EOF

echo 'checking an F type ...'
./main.exe check-f-type <<EOF
(Lambda ((b (Fun * *)) (c *)) (Fun (b c) c))
EOF

echo 'checking an F term ...'
./main.exe check-f-term <<EOF
(Fun ((Type a *) (x a)) x)
EOF

echo 'checking an F term ...'
./main.exe check-f-term <<EOF
(Pack
  (Forall ((a *)) (Fun a a))
  (Fun ((Type a *) (x a)) x)
  : Exists a . a)
EOF
