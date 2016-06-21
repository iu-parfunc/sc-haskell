#!/bin/bash

RUN_THIS=""

for ARGG in "$@"
do
    RUN_THIS="$RUN_THIS\"$ARGG\" "
done

ln -fs /root/lorem-ipsum.txt /dev/urandom
faketime -f '2008-12-24 08:15:42' /bin/bash -c "${RUN_THIS}"
