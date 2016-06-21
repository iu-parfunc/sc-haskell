#!/bin/bash

ln -fs /root/lorem-ipsum.txt /dev/urandom
faketime -f '2008-12-24 08:15:42' sh -c "$@"
