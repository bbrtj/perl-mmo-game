#!/bin/bash

STANDARD_PROC="perl Makefile.PL && rm -f *.tar.gz && make manifest && make dist"
CLEANUP="make veryclean && cd .."

eval "cd Game-Common && $STANDARD_PROC && $CLEANUP"
eval "cd Game-Worker && $STANDARD_PROC && $CLEANUP"

rm -Rf local/lib/perl5/Game
rm -Rf local/cache/vendor

carton install
