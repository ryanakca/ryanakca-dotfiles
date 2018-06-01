#!/bin/sh

set -e

git config core.hooksPath .githooks

if [ ! -f SUBSTS.local ]; then
    echo "Please create the file SUBSTS.local first."
    exit 1;
fi

chmod 600 SUBSTS.local

if [ `uname` = "Linux" ]; then
    make --version | grep "GNU Make" > /dev/null 2>&1 || { echo "Please install GNU Make"; exit 1; }
    MAKE=`command -v make`
else
    command -v gmake > /dev/null 2>&1 || { echo "Please install GNU Make"; exit 1; }
    MAKE=`command -v gmake`
fi

${MAKE} -f Makefile $@

# vim:ft=sh
