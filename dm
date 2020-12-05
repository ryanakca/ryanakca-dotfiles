#!/bin/sh

set -e

git config core.hooksPath .githooks

if [ ! -f SUBSTS.local ]; then
    echo "Please create the file SUBSTS.local first."
    exit 1;
fi

chmod 600 SUBSTS.local

case `uname` in
    Linux|Darwin)
        make --version | grep "GNU Make" > /dev/null 2>&1 || { echo "Please install GNU Make"; exit 1; }
        MAKE=`command -v make`
        ;;
    *)
        command -v gmake > /dev/null 2>&1 || { echo "Please install GNU Make"; exit 1; }
        MAKE=`command -v gmake`
        ;;
esac

${MAKE} -f Makefile $@

# vim:ft=sh
