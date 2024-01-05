#!/bin/sh

for f in ${HOME}/.config/environment.d/*; do
    . $f
    for VAR in $(sed -n '/^[^#]/ s/=.*//p' $f); do
        export $VAR
    done
done
