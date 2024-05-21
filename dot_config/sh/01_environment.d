#!/bin/sh

for f in "${HOME}/.config/environment.d/"*; do
    . "$f"
    sed -n '/^[^#]/ s/=.*//p' "$f" | while read -r VAR; do
        export "${VAR?}"
    done
done
