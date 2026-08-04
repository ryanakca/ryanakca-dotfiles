#!/bin/sh

TARGETS="unstable-amd64 unstable-i386 unstable-sh4"

DEST=~/.cache/sbuild

call_mmdebstrap() {
    # call_mmdebstrap dist arch mirror
    mmdebstrap \
        --quiet \
        --arch=$2 \
        --variant=buildd \
        --components=main,contrib,non-free \
        --aptopt='Acquire::http::proxy "http://127.0.0.1:3142";' \
        $1 "$DEST/$1-$2.tar" "$3"
}

get_mirror() {
    case $1 in
        sh4)
            echo "http://ftp.ports.debian.org/debian-ports/"
            ;;
        *)
            echo "http://deb.debian.org/debian"
            ;;
    esac
}

for target in $TARGETS; do
    dist="$(echo "$target" | cut -d- -f1)"
    arch="$(echo "$target" | cut -d- -f2)"
    mirror="$(get_mirror $arch)"
    call_mmdebstrap "$dist" "$arch" "$mirror"
    if [ "$dist" = "unstable" ] && [ ! -L "$DEST/experimental-$arch.tar" ]; then
        ln -s "$DEST/$dist-$arch.tar" "$DEST/experimental-$arch.tar"
    fi
done
