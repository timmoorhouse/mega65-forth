#!/bin/bash

usage() {
	echo 'Usage: build [options...] [module...]' >&2
}

error() {
    echo "ERROR: $*" >&2
}

die() {
    error "$@"
    exit 10
}

cmd() {
    [ -n "${opt[quiet]}" ] || echo "$@"
    [ -n "${opt[dryrun]}" ] || "$@"
}

thisdir=$(dirname $(readlink -f "$0"))
topdir=$(readlink -f "$thisdir/..")

declare -A opt=(
    [acme]=$(realpath -m "$topdir/../../build/src/acme")
    [all]=1
    # [build]
    [builddir]="$topdir/build"
    [c1541]=c1541
    # [debug]=1 # TODO
    # [emulate]=1
    [m65]=$(realpath -m "$topdir/../../build/bin/m65")
    [m65dbg]=$(realpath -m "$topdir/../m65dbg/m65dbg")
    [mega65_ftp]=$(realpath -m "$topdir/../../build/bin/mega65_ftp")
    [petcat]=petcat
    [rom]=$(realpath -m "$topdir/../../MEGA65.ROM")
    # [test]
    [xmega65]=$(realpath -m "$topdir/../xemu/build/bin/xmega65.native")
)

[ -r "${opt[rom]}" ] || unset opt[rom]

ARGS=$(getopt -o fhnqv -l acme:,build,builddir:,c1541:,debug,dry-run,force,help,m65:,m65dbg:,mega65_ftp:,quiet,rom:,screenshot,test,verbose,xmega65: -n $(basename "$0") -- "$@") || ARGS='-?'
eval set -- "$ARGS"

while [ $# -gt 0 ]; do
    case "$1" in

    --acme|--builddir|--c1541|--m65|--m65dbg|--mega65_ftp|--rom|--xmega65)
        opt[${1#--}]=${2}
        shift 2
        ;;

    --build|--screenshot|--test)
        opt[${1#--}]=1
        unset opt[all]
        shift
        ;;

    --debug)
        opt[debug]=1
        shift
        ;;

    -n|--dry-run)
        opt[dryrun]=1
        shift
        ;;

    -f|--force)
        opt[force]=1
        shift
        ;;

    -q|--quiet)
        opt[quiet]=1
        shift
        ;;

    -v|--verbose)
        opt[verbose]=1
        shift
        ;;

    -h|--help)
        usage
        exit 0
        ;;

	--)
		shift
		break
		;;

	-?)
		exit 5
		;;

	*)
		error "Unexpected option $1"
		usage
		exit 5        
    esac
done

"${opt[m65]}" --quiet --autodiscover 2>/dev/null || opt[emulate]=1

set -e

add_text_files() {
    local image="$1"
    shift
    for src; do
        local name=$(basename "$src")
        cmd dd if="$src" of="${opt[builddir]}/$name.lc" conv=lcase
        cmd "${opt[petcat]}" -text -w10 -o "${opt[builddir]}/$name" -- "${opt[builddir]}/$name.lc"
        cmd "${opt[c1541]}" "$image" -write "${opt[builddir]}/$name"
    done
}

do_build() {
    # ls -l ${ACME}
    # ${ACME} -h
    # TODO --color?
    declare -a ACMEOPTS
    ACMEOPTS+=(--color)
    # ACMEOPTS+=(-I include/acme)
    ACMEOPTS+=(-I src)
    [ -z "${opt[builddir]}" ] || ACMEOPTS+=(-I "${opt[builddir]}")
    ACMEOPTS+=(-f cbm)
    ACMEOPTS+=(--msvc)

    local rev=$(git show-ref --head -s --abbrev | head -n1)
    if [ -n "$rev" -a -z "${opt[dryrun]}"]; then
        cat <<EOF > "${opt[builddir]}/revision.asm"
_revision +STRING "$rev"
EOF
    fi
    [ -z "$rev" ] || ACMEOPTS+=(-DHAVE_REVISION=1)

    # TODO we put the .sym and .rep files into the source dir
    # (at least temporarily) to make it easier to use m65dbg
    cmd "${opt[acme]}" "${ACMEOPTS[@]}" \
        -l "$topdir/src/forth.sym" \
        -o "${opt[builddir]}/forth.prg" \
        -r "$topdir/src/forth.rep" \
        src/forth.asm # 2>&1 | tee build/forth.log

    # TODO generate d81 image with everything
    cmd "${opt[c1541]}" -format 'mega65 forth,1' d81 "${opt[builddir]}/mega65-forth.d81"
    cmd "${opt[c1541]}" "${opt[builddir]}/mega65-forth.d81" -write "${opt[builddir]}/forth.prg" autoboot.c65
    add_text_files "${opt[builddir]}/mega65-forth.d81" src/bootstrap.f "$topdir/test/forth2012-test-suite/src/prelimtest.fth"
    cmd "${opt[c1541]}" "${opt[builddir]}/mega65-forth.d81" -dir > "${opt[builddir]}/mega65-forth.txt"

    cmd ls -l "${opt[builddir]}/forth.prg"
}

do_screenshot() {
    if [ -n "${opt[emulate]}" ]; then
        :
    else
        "${opt[m65]}" --quiet --screenshot="${opt[builddir]}/screenshot.png"
    fi
}

do_test() {
    local -a m65opts
    local -a m65dbgopts

    if [ -n "${opt[emulate]}" ]; then
        cmd "${opt[xmega65]}" \
            ${opt[rom]:+-besure -rom "${opt[rom]}"} \
            -uartmon :4510 \
            -prg "${opt[builddir]}/forth.prg"
        m65dbgopts+=(-l tcp)
        # "C:\Users\timmo\OneDrive\MEGA65\mega65r3-release-0.95\sdcard-files\MEGA65.ROM"
    else
        # "${opt[m65]}" --quiet --reset

        # cmd "${opt[mega65_ftp]}" \
        #     -c "put ${opt[builddir]}/mega65-forth.d81" \
        #     -c "mount mega65-forth.d81" \
        #     -c quit

        [ -n "${opt[debug]}" ] || m65opts+=(--run)
        cmd "${opt[m65]}" --quiet "${m65opts[@]}" "${opt[builddir]}/forth.prg"

        # TODO
        # "${opt[m65dbg]}" load "${opt[builddir]}/forth.prg"

    fi
}

[ -z "${opt[all]}" -a -z "${opt[build]}"       ] || do_build
[ -z "${opt[all]}" -a -z "${opt[test]}"        ] || do_test

[ -z "${opt[all]}" -a -z "${opt[screenshot]}"  ] || do_screenshot
