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
    [ -n "${opt[quiet]}"  ] || echo "$@"
    [ -n "${opt[dryrun]}" ] || "$@"
}

bg() {
    "$@" &
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
    # [test]
    [xmega65]=$(realpath -m "$topdir/../xemu/build/bin/xmega65.native")
)

ARGS=$(getopt -o fhnqv -l acme:,build,builddir:,c1541:,debug,dry-run,force,help,m65:,m65dbg:,mega65_ftp:,quiet,release,release-test,screenshot,test,verbose,xmega65: -n $(basename "$0") -- "$@") || ARGS='-?'
eval set -- "$ARGS"

while [ $# -gt 0 ]; do
    case "$1" in

    --acme|--builddir|--c1541|--m65|--m65dbg|--mega65_ftp|--xmega65)
        opt[${1#--}]=${2}
        shift 2
        ;;

    --build|--debug|--release|--release-test|--screenshot|--test)
        opt[${1#--}]=1
        unset opt[all]
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

declare -a acmeopts
acmeopts+=(--color)
acmeopts+=(-f cbm)
acmeopts+=(--msvc)
acmeopts+=(-I src)

declare -a m65dbgopts
[ -z "${opt[emulate]}" ] || m65dbgopts+=(-l tcp)

declare -a m65opts
[ -z "${opt[emulate]}" ] || m65opts+=(-l tcp)

declare -a xmega65opts
xmega65opts+=(-uartmon :4510)
xmega65opts+=(-autoload 1)

# acmeopts+=(-DDEBUG=1)

set -e

builddir="${opt[builddir]}/build"
buildimg="${builddir}/mega65-forth-build.d81"
[ -e "$builddir" ] || cmd mkdir -p "$builddir"

releasedir="${opt[builddir]}/release"
releaseimg="$releasedir/mega65-forth.d81"
[ -d "$releasedir" ] || cmd mkdir -p "$releasedir"

add_raw_files() {
    local image="$1"
    shift
    for src; do
        local name=$(basename "$src")
        cmd "${opt[c1541]}" "$image" -write "$src" "$name,s"
    done
}

add_files() {
    local image="$1"

    tmpdir="${opt[builddir]}/tmp"
    [ -d "$tmpdir" ] || cmd mkdir -p "$tmpdir"

    shift
    for f; do
        local suffix

        local name=$(basename "$f")

        case "$f" in
        *=*)
            echo "$f"
            newf=${f%=*}
            name=${f#*=}
            f=${newf}
            echo "$f --- $name"
        esac

        case "$name" in
        *.f)
            # name=$(basename "$name" .f)
            ;;
        *.prg)
            name=$(basename "$name" .prg)
        esac

        case "$f" in
    
        *.prg)
            suffix=""
            ;;

        *)
            suffix=",s"
            newf="$tmpdir/$name"
            cmd "${opt[petcat]}" -text -w10 -o "$newf" -- "$f"
            f="$newf"
        esac

        cmd "${opt[c1541]}" "$image" -write "$f" "${name}${suffix}"
    done
}

do_build() {
    local rev=$(git describe --tags)
    if [ -n "$rev" -a -z "${opt[dryrun]}" ]; then
        cat <<EOF > "$builddir/revision.asm"
_revision +STRING "$rev"
EOF
    fi
    [ -z "$rev" ] || acmeopts+=(-DHAVE_REVISION=1)

    # TODO we put the .sym and .rep files into the source dir
    # (at least temporarily) to make it easier to use m65dbg
    cmd "${opt[acme]}" "${acmeopts[@]}" -I "$builddir" \
        -l "$topdir/src/forth.sym" \
        -o "$builddir/forth-skeletal.prg" \
        -r "$topdir/src/forth.rep" \
        src/forth.asm

    # TODO generate d81 image with everything
    cmd "${opt[c1541]}" -format 'mega65 forth,1' d81 "$buildimg"

    # TODO add the floating point tests (from fp subdir in the test suite)
    add_files "$buildimg" \
        "$builddir/forth-skeletal.prg" \
        src/bootstrap1.f \
        src/bootstrap2.f \
        src/benchmark.f \
        src/test.f \
        src/d-block.f \
        src/d-block-ext.f \
        src/d-core.f \
        src/d-core-ext.f \
        src/d-double.f \
        src/d-double-ext.f \
        src/d-exception.f \
        src/d-facility.f \
        src/d-facility-ext.f \
        src/d-file.f \
        src/d-file-ext.f \
        src/d-floating.f \
        src/d-floating-ext.f \
        src/d-locals.f \
        src/d-locals-ext.f \
        src/d-memory.f \
        src/d-search.f \
        src/d-search-ext.f \
        src/d-string.f \
        src/d-string-ext.f \
        src/d-tools.f \
        src/d-tools-ext.f \
        src/d-xchar.f \
        src/d-xchar-ext.f \
        "$topdir/test/forth2012-test-suite/src/blocktest.fth=t-block.f" \
        "$topdir/test/forth2012-test-suite/src/core.fr=t-core.f" \
        "$topdir/test/forth2012-test-suite/src/coreplustest.fth=t-core-plus.f" \
        "$topdir/test/forth2012-test-suite/src/coreexttest.fth=t-core-ext.f" \
        "$topdir/test/forth2012-test-suite/src/doubletest.fth=t-double.f" \
        "$topdir/test/forth2012-test-suite/src/errorreport.fth=t-error-report.f" \
        "$topdir/test/forth2012-test-suite/src/exceptiontest.fth=t-exception.f" \
        "$topdir/test/forth2012-test-suite/src/facilitytest.fth=t-facility.f" \
        "$topdir/test/forth2012-test-suite/src/filetest.fth=t-file.f" \
        src/t-internals.f \
        "$topdir/test/forth2012-test-suite/src/localstest.fth=t-locals.f" \
        "$topdir/test/forth2012-test-suite/src/memorytest.fth=t-memory.f" \
        "$topdir/test/forth2012-test-suite/src/prelimtest.fth=t-preliminary.f" \
        "$topdir/test/forth2012-test-suite/src/searchordertest.fth=t-search.f" \
        "$topdir/test/forth2012-test-suite/src/stringtest.fth=t-string.f" \
        "$topdir/test/forth2012-test-suite/src/tester.fr=t-tester.f" \
        "$topdir/test/forth2012-test-suite/src/toolstest.fth=t-tools.f" \
        "$topdir/test/forth2012-test-suite/src/utilities.fth=t-utilities.f"
    cmd "${opt[c1541]}" "$buildimg" -dir > "$builddir/$(basename $buildimg .d81).txt"
}

do_release() {
    # Generate a release disk image from the build one ...
    # TODO copy disk image back from the MEGA65
    cmd "${opt[c1541]}" "$buildimg" -read forth-minimal,p "$releasedir/forth-minimal.prg"
    cmd "${opt[c1541]}" "$buildimg" -read forth-complete,p "$releasedir/forth-complete.prg"

    cmd "${opt[c1541]}" -format 'mega65 forth,1' d81 "$releaseimg"
    add_files "$releaseimg" \
        "$releasedir/forth-complete.prg" \
        "$releasedir/forth-minimal.prg" \
        "$builddir/forth-skeletal.prg"

    "${opt[c1541]}" "$buildimg" -list | while read size f type; do
        f=${f%\"}
        f=${f#\"}
        case "$f:$type" in
        # *:prg)
        *:seq)
            cmd "${opt[c1541]}" "$buildimg" -read "${f},s" "$tmpdir/$f"
            cmd "${opt[c1541]}" "$releaseimg" -write "$tmpdir/$f" "${f},s"
            echo "$size,$f,$type"
        esac
    done

    cmd "${opt[c1541]}" "$releaseimg" -dir > "$releasedir/$(basename $releaseimg .d81).txt"
}

# could run xemu headless, full speed to do these ...
do_build_minimal() {
    # TODO
    :
}

do_build_full() {
    # TODO
    :
}

# TODO run various unit tests ...
# TODO run various benchmarks ...

do_screenshot() {
    cmd "${opt[m65]}" "${m65opts[@]}" --quiet --screenshot="${opt[builddir]}/screenshot.png"
}

do_bootstrap() {
    :
}

do_test() {
    if [ -n "${opt[emulate]}" ]; then

        cmd "${opt[xmega65]}" "${xmega65opts[@]}" -8 "$buildimg"
        # -prg "${opt[builddir]}/forth.prg"

        # cmd bg "${opt[xmega65]}" "${xmega65opts[@]}" -8 "${opt[builddir]}/mega65-forth.d81"
        # cmd sleep 10
        # cmd "${opt[m65]}" "${m65opts[@]}" -t 'require "bootstrap.f"'
    else
        "${opt[m65]}" --quiet --reset

        cmd "${opt[mega65_ftp]}" \
            -c "put $buildimg" \
            -c "mount $(basename $buildimg)" \
            -c quit

        local -a m65opts_
        [ -n "${opt[debug]}" ] || m65opts_+=(--run)
        cmd "${opt[m65]}" "${m65opts[@]}" "${m65opts_[@]}" --quiet "$builddir/forth-skeletal.prg"

        # TODO
        # "${opt[m65dbg]}" load "${opt[builddir]}/forth-skeletal.prg"

    fi
}

do_release_test() {
    if [ -n "${opt[emulate]}" ]; then

        cmd "${opt[xmega65]}" "${xmega65opts[@]}" -8 "$releaseimg"
        # -prg "${opt[builddir]}/forth.prg"

        # cmd bg "${opt[xmega65]}" "${xmega65opts[@]}" -8 "${opt[builddir]}/mega65-forth.d81"
        # cmd sleep 10
        # cmd "${opt[m65]}" "${m65opts[@]}" -t 'require "bootstrap.f"'
    else
        "${opt[m65]}" --quiet --reset

        cmd "${opt[mega65_ftp]}" \
            -c "put $releaseimg" \
            -c "mount $(basename $releaseimg)" \
            -c quit

        local -a m65opts_
        [ -n "${opt[debug]}" ] || m65opts_+=(--run)
        cmd "${opt[m65]}" "${m65opts[@]}" "${m65opts_[@]}" --quiet "$builddir/forth-skeletal.prg"

        # TODO
        # "${opt[m65dbg]}" load "${opt[builddir]}/forth-skeletal.prg"

    fi
}

do_debug() {
    cmd "${opt[m65dbg]}" "${m65dbgopts[@]}"
}

[ -z "${opt[all]}" -a -z "${opt[build]}"       ] || do_build
[ -z "${opt[all]}" -a -z "${opt[test]}"        ] || do_test

[ -z "${opt[debug]}"        ] || do_debug
[ -z "${opt[screenshot]}"   ] || do_screenshot
[ -z "${opt[release]}"      ] || do_release
[ -z "${opt[release-test]}" ] || do_release_test  
