#!/usr/bin/env bash

filter="$1"
shift

die() {
    if [[ $# != 0 ]]; then
        echo "$*" >&2
    fi
    exit 1
}

getSize() {
    local -a cmd
    local out
    local example="$1"
    shift
    cmd=( cargo bloated --example "$example" --output sections --quiet "$@" -- --quiet )
    out="$("${cmd[@]}")" || die "command fail: ${cmd[*]}"
    sed -nE 's/.*\s(\S+)\s+\(file\).*/\1/p' <<<"$out"
}

printf "%-20s %8s %8s\n" "Example" "default" "no-default-features"

for example in simple-{clap,argh,palc,none} criterion-{clap,argh,palc} deno-{clap,palc}; do
    if [[ "size-$example" != *"$filter"* ]]; then
        continue
    fi

    defaultSize="$(getSize $example)" || die
    minSize="$(getSize $example --no-default-features)" || die

    printf "%-20s %8s %8s\n" "size-$example" "$defaultSize" "$minSize"
done

getCompileTime() {
    CARGO_TARGET_DIR="$tmpdir" command time --format "%Uuser %Ssystem %Eelapsed" \
        cargo build --package compile-bench --quiet "$@" || die "failed to build"
}

if [[ "comptime" = *"$filter"* ]]; then
    echo
    echo "Compile time"

    tmpdir="$(mktemp -d /tmp/palc-target.XXXXXX)" || die "failed to mktemp"
    trap 'rm -r -- "$tmpdir"' EXIT

    # Prepare and download dependencies sources.
    cargo metadata --format-version 1 >/dev/null || die "failed to run cargo metadata"

    printf "comptime-default     "
    getCompileTime || die
    printf "comptime-incremental "
    touch --no-create compile-bench/src/main.rs
    getCompileTime || die
fi