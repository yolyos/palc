#!/usr/bin/env bash

filter="$1"
shift
flags=(
    --release
    --no-default-features
    --features __test-allow-unknown-fields
)

# Exclude outline-able std staticlib functions.
# Locally instantiated generic functions will still be counted.
if [[ "$1" = "-e" ]]; then
    shift
    export RUSTFLAGS="-Cprefer-dynamic $RUSTFLAGS"
    echo "INFO: Linking to dylib std"
fi

# Show details.
if [[ "$1" = "-d" ]]; then
    shift
    flags+=( "$@" )
    cargo bloat \
        --example "$filter" \
        "${flags[@]}" \
        -n 0 \
        | rg --passthru --color=always -F 'static' | $PAGER
    exit "$?"
fi

# Minimal standalone binary.
if [[ "$1" = "-m" ]]; then
    shift
    flags=(
        --profile minimal
        "${flags[@]:1}"
        -Z build-std=std,panic_abort
        -Z build-std-features=panic_immediate_abort
        "$@"
    )
    if ! output="$(cargo build --message-format json --example "$filter" "${flags[@]}")"; then
        exit "$?"
    fi
    exepath="$(jq -s -r '.[] | select(.executable != null) .executable' <<<"$output")"
    ls -lh "$exepath"
    exit 0
fi

for example in simple-{clap,argh,palc,none} criterion-{clap,argh,palc} deno-{clap,palc}; do
    if [[ "$example" != *"$filter"* ]]; then
        continue
    fi
    if [[ "$detail" = "-d" ]]; then
        flags+=(-)
    fi

    echo "<<< $example >>>"

    cargo bloat \
        --example "$example" \
        "${flags[@]}" \
        --crates 2>/dev/null \
        | rg --color=never "$example"'|palc|std|clap|argh|\.text'
    echo

    cargo bloat \
        --example "$example" \
        "${flags[@]}" \
        --symbols-section .rodata 2>/dev/null \
        | rg --color=never '\.rodata'
    echo
done
