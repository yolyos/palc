#!/usr/bin/env bash

filter="$1"
flags=(
    --no-default-features
    --features __test-allow-unknown-fields
)

if [[ "$2" = "-d" ]]; then
    cargo bloat \
        --example "$filter" \
        "${flags[@]}" \
        -n 0 \
        | rg --passthru --color=always -F 'static' | $PAGER
    exit "$?"
fi

for example in criterion-{clap,argh,static} deno-{clap,static}; do
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
        | rg --color=never "$example"'|clap_static|std|clap|argh|\.text'
    echo

    cargo bloat \
        --example "$example" \
        "${flags[@]}" \
        --symbols-section .rodata 2>/dev/null \
        | rg --color=never '\.rodata'
    echo
done
