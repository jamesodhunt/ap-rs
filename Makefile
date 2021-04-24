# Copyright (c) 2021 James O. D. Hunt.
#
# SPDX-License-Identifier: Apache-2.0
#

ifneq ($(DEBUG),)
    CARGO_TEST_ARGS = -vv -- --nocapture
    # XXX: Note: don't quote the value, or it won't work!
    export RUSTFLAGS=-D warnings
endif

default: build check doc

build:
	cargo build

check: test examples

test: unit-test test-coverage

test-coverage:
	cargo tarpaulin -v

unit-test:
	cargo test $(CARGO_TEST_ARGS)

examples:
	cargo run --example simple -- -a foo -d -a bar -d -a baz
	cargo run --example positional-args-only -- one two "hello world" three "foo bar" four "the end"
	cargo run --example option-and-positional-args -- "posn 1" -d "posn 2" -a "hello world" -a "foo bar" "the end" -d

doc:
	cargo doc

clean:
	cargo clean

.PHONY: check
.PHONY: clean
.PHONY: default
.PHONY: doc
.PHONY: examples
.PHONY: test
.PHONY: test-coverage
.PHONY: unit-test
