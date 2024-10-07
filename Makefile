chibicc-rust:
	cargo build

test: chibicc-rust
	sh ./test.sh

clean:
	cargo clean

.PHONY: test clean
