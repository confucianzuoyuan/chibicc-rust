chibicc-rust:
	cargo build

test: chibicc-rust
	sh ./test.sh
	sh ./test-driver.sh

clean:
	cargo clean

.PHONY: test clean
