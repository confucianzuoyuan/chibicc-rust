TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

chibicc: clean
	cargo build
	cp target/debug/chibicc-rust chibicc

test/%.exe: chibicc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./chibicc -o test/$*.s -
	$(CC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh

clean:
	rm -rf chibicc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
	cargo clean

.PHONY: test clean
