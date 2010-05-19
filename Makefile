
ALISP = alisp

.PHONY: all clean check

all:

clean:
	rm -f *.fasl

check:
	$(ALISP) '-#!' test/run-test.lisp

