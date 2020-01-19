
LISP_RUN = sbcl --script

all: build

build:
	$(LISP_RUN) utils/build.lisp

test:
	$(LISP_RUN) utils/test.lisp

.PHONY: all build test
