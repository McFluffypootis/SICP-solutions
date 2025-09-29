install:
	raco pkg install sicp
	raco pkg install review
	raco pkg install racket/exn

test:
	raco test sicp

lint:
	@echo "check codestyle"
	@(for f in $$(find sicp -name '*.rkt'); do raco review $$f; done)

.PHONY: test
