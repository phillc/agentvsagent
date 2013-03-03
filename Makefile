REPORTER = spec

test:
	./node_modules/mocha/bin/mocha --reporter $(REPORTER)

test-cov: lib-cov
	@$(MAKE) REPORTER=html-cov > coverage.html

lib-cov:
	jscoverage hearts hearts-cov


.PHONY: test
