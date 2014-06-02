REPORTER = spec
all: test

test:
	./node_modules/mocha/bin/mocha --reporter $(REPORTER)

test-cov: lib-cov
	@$(MAKE) REPORTER=html-cov > gen/coverage.html

lib-cov:
	jscoverage hearts hearts-cov

stats:
	gitstats . gen/stats

setup:
	npm install
	brew install haskell-platform
	brew install go

.PHONY: test stats clean all
