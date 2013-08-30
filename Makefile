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
	brew install thrift --with-haskell --HEAD
	gem install thrift

update-thrift:
	git submodule init
	git submodule update

package-thrift:
	mkdir -p node_modules
	rm -rf node_modules/thrift
	cp -r vendor/thrift/lib/nodejs node_modules/thrift
	git add -A -f node_modules/thrift
	cp vendor/thrift/lib/js/thrift.js web/public/javascripts/thrift.js
	git add web/public/javascripts/thrift.js

.PHONY: test stats clean all
