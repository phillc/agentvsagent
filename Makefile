build: clean compile
clean:
	rm -rf hearts/server/lib/*
	rm -rf hearts/dist/ruby/lib/*
	rm -rf hearts/dist/nodejs/lib/*
compile:
	thrift -out hearts/server/lib --gen js:node hearts/hearts.thrift
	thrift -out hearts/dist/ruby/lib --gen rb hearts/hearts.thrift
	thrift -out hearts/dist/nodejs/lib --gen js:node hearts/hearts.thrift
