FROM ubuntu:latest

RUN apt-get update
RUN apt-get install build-essential -y
RUN apt-get install libz-dev -y

WORKDIR /var/www
COPY server /var/www
COPY maps /var/www/maps

RUN make clean
RUN make all
RUN cd htdocs && make clean && make all

RUN ls /var/www

EXPOSE 80
