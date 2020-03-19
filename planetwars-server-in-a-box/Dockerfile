FROM ubuntu:latest

RUN apt-get update
RUN apt-get install build-essential -y

WORKDIR /srv
COPY server /srv

RUN make all

