FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y build-essential libz-dev
RUN apt-get install -y supervisor nginx

WORKDIR /home

COPY supervisord.conf /var/www/supervisord.conf

COPY server server
COPY maps maps

RUN cd server && make all
RUN cd server/htdocs && make all && make install

EXPOSE 80

CMD ["/usr/bin/supervisord", "-c", "/var/www/supervisord.conf"]
