FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y build-essential libz-dev
RUN apt-get install -y supervisor
RUN apt-get install -y apache2

RUN a2enmod cgi # enable apache cgi module

WORKDIR /home

COPY supervisord.conf /var/www/supervisord.conf
COPY apache-config.conf /etc/apache2/sites-enabled/000-default.conf
COPY server server
COPY maps maps

RUN cd server && make all
RUN cd server/htdocs && make all && make install

EXPOSE 80
EXPOSE 995

CMD ["/usr/bin/supervisord", "-c", "/var/www/supervisord.conf"]
