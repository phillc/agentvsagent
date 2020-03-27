#!/bin/bash
set -e

rm /home/data/rate.lock
touch /home/data/users.txt
touch /home/data/ratings.txt
touch /home/data/results.pgn

mkdir -p /var/www/tmp
touch /var/www/results.pgn
touch /var/www/ratings.txt

exec "$@"
