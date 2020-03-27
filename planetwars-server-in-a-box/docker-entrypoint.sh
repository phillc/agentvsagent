#!/bin/bash
set -e

touch /home/data/users.txt
touch /home/data/ratings.txt
touch /home/data/results.pgn

mkdir -p /var/www/tmp
touch /var/www/results.pgn

exec "$@"
