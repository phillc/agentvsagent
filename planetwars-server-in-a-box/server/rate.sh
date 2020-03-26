#!/bin/sh
set -e
set -x

DIR=/home/data
WWW=/var/www

cd ${DIR}

if [ -f rate.lock ]; then
	exit 0
fi
touch rate.lock

../server/expire 168
chgrp -R www-data ${WWW}/games
chmod -R 0640 ${WWW}/games
chmod 0750 ${WWW}/games

# to build bayeselo binary:
# fetch http://remi.coulom.free.fr/Bayesian-Elo/bayeselo.tar.bz2
# extract here, cd BayesElo, make
printf "prompt off\nreadpgn results.pgn\nelo\nmm\nadvantage 0\nexactdist\nratings\nx\nx\n" | ../server/BayesElo/bayeselo >ratings.txt.new 2>&1

mv ratings.txt.new ratings.txt

cp ratings.txt ${WWW}/tmp/ratings.txt
cp results.pgn ${WWW}/tmp/results.pgn
chgrp www-data ${WWW}/tmp/ratings.txt ${WWW}/tmp/results.pgn
chmod o-rwx ${WWW}/tmp/ratings.txt ${WWW}/tmp/results.pgn
mv ${WWW}/tmp/results.pgn ${WWW}/results.pgn
mv ${WWW}/tmp/ratings.txt ${WWW}/ratings.txt

rm -f rate.lock
