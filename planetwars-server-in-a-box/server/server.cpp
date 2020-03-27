/*
 * Copyright (c) 2010 Daniel Hartmeier <daniel@benzedrine.cx>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *    - Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    - Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials provided
 *      with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/resource.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <fts.h>
#include <poll.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <list>
#include <vector>
#include <map>
#include <string>

#include "board.h"

using namespace std;

enum { STATE_CONNECTED=1, STATE_LOGGEDIN=2, STATE_PLAYING=3, STATE_CLOSED=4 };

const char	*bdn = "maps";
const char	*gameidfn = "data/gameid";
const char	*gamespath = "/var/www/games";
const char	*resultsfn = "data/results.pgn";
const char	*usersfn = "data/users.txt";
const char	*ratingsfn = "data/ratings.txt";
const char	*listen_addr = "0.0.0.0";
const unsigned	 listen_port = 995;
static int	 quit = 0;
static int	 kick = 0;
static int	 shut = 0;
static unsigned	 lastscan = 0;
static unsigned	 gameid = 0;

#define	BUF_SIZE	4*65536
#define	MAX_CONNECTIONS	512

struct Buf {
	char		 buf[BUF_SIZE];
	unsigned	 len;
	unsigned	 off;
};

struct Game;

struct Conn {
	int			 state;
	struct sockaddr_in	 src;
	int			 fd;
	Buf			 in;
	Buf			 out;
	string			 user;
	Game			*game;
	int			 move;
	unsigned long		 timeout;
	int			 elo;
};

struct Game {
	Game(Conn *p1, Conn *p2, const Board &board) : p1(p1), p2(p2),
		board(board), timeout(0), id(++gameid), turns(0) { }
	Conn		*p1;
	Conn		*p2;
	Board		 board;
	unsigned long	 timeout;
	string		 s;
	string		 d;
	unsigned	 id;
	unsigned	 turns;
};

static list<Conn *>	 conns;
static vector<Board>	 boards;
static list<Game *>	 games;

void	 handle_connect(int);
void	 handle_read(Conn *);
void	 handle_write(Conn *);
void	 handle_signal(int);
void	 write_buf(Conn *conn, const void *data, unsigned len);
void	 log(const char *, ...);

static inline unsigned long
usec()
{
	static struct timeval tv;

	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000000 + tv.tv_usec;
}

static void
readBoards(const char *dn)
{
	FTS *fts;
	FTSENT *e;
	char * const path_argv[] = { (char *)dn, NULL };
	list<string> fns;

	if (time(NULL) < lastscan + 20)
		return;
	lastscan = time(NULL);
	log("%u connections, %u games%s%s", (unsigned)conns.size(),
		(unsigned)games.size(), kick ? " KICK" : "", shut ? " SHUT" : "");
	boards.clear();
	fts = fts_open(path_argv, FTS_NOSTAT, 0);
	if (fts == NULL) {
		log("readBoards: fts_open: %s: %s", dn, strerror(errno));
		return;
	}
	while ((e = fts_read(fts)))
		if ((e->fts_info & FTS_F))
			fns.insert(fns.end(), string(e->fts_path));
	fts_close(fts);
	for (list<string>::iterator i = fns.begin(); i != fns.end(); ++i) {
		Board board(i->c_str());
		if (board.isValid())
			boards.insert(boards.end(), board);
		else
			log("readBoards: board %s is not valid", i->c_str());
	}
}

static void
dropIdle()
{
	for (list<Conn *>::iterator c = conns.begin(); c != conns.end(); ++c) {
		if ((*c)->state == STATE_CONNECTED && (*c)->timeout < usec()) {
			log("dropping idle connection from %s",
				(*c)->user.c_str());
			(*c)->state = STATE_CLOSED;
		}
	}
}

static inline int
absdiff(int a, int b)
{
	return a < b ? b - a : a - b;
}

static void
startNewGame()
{
	unsigned i;
	vector<Conn *> v, w;
	Conn *p1, *p2;
	char n[8192];

	if (boards.size() < 1)
		return;

	if (shut) {
		for (list<Conn *>::iterator c = conns.begin();
			c != conns.end(); ++c) {
			if ((*c)->state == STATE_CONNECTED ||
				(*c)->state == STATE_LOGGEDIN) {
				snprintf(n, sizeof(n), "INFO Server is "
					"shutting down, waiting for %u games to "
					"complete. Try again in a couple of "
					"seconds.\n", (unsigned)games.size());
				write_buf(*c, n, strlen(n));
				(*c)->state = STATE_CLOSED;
			}
		}
		return;
	}
	/* pick the first player among those that waited */
	for (list<Conn *>::iterator c = conns.begin(); c != conns.end(); ++c)
		if ((*c)->state == STATE_LOGGEDIN &&
			(*c)->timeout < usec())
			v.insert(v.end(), *c);
	if (v.size() < 1)
		return;
	i = rand() % v.size();
	p1 = v[i];

	/* pick the second player among all that are ready */
	v.clear();
	for (list<Conn *>::iterator c = conns.begin(); c != conns.end(); ++c)
		if ((*c)->state == STATE_LOGGEDIN &&
			(*c)->user != p1->user)
			v.insert(v.end(), *c);
	if (v.size() < 1)
		return;

	/* from those that are ready, pick the one with the closest elo */
	p2 = v[0];
	w.insert(w.end(), p2);
	for (i = 1; i < v.size(); ++i) {
		if (absdiff(p1->elo, v[i]->elo) == absdiff(p1->elo, p2->elo))
			w.insert(w.end(), v[i]);
		else if (absdiff(p1->elo, v[i]->elo) <
			absdiff(p1->elo, p2->elo)) {
			w.clear();
			w.insert(w.end(), v[i]);
			p2 = v[i];
		}
	}
	i = rand() % w.size();
	p2 = w[i];
	i = rand() % boards.size();
	Game *g = new Game(p1, p2, boards[i]);
	p1->state = p2->state = STATE_PLAYING;
	p1->game = p2->game = g;
	snprintf(n, sizeof(n), "%u\n%u\n%s\n",
		(unsigned)time(NULL), g->id, g->board.getFn().c_str());
	g->s = n;
	g->d = p1->user + "\n" + p2->user + "\n" + g->board.getFn() + "\n";
	games.insert(games.end(), g);
	snprintf(n, sizeof(n), "INFO Your map is %s\n",
		g->board.getFn().c_str());
	write_buf(p1, n, strlen(n));
	write_buf(p2, n, strlen(n));
	snprintf(n, sizeof(n), "INFO Your opponent is %s with %d Elo\n",
		p2->user.c_str(), p2->elo);
	write_buf(p1, n, strlen(n));
	snprintf(n, sizeof(n), "INFO Your opponent is %s with %d Elo\n",
		p1->user.c_str(), p1->elo);
	write_buf(p2, n, strlen(n));
	snprintf(n, sizeof(n), "INFO This is game_id=%u\n", g->id);
	write_buf(p1, n, strlen(n));
	write_buf(p2, n, strlen(n));
	g->board.print(1, n, sizeof(n));
	write_buf(p1, n, strlen(n));
	g->board.print(2, n, sizeof(n));
	write_buf(p2, n, strlen(n));
	write_buf(p1, "go\n", 3);
	write_buf(p2, "go\n", 3);
	p1->move = p2->move = 0;
	g->timeout = usec() + 10000000;
	log("started new game between %s and %s on map %s",
		p1->user.c_str(), p2->user.c_str(), g->board.getFn().c_str());
}

/* WIN=p1 wins, LOSS=p2 wins */
static void
endGame(Game *g, RESULT r, string s = "")
{
	FILE *f;
	char n[8192];

	snprintf(n, sizeof(n), "turns: %u\n", g->turns);
	g->s += n;
	if (!s.empty())
		g->s += s + "\n";
	if ((f = fopen(gameidfn, "w"))) {
		fprintf(f, "%u\n", gameid);
		fflush(f);
		fclose(f);
	} else
		log("ERROR: fopen: %s: %s", gameidfn, strerror(errno));
	snprintf(n, sizeof(n), "%s/%u", gamespath, g->id);
	if ((f = fopen(n, "w"))) {
		fprintf(f, "%s\n", g->d.c_str());
		fflush(f);
		fclose(f);
	} else
		log("ERROR: fopen: %s: %s", n, strerror(errno));
	if ((f = fopen(resultsfn, "a"))) {
		fprintf(f, "[White \"%s\"]\n", g->p1->user.c_str());
		fprintf(f, "[Black \"%s\"]\n", g->p2->user.c_str());
		fprintf(f, "[Result \"%s\"]\n",
			(r == DRAW ? "1/2-1/2" : (r == WIN ? "1-0" : "0-1")));
		fprintf(f, "%s\n", g->s.c_str());
		fflush(f);
		fclose(f);
	} else
		log("ERROR: fopen: %s: %s", resultsfn, strerror(errno));
	if (r == DRAW)
		log("game between %s and %s ends: draw",
			g->p1->user.c_str(), g->p2->user.c_str());
	else
		log("game between %s and %s ends after %u turns: %s wins%s%s%s",
			g->p1->user.c_str(), g->p2->user.c_str(), g->turns,
			r == WIN ? g->p1->user.c_str() : g->p2->user.c_str(),
			s.empty() ? "" : " (", s.c_str(), s.empty() ? "" : ")");
	if (r == WIN)
		snprintf(n, sizeof(n), "INFO You WIN against %s\n",
			g->p2->user.c_str());
	else if (r == LOSS)
		snprintf(n, sizeof(n), "INFO You LOSE against %s\n",
			g->p2->user.c_str());
	else
		snprintf(n, sizeof(n), "INFO You DRAW with %s\n",
			g->p2->user.c_str());
	if (g->p1->state == STATE_PLAYING)
		write_buf(g->p1, n, strlen(n));
	if (r == WIN)
		snprintf(n, sizeof(n), "INFO You LOSE against %s\n",
			g->p1->user.c_str());
	else if (r == LOSS)
		snprintf(n, sizeof(n), "INFO You WIN against %s\n",
			g->p1->user.c_str());
	else
		snprintf(n, sizeof(n), "INFO You DRAW with %s\n",
			g->p1->user.c_str());
	if (g->p2->state == STATE_PLAYING)
		write_buf(g->p2, n, strlen(n));
	g->p1->game = g->p2->game = 0;
	g->p1->move = g->p2->move = 0;
	g->p1->state = g->p2->state = STATE_CLOSED;
	delete g;
}

static void
checkRunningGames()
{
	static char n[65536];

	list<Game *>::iterator g = games.begin();
	while (g != games.end()) {
		if (kick) {
			snprintf(n, sizeof(n), "INFO Server shutting down "
				"immediately, this game is aborted and not "
				"counted. Sorry.\n");
			write_buf((*g)->p1, n, strlen(n));
			write_buf((*g)->p2, n, strlen(n));
			(*g)->p1->state = STATE_CLOSED;
			(*g)->p1->game = 0;
			(*g)->p2->state = STATE_CLOSED;
			(*g)->p2->game = 0;
			delete *g;
			g = games.erase(g);
			continue;
		}
		if ((*g)->p1->move > 0 && (*g)->p2->move > 0) {
			/* both players have made their moves */
			(*g)->p1->move = 0;
			(*g)->p2->move = 0;
			(*g)->board.nextTurn();
			(*g)->turns++;
			RESULT r = (*g)->board.rate();
			(*g)->d += (*g)->board.describe() + "\n";
			if (r == UNDECIDED) {
				(*g)->board.print(1, n, sizeof(n));
				write_buf((*g)->p1, n, strlen(n));
				(*g)->board.print(2, n, sizeof(n));
				write_buf((*g)->p2, n, strlen(n));
				write_buf((*g)->p1, "go\n", 3);
				write_buf((*g)->p2, "go\n", 3);
				(*g)->timeout = usec() + 5000000;
				++g;
			} else {
				endGame(*g, r);
				g = games.erase(g);
			}
			continue;
		}
		if ((*g)->timeout < usec()) {
			log("game timed out");
			if ((*g)->p1->move > 0) {
				snprintf(n, sizeof(n), "INFO %s timed out\n",
					(*g)->p2->user.c_str());
				write_buf((*g)->p1, n, strlen(n));
				write_buf((*g)->p2, n, strlen(n));
				endGame(*g, WIN, "TIMEOUT");
			} else if ((*g)->p2->move > 0) {
				snprintf(n, sizeof(n), "INFO %s timed out\n",
					(*g)->p1->user.c_str());
				write_buf((*g)->p1, n, strlen(n));
				write_buf((*g)->p2, n, strlen(n));
				endGame(*g, LOSS, "TIMEOUT");
			} else {
				snprintf(n, sizeof(n), "INFO both players "
					"timed out\n");
				write_buf((*g)->p1, n, strlen(n));
				write_buf((*g)->p2, n, strlen(n));
				endGame(*g, DRAW, "TIMEOUT");
			}
			g = games.erase(g);
			continue;
		}
		++g;
	}
}

int main(int argc, char *argv[])
{
	FILE *f;
	int listen_fd = -1;
	int val;
	struct sockaddr_in sa;
	struct pollfd fds[MAX_CONNECTIONS];
	struct rlimit rl;

	signal(SIGTERM, handle_signal);
	signal(SIGINT, handle_signal);
	signal(SIGPIPE, handle_signal);
	signal(SIGUSR1, handle_signal);
	signal(SIGUSR2, handle_signal);

	srand(time(NULL));
	if (getrlimit(RLIMIT_NOFILE, &rl)) {
		perror("main: getrlimit");
		goto error;
	}
	if (rl.rlim_cur < 1024) {
		if (rl.rlim_max < 1024) {
			fprintf(stderr, "main: rl.rlim_max %d < 1024\n",
				(int)rl.rlim_max);
			goto error;
		}
		rl.rlim_cur = 1024;
		if (setrlimit(RLIMIT_NOFILE, &rl)) {
			perror("main: setrlimit");
			goto error;
		}
	}
	readBoards(bdn);
	if ((f = fopen(gameidfn, "r")) != NULL) {
		fscanf(f, "%u", &gameid);
		fclose(f);
		log("highest gameid %u", gameid);
	}

	if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("main: socket");
		goto error;
	}

	if (fcntl(listen_fd, F_SETFL, fcntl(listen_fd, F_GETFL) | O_NONBLOCK)) {
		perror("main: fcntl");
		goto error;
	}

	val = 1;
	if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&val,
		sizeof(val))) {
		perror("main: setsockopt");
		goto error;
	}

	memset(&sa, 0, sizeof(sa));
	sa.sin_family = AF_INET;
	sa.sin_addr.s_addr = inet_addr(listen_addr);
	sa.sin_port = htons(listen_port);
	if (bind(listen_fd, (const struct sockaddr *)&sa, sizeof(sa))) {
		perror("main: bind");
		goto error;
	}

	if (listen(listen_fd, 1024)) {
		perror("main: listen");
		goto error;
	}

	log("ready, accepting connections");

	while (!quit) {
		int nfds = 0;
		int r;
		int i;

		if (shut && games.empty())
			break;
		fds[nfds].fd = listen_fd;
		fds[nfds].revents = 0;
		fds[nfds++].events = POLLIN;

		for (list<Conn *>::iterator c = conns.begin();
		c != conns.end(); ++c) {
			fds[nfds].fd = (*c)->fd;
			fds[nfds].revents = 0;
			fds[nfds].events = 0;
			if ((*c)->out.len)
				fds[nfds].events |= POLLOUT;
			fds[nfds].events |= POLLIN;
			nfds++;
			if (nfds + 1 > MAX_CONNECTIONS)
				break;
		}

		r = poll(fds, nfds, 100);
		if (r < 0) {
			if (errno != EINTR) {
				perror("main: poll");
				goto error;
			}
			continue;
		} else if (r == 0) {
			readBoards(bdn);
			dropIdle();
			checkRunningGames();
			startNewGame();
			continue;
		}

		if (fds[0].revents & POLLIN)
			handle_connect(listen_fd);

		list<Conn *>::iterator c = conns.begin();
		for (i = 1; i < nfds; ++i) {
			if (fds[i].revents & POLLIN)
				handle_read(*c);
			if (fds[i].revents & POLLOUT)
				handle_write(*c);
			if ((*c)->state == STATE_CLOSED) {
				log("player %s disconnected",
					(*c)->user.c_str());
				if ((*c)->game) {
					char n[512];
					list<Game *>::iterator g =
						games.begin();
					while (g != games.end()) {
						if (*g == (*c)->game) {
							games.erase(g);
							break;
						}
						++g;
					}
					snprintf(n, sizeof(n), "INFO Opponent "
						"disconnected\n");
					if (*c == (*c)->game->p1) {
						if ((*c)->game->p2->state ==
							STATE_PLAYING)
							write_buf(
								(*c)->game->p2, n,
								strlen(n));
						endGame((*c)->game, LOSS,
							"DISCONNECT");
					} else {
						if ((*c)->game->p1->state ==
							STATE_PLAYING)
							write_buf(
								(*c)->game->p1, n,
								strlen(n));
						endGame((*c)->game, WIN,
							"DISCONNECT");
					}
				}
				close((*c)->fd);
				delete *c;
				c = conns.erase(c);
			} else
				c++;
		}

	}
	log("quitting gracefully");

error:
	if (listen_fd != -1)
		close(listen_fd);
	while (!conns.empty()) {
		close((*conns.begin())->fd);
		delete *conns.begin();
		conns.erase(conns.begin());
	}
	return 0;
}

void
handle_connect(int listen_fd)
{
	Conn *conn;
	struct sockaddr_in sa;
	socklen_t len;
	char u[1024];

	conn = new Conn;
	conn->fd = -1;
	conn->elo = -999999;
	conn->game = NULL;

	memset(&sa, 0, sizeof(sa));
	len = sizeof(sa);
	conn->fd = accept(listen_fd, (struct sockaddr *)&sa, &len);
	if (conn->fd < 0 || len != sizeof(sa)) {
		perror("handle_connect: accept");
		goto error;
	}

	log("connection from %s:%i", inet_ntoa(sa.sin_addr),
		ntohs(sa.sin_port));

	conn->src.sin_family = AF_INET;
	conn->src.sin_addr.s_addr = sa.sin_addr.s_addr;
	conn->src.sin_port = sa.sin_port;
	conn->in.len = 0;
	conn->in.off = 0;
	conn->out.len = 0;
	conn->out.off = 0;
	conn->state = STATE_CONNECTED;
	snprintf(u, sizeof(u), "%s", inet_ntoa(sa.sin_addr));
	conn->user = u;
	conn->timeout = usec() + 5000000;

	conns.insert(conns.end(), conn);
	return;

error:
	if (conn && conn->fd != -1)
		close(conn->fd);
	if (conn)
		delete conn;
}

void
write_buf(Conn *conn, const void *data, unsigned len)
{
	if (len > sizeof(conn->out.buf) - conn->out.off - conn->out.len) {
		log("write_buf: len %u > sizeof %u - off %u - len %u",
			len, (unsigned)sizeof(conn->out.buf), conn->out.off,
			conn->out.len);
		return;
	}
	memcpy(conn->out.buf + conn->out.off + conn->out.len, data, len);
	conn->out.len += len;
}

static int
read_elo(const char *user)
{
	FILE *f;
	char s[512], u[256];
	int rank, elo;
	bool skip = true;

	if (!(f = fopen(ratingsfn, "r"))) {
		log("read_elo: fopen: %s: %s", ratingsfn, strerror(errno));
		return -999999;
	}
	while (fgets(s, sizeof(s), f)) {
		if (skip) {
			if (!strncmp(s, "Rank Name", 9))
				skip = false;
			continue;
		}
		if (sscanf(s, "%d %255s %d", &rank, u, &elo) != 3)
			continue;
		if (!strcmp(u, user)) {
			log("player %s has elo %d", user, elo);
			fclose(f);
			return elo;
		}
	}
	fclose(f);
	return -999999;
}

bool
check_login(const char *user, const char *password)
{
	FILE *f;
	char s[512], u[256], p[256];

	if ((f = fopen(usersfn, "r")) == NULL) {
		log("check_login: fopen: %s: %s", usersfn, strerror(errno));
		return true;
	}
	while (fgets(s, sizeof(s), f) != NULL) {
		if (sscanf(s, "%255s %255s", u, p) != 2) {
			log("check_login: sscanf: %s", s);
			continue;
		}
		if (!strcmp(u, user)) {
			fclose(f);
			return strcmp(p, password);
		}
	}
	fclose(f);
	// user not found in file, allow login
	if (password[0]) {
		// new user sets a password
		if ((f = fopen(usersfn, "a")) == NULL) {
			log("check_login: fopen: %s: %s",
				usersfn, strerror(errno));
		} else {
			fprintf(f, "%s %s\n", user, password);
			fflush(f);
			fclose(f);
		}
	}
	return false;
}

/* returns false to stop reading from conn */
bool
handle_line(Conn *conn, const char *s)
{
	// ignore empty lines, comments, and spurious CR
	if (!s[0] || s[0] == '#' || !strcmp(s, "\r"))
		return true;
	switch (conn->state) {
	case STATE_CONNECTED: {
		if (!strncmp(s, "USER ", 5)) {
			char u[256], p[256];
			int r;

			r = sscanf(s + 5, "%255s PASS %255s", u, p);
			if (r == 1)
				p[0] = 0;
			else if (r != 2) {
				log("handle_line: sscanf: '%s'", s);
				break;
			}
			if (check_login(u, p)) {
				if (p[0]) {
					log("login failed for user '%s' with "
						"password '%s'", u, p);
					snprintf(u, sizeof(u),
						"INFO Password rejected\n");
				} else {
					log("login failed for user '%s' "
						"without password", u);
					snprintf(u, sizeof(u),
						"INFO Password required\n");
				}
				write_buf(conn, u, strlen(u));
				break;
			}
			log("connection %s identified as %s",
				conn->user.c_str(), u);
			conn->user = u;
			conn->elo = read_elo(u);
			snprintf(u, sizeof(u), "INFO You currently have "
				"%d Elo\n", conn->elo);
			write_buf(conn, u, strlen(u));
			snprintf(u, sizeof(u), "INFO There are currently "
				"%u connections and %u games running\n",
				(unsigned)conns.size(), (unsigned)games.size());
			write_buf(conn, u, strlen(u));
			conn->state = STATE_LOGGEDIN;
			conn->timeout = usec() + 5000000;
		}
		break;
	}
	case STATE_PLAYING: {
		int src, dst, ships;
		int player = conn == conn->game->p1 ? 1 : 2;
		char n[8192];

		list<Game *>::iterator game = games.begin();
		while (game != games.end() && *game != conn->game)
			game++;
		if (game == games.end()) {
			log("handle_line: game not found");
			return false;
		}
		if (conn->move) {
			log("handle_line: player %s: already moving: '%s'",
				conn->user.c_str(), s);
			snprintf(n, sizeof(n), "INFO You sent before I said "
				"go: '%s'\n", s);
			write_buf(conn, n, strlen(n));
			endGame(conn->game, player == 1 ? LOSS : WIN,
				"INVALID");
			games.erase(game);
			return false;
		}
		if (!strncmp(s, "go", 2)) {
			conn->move = 1;
			break;
		}
		if (sscanf(s, "%d %d %d", &src, &dst, &ships) != 3) {
			log("handle_line: player %s: sscanf: '%s'",
				conn->user.c_str(), s);
			snprintf(n, sizeof(n), "INFO You sent an invalid "
				"line: '%s'\n", s);
			write_buf(conn, n, strlen(n));
			endGame(conn->game, player == 1 ? LOSS : WIN,
				"INVALID");
			games.erase(game);
			return false;
		}
		if (!conn->game || (conn != conn->game->p1 &&
			conn != conn->game->p2)) {
			log("handle_line: player %s: !conn->game",
				conn->user.c_str());
			break;
		}
		string err;
		if (conn->game->board.issueOrder(player, src, dst, ships,
			err)) {
			log("handle_line: player %s: issueOrder(src %d, "
				"dst %d, ships %d) failed: %s", conn->user.c_str(),
				src, dst, ships, err.c_str());
			snprintf(n, sizeof(n), "INFO You issued an invalid "
				"order: src %d, dst %d, ships %d: %s\n",
				src, dst, ships, err.c_str());
			write_buf(conn, n, strlen(n));
			endGame(conn->game, player == 1 ? LOSS : WIN,
				"INVALID");
			games.erase(game);
			return false;
		}
		break;
	}
	default:
		log("handle_line: player %s: unexpected line '%s'",
			conn->user.c_str(), s);
	}
	return true;
}

void
handle_read(Conn *conn)
{
	int len;

	if (conn->in.off + conn->in.len + 1 >= sizeof(conn->in.buf))
		return;
	len = read(conn->fd, conn->in.buf + conn->in.off +
		conn->in.len, sizeof(conn->in.buf) - conn->in.off -
		conn->in.len - 1);
	if (len < 0) {
		if (errno != EINTR) {
			if (errno != ECONNRESET)
				perror("handle_read: read");
			conn->state = STATE_CLOSED;
		}
		return;
	} else if (len == 0) {
		conn->state = STATE_CLOSED;
		return;
	}
	conn->in.len += len;
	conn->in.buf[conn->in.off + conn->in.len] = 0;

	char *p;
	while ((p = strchr(conn->in.buf + conn->in.off, '\n'))) {
		*p = 0;
		len = (p - conn->in.buf) - conn->in.off;
		if (!handle_line(conn, conn->in.buf + conn->in.off))
			return;
		conn->in.off += len + 1;
		conn->in.len -= len + 1;

	}

	if (!conn->in.len)
		conn->in.off = 0;
}

void
handle_write(Conn *conn)
{
	int len;

	if (!conn->out.len) {
		log("handle_write: len 0");
		return;
	}
	len = write(conn->fd, conn->out.buf + conn->out.off,
		conn->out.len);
	if (len < 0) {
		if (errno != EINTR) {
			if (errno != EPIPE)
				perror("handle_write: write");
			conn->state = STATE_CLOSED;
		}
		return;
	}
	conn->out.len -= len;
	if (!conn->out.len)
		conn->out.off = 0;
	else
		conn->out.off += len;
}

void
handle_signal(int sigraised)
{
	switch (sigraised) {
	case SIGTERM:
	case SIGINT:
		quit = 1;
		break;
	case SIGUSR2:
		kick = 1;
	case SIGUSR1:
		shut = 1;
		break;
	case SIGPIPE:
		signal(SIGPIPE, handle_signal);
		break;
	}
}

void
log(const char *format, ...)
{
	time_t t;
	struct tm *tm;
	va_list ap;

	t = time(0);
	tm = localtime(&t);
	if (tm)
		fprintf(stderr, "%4.4i.%2.2i.%2.2i %2.2i:%2.2i:%2.2i ",
			tm->tm_year+1900, tm->tm_mon+1, tm->tm_mday,
			tm->tm_hour, tm->tm_min, tm->tm_sec);
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	fflush(stderr);
}
