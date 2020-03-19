#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <vector>
#include <string>
#include <zlib.h>

extern "C" {
#include "cgi.h"
#include "html.h"
}

using namespace std;

const char *fn = "results.pgn";
static gzFile gz = NULL;

struct Game {
	string p1, p2;
	string opponent;
	string result;
	string ts;
	string map;
	string id;
	string turns;
	string error;
};

static char *
timestr(time_t t)
{
	static char s[128];
	struct tm *tm;

	tm = gmtime(&t);
	snprintf(s, sizeof(s), "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
	    tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
	    tm->tm_hour, tm->tm_min, tm->tm_sec);
	return s;
}

static void
dprintf(const char *fmt, ...)
{
	static char s[65536];
	va_list ap;
	int r;

	va_start(ap, fmt);
	r = vsnprintf(s, sizeof(s), fmt, ap);
	va_end(ap);
	if (gz != NULL)
		r = gzputs(gz, s);
	else
		fprintf(stdout, "%s", s);
}

int main()
{
	const char *e;
	vector<Game> games;
	struct query *q = get_query();
	const char *player = get_query_param(q, "player");
	FILE *f;
	char s[8192], p1[256], p2[256], r[256], ee[1024];
	int w, h;

	printf("Connection: close\n");
	printf("Content-Type: text/html\n");
	if ((e = getenv("HTTP_ACCEPT_ENCODING")) != NULL &&
	    strstr(e, "gzip") != NULL) {
		printf("Content-Encoding: gzip\n\n");
		fflush(stdout);
		gz = gzdopen(fileno(stdout), "wb9");
	} else
		printf("\n");

	dprintf("<html><body>\n");
	if (!player || !player[0]) {
		dprintf("<b>Error: player not specified</b><br>\n");
		goto done;
	}
	dprintf("<h2>Latest games of %s</h2>\n", html_esc(player, ee, sizeof(ee), 0));
	f = fopen(fn, "r");
	while (fgets(s, sizeof(s), f)) {
		if (!strncmp(s, "[White \"", 8)) {
			memset(p1, 0, sizeof(p1));
			sscanf(s + 8, "%255[^\"]", p1);
			fgets(s, sizeof(s), f);
			if (!strncmp(s, "[Black \"", 8)) {
				memset(p2, 0, sizeof(p2));
				sscanf(s + 8, "%255[^\"]", p2);
				fgets(s, sizeof(s), f);
				if (!strncmp(s, "[Result \"", 9)) {
					memset(r, 0, sizeof(r));
					sscanf(s + 9, "%255[^\"]", r);
					Game game;
					game.p1 = p1;
					game.p2 = p2;
					if (!strcmp(p1, player)) {
						game.opponent = p2;
						if (!strcmp(r, "0-1"))
							game.result = "Loss";
						else if (!strcmp(r, "1-0"))
							game.result = "Win";
						else
							game.result = "Draw";
					} else if (!strcmp(p2, player)) {
						game.opponent = p1;
						if (!strcmp(r, "0-1"))
							game.result = "Win";
						else if (!strcmp(r, "1-0"))
							game.result = "Loss";
						else
							game.result = "Draw";
					} else
						continue;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					game.ts = s;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					game.id = s;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					game.map = s;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					if (!strncmp(s, "turns: ", 7)) {
						game.turns = s + 7;
						fgets(s, sizeof(s), f);
						s[strlen(s) - 1] = 0;
					}
					game.error = s;
					games.insert(games.end(), game);
				}
			}
		}
	}
	fclose(f);
	if (!games.size()) {
		dprintf("No games found.<p>\n");
		goto done;
	}

	dprintf("Found %u games.<p>\n", (unsigned)games.size());
	dprintf("<table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">\n");
	dprintf("<tr><th align=\"left\">Date Time (UTC)</th><th align=\"left\">Opponent</th><th align=\"left\">Turns</th><th align=\"left\">Outcome</th></tr>\n");
	for (unsigned i = 0; i < 32 && i < games.size(); ++i) {
		const char *bg = i % 2 ? "#FFFFFF" : "#DDDDFF";
		Game g = games[games.size() - i - 1];
		const char *fg = g.result == "Draw" ? "#000000" : (g.result == "Win" ? "#00AA00" : "#EE0000");
		string remark;
		if (g.error == "INVALID")
			remark = "<sup><small>I</small></sup>";
		else if (g.error == "DISCONNECT")
			remark = "<sup><small>D</small></sup>";
		else if (g.error == "TIMEOUT")
			remark = "<sup><small>T</small></sup>";
		dprintf("<tr bgcolor=\"%s\"><td>%s</td><td><a href=\"getplayer?player=%s\">%s</a></td><td align=\"right\">%s</td><td><font color=\"%s\">%s%s</font></td><td><a href=\"canvas?game_id=%s\">View Game &gt;&gt;</a></td></tr>\n", bg, timestr(atol(g.ts.c_str())), g.opponent.c_str(), g.opponent.c_str(), g.turns.c_str(), fg, g.result.c_str(), remark.c_str(), g.id.c_str(), g.p1.c_str(), g.p2.c_str());
	}
	dprintf("</table>\n");
	dprintf("<p><small><sup>T</sup>=timed out, <sup>D</sup>=disconnected, <sup>I</sup>=invalid input</small>\n");
done:
	dprintf("</body><html>\n");
	if (gz != NULL)
		gzclose(gz);
	else
		fflush(stdout);
	return (0);
}
