#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <utility>
#include <vector>
#include <map>
#include <string>
#include <zlib.h>

extern "C" {
#include "cgi.h"
}

using namespace std;

const char *fn = "results.pgn";
static gzFile gz = NULL;

struct Game {
	string p1, p2;
	string result;
	string ts;
	string id;
	string map;
};

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
	map<string, pair<unsigned, unsigned> > maps;
	map<string, string> last;
	struct query *q = get_query();
	FILE *f;
	char s[8192], p1[256], p2[256], r[256];
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
	dprintf("<h2>Map statistics</h2>\n");
	dprintf("You can download the <a href=\"/maps.tar.gz\">complete map set</a> (360 kB).<p>\n");
	f = fopen(fn, "r");
	while (fgets(s, sizeof(s), f)) {
		if (!strncmp(s, "[White \"", 8)) {
			Game g;
			memset(p1, 0, sizeof(p1));
			sscanf(s + 8, "%255[^\"]", p1);
			g.p1 = p1;
			fgets(s, sizeof(s), f);
			if (!strncmp(s, "[Black \"", 8)) {
				memset(p2, 0, sizeof(p2));
				sscanf(s + 8, "%255[^\"]", p2);
				g.p2 = p2;
				fgets(s, sizeof(s), f);
				if (!strncmp(s, "[Result \"", 9)) {
					memset(r, 0, sizeof(r));
					sscanf(s + 9, "%255[^\"]", r);
					s[strlen(s) - 1] = 0;
					if (!strcmp(r, "1/2-1/2"))
						g.result = "DRAW";
					else
						g.result = "WIN";
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					g.ts = s;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					g.id = s;
					fgets(s, sizeof(s), f);
					s[strlen(s) - 1] = 0;
					g.map = s;
					games.insert(games.end(), g);
				}
			}
		}
	}
	fclose(f);
	for (unsigned i = 0; i < games.size(); ++i) {
		map<string, pair<unsigned, unsigned> >::iterator m =
		    maps.find(games[i].map);
		if (m == maps.end())
			if (games[i].result == "DRAW")
				maps.insert(make_pair(games[i].map, make_pair((unsigned)1, (unsigned)0)));
			else
				maps.insert(make_pair(games[i].map, make_pair((unsigned)0, (unsigned)1)));
		else {
			if (games[i].result == "DRAW")
				m->second.first++;
			else
				m->second.second++;
		}
		last[games[i].map] = games[i].id;
	}
	dprintf("<table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">\n");
	dprintf("<tr><th align=\"left\">Name</th><th align=\"left\">Games</th><th align=\"left\">Draws (%%)</th><th align=\"left\">Last</th></tr>\n");
	unsigned tg = 0, td = 0;
	unsigned i = 0;
	for (map<string, pair<unsigned, unsigned> >::iterator m = maps.begin();
	    m != maps.end(); ++m) {
		const char *gid = last[m->first.c_str()].c_str();
		const char *bg = i++ % 2 ? "#FFFFFF" : "#DDDDFF";
		unsigned g = m->second.first + m->second.second;
		unsigned d = m->second.first;
		tg += g;
		td += d;
		dprintf("<tr bgcolor=\"%s\"><td><a href=\"%s\">%s</a></td><td align=\"right\">%u</td><td align=\"right\">%u (%u%%)</td><td><a href=\"canvas?game_id=%s\">%s</a></td></tr>\n", bg, m->first.c_str(), m->first.c_str(), g, d, g > 0 ? d * 100 / g : 0, gid, gid);
	}
	const char *bg = i++ % 2 ? "#FFFFFF" : "#DDDDFF";
	dprintf("<tr bgcolor=\"%s\"><td>Total</td><td align=\"right\">%u</td><td align=\"right\">%u (%u%%)</td></tr>\n", bg, tg, td, tg > 0 ? td * 100 / tg : 0);
	dprintf("</table>\n");
	dprintf("</body><html>\n");
	if (gz != NULL)
		gzclose(gz);
	else
		fflush(stdout);
	return (0);
}
