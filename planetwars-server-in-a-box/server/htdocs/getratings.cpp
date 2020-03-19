#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <map>
#include <vector>
#include <string>
#include <zlib.h>

extern "C" {
#include "cgi.h"
#include "html.h"
}

using namespace std;

const char *fn = "ratings.txt";
const char *resultsfn = "results.pgn";
static gzFile gz = NULL;

struct Player {
	char		 name[256];
	unsigned	 rank, games, score, draws;
	int		 elo, plus, minus, oppo;
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

static map<string, pair<unsigned, unsigned> > times;

static void
gettimes()
{
	FILE *f;
	char s[8192], p1[256], p2[256];
	unsigned u;

	if ((f = fopen(resultsfn, "r")) == NULL)
		return;
	while (fgets(s, sizeof(s), f) != NULL) {
		if (strncmp(s, "[White \"", 8))
			continue;
		memset(p1, 0, sizeof(p1));
		sscanf(s + 8, "%255[^\"]", p1);
		fgets(s, sizeof(s), f);
		if (strncmp(s, "[Black \"", 8))
			continue;
		memset(p2, 0, sizeof(p2));
		sscanf(s + 8, "%255[^\"]", p2);
		fgets(s, sizeof(s), f);
		if (strncmp(s, "[Result \"", 9))
			continue;
		if (fscanf(f, "%u", &u) != 1 || !u)
			continue;
		if (times.find(p1) == times.end())
			times.insert(make_pair(p1, make_pair(time(NULL), 0)));
		if (u < times[p1].first)
			times[p1].first = u;
		if (u > times[p1].second)
			times[p1].second = u;
		if (times.find(p2) == times.end())
			times.insert(make_pair(p2, make_pair(time(NULL), 0)));
		if (u < times[p2].first)
			times[p2].first = u;
		if (u > times[p2].second)
			times[p2].second = u;
	}
	fclose(f);
}

static string
timestr(unsigned u)
{
	char s[512];
	bool pre = false;

	u = time(NULL) - u;
	s[0] = 0;
	if (u > 24*60*60) {
		snprintf(s + strlen(s), sizeof(s) - strlen(s), "%ud", u / (24*60*60));
		u = u % (24*60*60);
		pre = true;
	}
	if (u > 60*60 || pre) {
		snprintf(s + strlen(s), sizeof(s) - strlen(s), pre ? "%2.2uh" : "%uh", u / (60*60));
		u = u % (60*60);
		pre = true;
	}
	if (u > 60 || pre) {
		snprintf(s + strlen(s), sizeof(s) - strlen(s), pre ? "%2.2um" : "%um", u / 60);
		u = u % 60;
		pre = true;
	}
	snprintf(s + strlen(s), sizeof(s) - strlen(s), pre ? "%2.2us" : "%us", u);
	return string(s);
}

int main()
{
	const char *e;
	vector<Player> players;
	FILE *f;
	bool skip = true;

	unsigned rank;
	char s[8192];

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
	dprintf("<h2>Current ratings</h2>\n");

	f = fopen(fn, "r");
	while (fgets(s, sizeof(s), f)) {
		if (skip) {
			if (!strncmp(s, "Rank Name ", 10))
				skip = false;
			continue;
		}
		Player p;
		if (sscanf(s, "%u %255s %d %d %d %u %u%% %d %u%%",
		    &p.rank, p.name, &p.elo, &p.plus, &p.minus,
		    &p.games, &p.score, &p.oppo, &p.draws) != 9)
			continue;
		players.insert(players.end(), p);
	}
	fclose(f);

	gettimes();
	dprintf("<table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">\n");
	dprintf("<tr><th>Rank</th><th align=\"left\">Name</th><th align=\"left\">Last (ago)</th><th align=\"left\">First</th><th>Elo</th><th>+</th><th>-</th><th>games</th><th>score</th><th>oppo.</th><th>draws</th></tr>\n");
	for (unsigned i = 0; i < players.size(); ++i) {
		char e[1024];
		Player p = players[i];
		unsigned first, last;
		const char *bg = i % 2 ? "#FFFFFF" : "#DDDDFF";

		first = times[p.name].first;
		last = times[p.name].second;
		string f = timestr(first), l = timestr(last);
		dprintf("<tr bgcolor=\"%s\"><td align=\"right\">%u</td><td><a href=\"getplayer?player=%s\">%s</a></td><td align=\"right\">%s</td><td align=\"right\">%s</td><td align=\"right\"><b>%d</b></td><td align=\"right\">%d</td><td align=\"right\">%d</td><td align=\"right\">%u</td><td align=\"right\">%u%%</td><td align=\"right\">%d</td><td align=\"right\">%u%%</td></tr>\n",
		    bg, p.rank, p.name, html_esc(p.name, e, sizeof(e), 0), l.c_str(), f.c_str(), p.elo, p.plus,
		    p.minus, p.games, p.score, p.oppo, p.draws);
	}
	dprintf("</table>\n");

	dprintf("</body><html>\n");

	if (gz != NULL)
		gzclose(gz);
	else
		fflush(stdout);
	return (0);
}
