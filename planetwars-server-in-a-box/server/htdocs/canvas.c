#include <sys/types.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <zlib.h>

#include "cgi.h"

const char *path = "/games";
static gzFile gz = NULL;

static void
odprintf(const char *fmt, ...)
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
	struct query *q = get_query();
	const char *game_id = get_query_param(q, "game_id");
	unsigned id;
	const char *e;
	FILE *f, *fm, *fg, *fh;
	char s[8192], p1[256], p2[256];

	printf("Connection: close\n");
	printf("Content-Type: text/html\n");
	if ((e = getenv("HTTP_ACCEPT_ENCODING")) != NULL &&
	    strstr(e, "gzip") != NULL) {
		printf("Content-Encoding: gzip\n\n");
		fflush(stdout);
		gz = gzdopen(fileno(stdout), "wb9");
	} else
		printf("\n");

	if (game_id == NULL || !game_id[0]) {
		odprintf("Error: missing parameter game_id");
		return 0;
	}
	if (sscanf(game_id, "%u", &id) != 1 || !id) {
		odprintf("Error: malformed parameter game_id");
		return 0;
	}

	if ((fh = fopen("canvas.html", "r")) == NULL) {
		odprintf("Error: fopen: canvas.html: %s\n", strerror(errno));
		return 0;
	}
	while (fgets(s, sizeof(s), fh) != NULL) {
		if (!strncmp(s, "%%SCRIPT%%", 8))
			break;
		odprintf("%s", s);
	}

	int turns = 0;
	snprintf(s, sizeof(s), "%s/%u", path, id);
	if ((f = fopen(s, "r")) == NULL) {
		odprintf("Error: fopen: %s: %s", s, strerror(errno));
		return 0;
	}
	fgets(s, sizeof(s), f);
	sscanf(s, "%255s", p1);
	fgets(s, sizeof(s), f);
	sscanf(s, "%255s", p2);
	fgets(s, sizeof(s), f);
	s[strlen(s) - 1] = 0;

	if ((fm = fopen(s, "r")) == NULL) {
		odprintf("Error: fopen: %s: %s",
		    fm, strerror(errno));
		goto done;
	}
	odprintf("var data = \"");
	odprintf("map_id=%s\\n", s);
	odprintf("player_one=%s\\nplayer_two=%s\\n", p1, p2);
	odprintf("playback_string=");
	int planets = 0;
	while (fgets(s, sizeof(s), fm)) {
		double x, y;
		int owner, ships, growth;
		if (s[0] != 'P' || s[1]!= ' ' ||
		    sscanf(s + 2, "%lf %lf %d %d %d", &x, &y,
		    &owner, &ships, &growth) != 5) {
			odprintf("Error: sscanf: %s: %s", fm, s);
			goto done;
		}
		if (planets++)
			odprintf(":");
		odprintf("%f,%f,%d,%d,%d", x, y, owner, ships,
		    growth);
	}
	fclose(fm);

	while (fgets(s, sizeof(s), f)) {
		s[strlen(s) - 1] = 0;
		if (!s[0])
			break;
		odprintf("%c%s", turns++ ? ':' : '|', s);
	}
	odprintf("\"\n");

	fclose(f);

	while (fgets(s, sizeof(s), fh) != NULL)
		odprintf("%s", s);
	fclose(fh);
done:
	if (gz != NULL)
		gzclose(gz);
	else
		fflush(stdout);
	return (0);
}
