#include <sys/types.h>
#include <errno.h>
#include <fts.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <set>
#include <stdlib.h>
#include <time.h>

using namespace std;

const char *infn = "results.pgn";
const char *outfn = "results-new.pgn";
const char *bckfn = "results-old.pgn";
const char *gamepath = "/var/www/games";

static set<unsigned> ids;

static void
unlinkGames()
{
	FTS *fts;
	FTSENT *e;
	char * const path_argv[] = { (char *)gamepath, NULL };
	unsigned id;

	fts = fts_open(path_argv, FTS_NOSTAT, 0);
	if (fts == NULL) {
		fprintf(stderr, "fts_open: %s: %s\n", gamepath, strerror(errno));
		return;
	}
	while ((e = fts_read(fts))) {
		if ((e->fts_info & FTS_F)) {
			id = atol(e->fts_name);
			if (id < 1) {
				fprintf(stderr, "fts_path: %s\n", e->fts_path);
				continue;
			}
			if (ids.find(id) != ids.end())
				continue;
			printf("deleting %s\n", e->fts_path);
			if (unlink(e->fts_path))
				fprintf(stderr, "unlink: %s: %s\n", e->fts_path,
				    strerror(errno));
		}
	}
	fts_close(fts);
}

/*
[White "DaTwinkDaddy-s8"]
[Black "genericbob.7"]
[Result "1-0"]
1284840281
33314
maps/map88.txt
turns: 6
TIMEOUT

*/

int main(int argc, char *argv[])
{
	int hours = 0;
	FILE *in, *out;
	char s[8192], a[256], b[256], c[256];
	int state = 0;
	unsigned t, t0, id;
	unsigned countin = 0, countout = 0;

	if (argc != 2 || (hours = atoi(argv[1])) < 1 || hours > 999) {
		fprintf(stderr, "usage: %s hours\n", argv[0]);
		return (1);
	}
	t0 = time(NULL) - hours * 60 * 60 + 120;
	printf("copying >= %u (%d hours ago)\n", t0, hours);
	if ((out = fopen(outfn, "w")) == NULL) {
		fprintf(stderr, "fopen: %s: %s\n", outfn, strerror(errno));
		return (1);
	}
	if ((in = fopen(infn, "r")) == NULL) {
		fprintf(stderr, "fopen: %s: %s\n", infn, strerror(errno));
		fclose(out);
		return (1);
	}
	a[0] = b[0] = c[0] = 0;
	while (fgets(s, sizeof(s), in) != NULL) {
		s[strlen(s) - 1] = 0;
		switch (state) {
		case 0:
			if (!strncmp(s, "[White \"", 8))
				strcpy(a, s);
			if (!strncmp(s, "[Black \"", 8))
				strcpy(b, s);
			if (!strncmp(s, "[Result \"", 9)) {
				strcpy(c, s);
				state++;
			}
			break;
		case 1:
			countin++;
			t = atol(s);
			if (t >= t0)
				state = 2;
			else
				state = 4;
			break;
		case 2:
			id = atol(s);
			ids.insert(id);
			fprintf(out, "%s\n", a);
			fprintf(out, "%s\n", b);
			fprintf(out, "%s\n", c);
			fprintf(out, "%u\n", t);
			fprintf(out, "%u\n", id);
			state++;
			countout++;
			break;
		case 3: /* copy entry, all subsequent lines */
			fprintf(out, "%s\n", s);
			/* fall-through */
		case 4: /* skip entry, all subsequent lines */
			if (!s[0]) {
				a[0] = b[0] = c[0] = 0;
				state = 0;
			}
			break;
		}
	}
	fclose(in);
	fclose(out);
	if (rename(infn, bckfn)) {
		fprintf(stderr, "rename: %s: %s: %s\n", infn, bckfn, strerror(errno));
		return (1);
	}
	if (rename(outfn, infn)) {
		fprintf(stderr, "rename: %s: %s: %s\n", outfn, infn, strerror(errno));
		return (1);
	}
	printf("%u of %u records copied\n", countout, countin);
	unlinkGames();
	return (0);
}
