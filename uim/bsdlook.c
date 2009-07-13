/*	$OpenBSD: look.c,v 1.11 2005/06/25 17:00:35 niallo Exp $	*/
/*	$NetBSD: look.c,v 1.7 1995/08/31 22:41:02 jtc Exp $	*/
/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <config.h>
#include "bsdlook.h"

#ifndef lint
#if 0
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n	The Regents of the University of California.  All rights reserved.\n";
#endif
#endif /* not lint */

#ifndef lint
#if 0
static char sccsid[] = "@(#)look.c	8.2 (Berkeley) 5/4/95";
static char rcsid[] = "$OpenBSD: look.c,v 1.11 2005/06/25 17:00:35 niallo Exp $";
#endif
#endif /* not lint */

/*
 * look -- find lines in a sorted list.
 * 
 * The man page said that TABs and SPACEs participate in -d comparisons.
 * In fact, they were ignored.  This implements historic practice, not
 * the manual page.
 */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * FOLD and DICT convert characters to a normal form for comparison,
 * according to the user specified flags.
 * 
 * DICT expects integers because it uses a non-character value to
 * indicate a character which should not participate in comparisons.
 */
#define	EQUAL		0
#define	GREATER		1
#define	LESS		(-1)
#define NO_COMPARE	(-2)

#define	FOLD(c)	(isascii(c) && isupper(c) ? tolower(c) : (c))
#define	DICT(c)	(isascii(c) && isalnum(c) ? (c) : NO_COMPARE)

#ifndef SIZE_T_MAX
#define SIZE_T_MAX	ULONG_MAX
#endif

struct uim_look_ctx {
	int fd;
	size_t len;
	char *front0, *back0;
	char *front, *back;
	int dflag, fflag;
	char *acc;
};

static char	*binary_search(char *, uim_look_ctx *);
static int	 compare(char *, char *, uim_look_ctx *);
static char	*linear_search(char *, uim_look_ctx *);

uim_look_ctx *
uim_look_init(void)
{
	uim_look_ctx *ctx = calloc(1, sizeof(*ctx));

	if (!ctx) {
		perror("uim_look_init");
		return NULL;
	}

	/* set -df by default. */
	ctx->dflag = ctx->fflag = 1;
	return ctx;
}

void
uim_look_set_option_dictionary_order(int dflag, uim_look_ctx *ctx)
{
	ctx->dflag = dflag;
}

void
uim_look_set_option_ignore_case(int fflag, uim_look_ctx *ctx)
{
	ctx->fflag = fflag;
}

void
uim_look_reset(uim_look_ctx *ctx)
{
	ctx->front = ctx->acc = ctx->front0;
	ctx->back = ctx->back0;
}

void
uim_look_set(uim_look_ctx *ctx)
{
	ctx->acc = ctx->front;
}

size_t
uim_look_get(char *string, char *dst, size_t len, uim_look_ctx *ctx)
{
	char *front = ctx->acc, *back = ctx->back;
	char *p = dst;
	size_t dst_len = 0;

	if (front < back && compare(string, front, ctx) == EQUAL) {
		for (; dst_len < len - 1 && front < back && *front != '\n'; ++front, ++dst_len) {
			*p++ = *front;
		}
		ctx->acc = front + 1;
		*p = '\0';
		return dst_len;
	}
	return dst_len;
}

void
uim_look_finish(uim_look_ctx *ctx)
{
	if (!ctx)
		return;

	if (ctx->front0 > 0 && munmap(ctx->front0, ctx->len) == -1)
		perror("uim_look_finish");

	if (ctx->fd > 0)
		close(ctx->fd);

	free(ctx);
	return;
}

int
uim_look_open_dict(const char *dict, uim_look_ctx *ctx)
{
	struct stat sb;

	if ((ctx->fd = open(dict, O_RDONLY, 0)) < 0 || fstat(ctx->fd, &sb)) {
		perror("uim_look_open_dict");
		return 0;
	}
	if ((size_t)sb.st_size > SIZE_T_MAX) {
		perror("uim_look_open_dict");
		return 0;
	}
	if ((ctx->front0 = ctx->front = mmap(NULL,
		    (size_t)sb.st_size, PROT_READ, MAP_PRIVATE, ctx->fd, (off_t)0)) == MAP_FAILED) {
		perror("uim_look_open_dict");
	}
	ctx->len = (size_t)sb.st_size;
	ctx->back0 = ctx->back = ctx->front + sb.st_size;

	return 1;
}

int
uim_look(char *string, uim_look_ctx *ctx)
{
	int ch;
	char *readp, *writep;
	int fflag = ctx->fflag, dflag = ctx->dflag;

	/* Reformat string to avoid doing it multiple times later. */
	for (readp = writep = string; (ch = *readp++) != '\0';) {
		if (fflag)
			ch = FOLD(ch);
		if (dflag)
			ch = DICT(ch);
		if (ch != NO_COMPARE)
			*(writep++) = (char)ch;
	}
	*writep = '\0';

	ctx->front = binary_search(string, ctx);
	ctx->front = linear_search(string, ctx);

	return (ctx->front ? 1 : 0);
}

#if 0
int
main(int argc, char *argv[])
{
	int ch, termchar;
	char *file, *string = NULL, *p;
	int dflag = 0, fflag = 0;
	int ret;
	uim_look_ctx *ctx;
	char buf[BUFSIZ];

	file = "/usr/share/dict/words";
	termchar = '\0';
	while ((ch = getopt(argc, argv, "dft:")) != -1)
		switch(ch) {
		case 'd':
			dflag = 1;
			break;
		case 'f':
			fflag = 1;
			break;
		case 't':
			termchar = *optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch (argc) {
	case 2:				/* Don't set -df for user. */
		string = *argv++;
		file = *argv;
		break;
	case 1:				/* But set -df by default. */
		dflag = fflag = 1;
		string = *argv;
		break;
	default:
		usage();
	}

	if (termchar != '\0' && (p = strchr(string, termchar)) != NULL)
		*++p = '\0';


	ctx = look_init();

	if (!ctx)
		exit(1);

	look_set_option_dictionary_order(dflag, ctx);
	look_set_option_ignore_case(fflag, ctx);

	if (termchar != '\0' && (p = strchr(string, termchar)) != NULL)
		*++p = '\0';

	if (!look_open_dict(file, ctx))
		exit(1);

	if ((ret = look(string, ctx)) != 0) {
		look_set(ctx);
		while (look_get(string, buf, sizeof(buf), ctx) != 0)
			printf("%s\n", buf);
	}

	look_finish(ctx);

	return ret;
}
#endif

/*
 * Binary search for "string" in memory between "front" and "back".
 * 
 * This routine is expected to return a pointer to the start of a line at
 * *or before* the first word matching "string".  Relaxing the constraint
 * this way simplifies the algorithm.
 * 
 * Invariants:
 * 	front points to the beginning of a line at or before the first 
 *	matching string.
 * 
 * 	back points to the beginning of a line at or after the first 
 *	matching line.
 * 
 * Base of the Invariants.
 * 	front = NULL; 
 *	back = EOF;
 * 
 * Advancing the Invariants:
 * 
 * 	p = first newline after halfway point from front to back.
 * 
 * 	If the string at "p" is not greater than the string to match, 
 *	p is the new front.  Otherwise it is the new back.
 * 
 * Termination:
 * 
 * 	The definition of the routine allows it return at any point, 
 *	since front is always at or before the line to print.
 * 
 * 	In fact, it returns when the chosen "p" equals "back".  This 
 *	implies that there exists a string is least half as long as 
 *	(back - front), which in turn implies that a linear search will 
 *	be no more expensive than the cost of simply printing a string or two.
 * 
 * 	Trying to continue with binary search at this point would be 
 *	more trouble than it's worth.
 */
#define	SKIP_PAST_NEWLINE(p, back) \
	while (p < back && *p++ != '\n');

static char *
binary_search(char *string, uim_look_ctx *ctx)
{
	char *p;
	char *front = ctx->front, *back = ctx->back;

	p = front + (back - front) / 2;
	SKIP_PAST_NEWLINE(p, back);

	/*
	 * If the file changes underneath us, make sure we don't
	 * infinitely loop.
	 */
	while (p < back && back > front) {
		if (compare(string, p, ctx) == GREATER)
			front = p;
		else
			back = p;
		p = front + (back - front) / 2;
		SKIP_PAST_NEWLINE(p, back);
	}
	return (front);
}

/*
 * Find the first line that starts with string, linearly searching from front
 * to back.
 * 
 * Return NULL for no such line.
 * 
 * This routine assumes:
 * 
 * 	o front points at the first character in a line. 
 *	o front is before or at the first line to be printed.
 */
static char *
linear_search(char *string, uim_look_ctx *ctx)
{
	char *front = ctx->front, *back = ctx->back;

	while (front < back) {
		switch (compare(string, front, ctx)) {
		case EQUAL:		/* Found it. */
			return (front);
			break;
		case LESS:		/* No such string. */
			return (NULL);
			break;
		case GREATER:		/* Keep going. */
			break;
		}
		SKIP_PAST_NEWLINE(front, back);
	}
	return (NULL);
}

#if 0
/*
 * Print as many lines as match string, starting at front.
 */
void
look_print_from(char *string, uim_look_ctx *ctx)
{
	char *front = ctx->front, *back = ctx->back;

	for (; front < back && compare(string, front, ctx) == EQUAL; ++front) {
		for (; front < back && *front != '\n'; ++front)
			if (putchar(*front) == EOF) {
				fprintf(stderr, "print_from: stdout");
				return;
			}
		if (putchar('\n') == EOF) {
			fprintf(stderr, "print_from: stdout");
			return;
		}
	}
}
#endif

/*
 * Return LESS, GREATER, or EQUAL depending on how the string1 compares with
 * string2 (s1 ??? s2).
 * 
 * 	o Matches up to len(s1) are EQUAL. 
 *	o Matches up to len(s2) are GREATER.
 * 
 * Compare understands about the -f and -d flags, and treats comparisons
 * appropriately.
 * 
 * The string "s1" is null terminated.  The string s2 is '\n' terminated (or
 * "back" terminated).
 */
static int
compare(char *s1, char *s2, uim_look_ctx *ctx)
{
	int ch;
	unsigned char *back = (unsigned char *)ctx->back;
	int fflag = ctx->fflag, dflag = ctx->dflag;

	for (; (unsigned char)*s1 && (unsigned char *)s2 < back && *s2 != '\n'; ++s1, ++s2) {
		ch = (unsigned char)*s2;
		if (fflag)
			ch = FOLD(ch);
		if (dflag)
			ch = DICT(ch);

		if (ch == NO_COMPARE) {
			++s2;		/* Ignore character in comparison. */
			continue;
		}
		if ((unsigned char)*s1 != ch)
			return ((unsigned char)*s1 < ch ? LESS : GREATER);
	}
	return ((unsigned char)*s1 ? GREATER : EQUAL);
}

#if 0
void
usage(void)
{
	(void)fprintf(stderr, "usage: look [-df] [-t char] string [file]\n");
	exit(2);
}
#endif

