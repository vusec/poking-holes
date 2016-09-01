/* Keep allocating memory and deduce how the allocator behaves */

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>

#include <readline/readline.h>
#include <readline/history.h>

enum {
	MAX_ARGV = 12,
	MSG_BUF_SIZE = 1024,
};

struct linear_region {
	void *base;
	size_t stride;
};

enum result_tag {
	RT_SUCCESS,
	RT_FAIL,
	RT_QUIT,
};

struct result {
	enum result_tag tag;
	/*
	 * Not be freed when done, just set to NULL. So
	 * no heap pointers here please
	 */
	const char *msg;
	bool truncated;
};

static char message_buffer[MSG_BUF_SIZE];

static void reset_message_buffer(void)
{
	message_buffer[0] = '\0';
}

static void res_reset(struct result *res)
{
	res->tag = RT_FAIL;
	res->truncated = false;
	res->msg = "Internal error: message not set";
}

static void fail(struct result *res, const char *fmt, ...)
{
	va_list ap;
	size_t ret;

	va_start(ap, fmt);
	/* Should be escaping newlines here too... */
	ret = vsnprintf(message_buffer, sizeof(message_buffer) - 1, fmt, ap);
	va_end(ap);
	message_buffer[sizeof(message_buffer) - 1] = '\0';
	if (ret >= (sizeof(message_buffer) - 1)) {
		res->truncated = true;
	}
	res->tag = RT_FAIL;
	res->msg = &message_buffer[0];
}

static void success(struct result *res, const char *fmt, ...)
{
	va_list ap;
	size_t ret;

	va_start(ap, fmt);
	ret = vsnprintf(message_buffer, sizeof(message_buffer) - 1, fmt, ap);
	va_end(ap);
	message_buffer[sizeof(message_buffer) - 1] = '\0';
	if (ret >= (sizeof(message_buffer) - 1)) {
		res->truncated = true;
	}
	res->tag = RT_SUCCESS;
	res->msg = &message_buffer[0];
}

struct command {
	const char *name;
	void (*func)(struct result *, int, char **);
};

static int
parse_ull(struct result *res, const char *str, unsigned long long *ret)
{
	unsigned long long num;
	char *eptr = NULL;

	num = strtoull(str, &eptr, 0);
	if (*eptr != '\0') {
		unsigned long long mult = 1;
		if (*eptr == 'K') {
			mult = 1024;
		} else if (*eptr == 'M') {
			mult = 1024 * 1024;
		} else if (*eptr == 'G') {
			mult = 1024 * 1024 * 1024;
		} else {
			fail(res, "Could not parse %s (remaining: %s)", str, eptr);
			return !0;
		}
		mult *= num;
		if (mult < num) {
			fail(res, "Overflow on %s", str);
			return !0;
		}
		*ret = mult;
		return 0;
	} else if (ULLONG_MAX == num) {
		fail(res, "Overflow on %s", str);
		return !0;
	} else {
		*ret = num;
		return 0;
	}
}

static void
cmd_alloc(struct result *res, int argc, char **argv)
{
	unsigned long long nbytes;

	if (argc != 2) {
		fail(res, "Need exactly one argument");
		return;
	}
	if (parse_ull(res, argv[1], &nbytes)) {
		return;
	}
	void *ptr = mmap(NULL, nbytes, PROT_READ|PROT_WRITE,
			 MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);

	if (MAP_FAILED == ptr) {
		fail(res, strerror(errno));
	} else {
		success(res, "%#Lx %ju", ptr, nbytes);
	}
}

static void
cmd_talloc(struct result *res, int argc, char **argv)
{
	unsigned long long nbytes;

	if (argc != 2) {
		fail(res, "Need exactly one argument");
		return;
	}
	if (parse_ull(res, argv[1], &nbytes)) {
		return;
	}
	void *ptr = mmap(NULL, nbytes, PROT_READ|PROT_WRITE,
			 MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);

	if (MAP_FAILED == ptr) {
		fail(res, strerror(errno));
	} else {
                if (munmap(ptr, nbytes)) {
                        fail(res, "munmap failed: %s", strerror(errno));
                } else {
                        success(res, "%#Lx %ju", ptr, nbytes);
                }
	}
}

static void
cmd_mmap_fixed(struct result *res, int argc, char **argv)
{
	unsigned long long addr, nbytes;
	char *eptr = NULL;

	if (argc != 3) {
		fail(res, "Need exactly two arguments");
		return;
	}
	if (parse_ull(res, argv[1], &addr)) {
		return;
	}

	if (parse_ull(res, argv[2], &nbytes)) {
		return;
	}

	assert(sizeof(void *) == sizeof(addr));

	void *ptr = mmap((void *)addr, nbytes,
				 PROT_READ,
				 MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE|MAP_FIXED, -1, 0);

	if (MAP_FAILED == ptr) {
		fail(res, strerror(errno));
	} else {
		success(res, "%#Lx %ju", ptr, nbytes);
	}
}

static void
cmd_brkalloc(struct result *res, int argc, char **argv)
{
	unsigned long long nbytes;

	if (argc != 2) {
		fail(res, "Need exactly one argument");
		return;
	}
	if (parse_ull(res, argv[1], &nbytes)) {
		return;
	}
	void *ptr = sbrk(nbytes);

	if (MAP_FAILED == ptr) {
		fail(res, strerror(errno));
	} else {
		success(res, "%#Lx %ju", ptr, nbytes);
	}
}

static void
cmd_munmap(struct result *res, int argc, char **argv)
{
        unsigned long long addr, nbytes;

        if (argc != 3) {
		fail(res, "Need exactly two arguments");
		return;
	}
        if (parse_ull(res, argv[1], &addr)) {
                return;
        }
        if (parse_ull(res, argv[2], &nbytes)) {
                return;
        }

        assert(sizeof(void *) == sizeof(addr));

        if (munmap((void *)addr, nbytes)) {
                fail(res, strerror(errno));
        } else {
                success(res, "Unmapped");
        }
}

static void
cmd_maps(struct result *res, int argc, char **argv)
{
	if (argc != 1) {
		fail(res, "Stray argument: %s", argv[1]);
	} else {
		char buf[1024];
		int ret;

		/* XXX: should be getting the path from our own path, not CWD */
		ret = snprintf(buf, sizeof(buf) - 1, "./maps /proc/%ld/maps", (long)getpid());
		if (ret >= (int)(sizeof(buf) - 1)) {
			fail(res, "Truncated command string");
			return;
		}
		buf[sizeof(buf) - 1] = '\0';	/* paranoia */
		system(buf);
		success(res, "-- Done --");
	}
}

static void
cmd_pid(struct result *res, int argc, char **argv)
{
	if (argc != 1) {
		fail(res, "Stray argument: %s", argv[1]);
	} else {
		success(res, "%ld", (long)getpid());
	}
}

static void
cmd_quit(struct result *res, int argc, char **argv)
{
	if (argc != 1) {
		fail(res, "Stray argument: %s", argv[1]);
	} else {
		success(res, "Quitting");
		res->tag = RT_QUIT;	/* XXX: ugh */
	}
}

struct command commands[] = {
	{
		"alloc",
		&cmd_alloc,
	},
	{
		"talloc",
		&cmd_talloc,
	},
	{
		"mmap_fixed",
		&cmd_mmap_fixed,
	},
	{
		"brkalloc",
		&cmd_brkalloc,
	},
        {
                "munmap",
                &cmd_munmap,
        },
	{
		"maps",
		&cmd_maps,
	},
	{
		"pid",
		&cmd_pid,
	},
	{
		"q",
		&cmd_quit,
	},
	{
		"quit",
		&cmd_quit,
	},
	{
		NULL,
		NULL,
	},
};

static const struct command *
lookup_cmd(const char *str)
{
	const struct command *cmd;

	for (cmd = &commands[0]; cmd->name; ++cmd) {
		if (!strcmp(cmd->name, str)) {
			return cmd;
		}
	}
	return NULL;
}

static void
do_cmd(int argc, char **argv)
{
	const struct command *cmd = NULL;
	struct result res;
	const char *trunc;

	res_reset(&res);
	reset_message_buffer();

	if (!(cmd = lookup_cmd(argv[0]))) {
		fail(&res, "No such command: %s", argv[0]);
	} else {
		cmd->func(&res, argc, argv);
	}

	trunc = res.truncated ? "|TRUNC" : "";
	switch(res.tag) {
	case RT_SUCCESS:
	case RT_QUIT:
		printf("OK%s\n%s\n", trunc, res.msg);
		break;
	case RT_FAIL:
		printf("ERR%s\n%s\n", trunc, res.msg);
		break;
	}
	if (res.tag == RT_QUIT) {
		exit(0);
	}
}

static void
do_line(char *line)
{
	char *save = NULL;
	const char *delim = " \t";
	char *tok = NULL;
	char *argv[MAX_ARGV];
	unsigned next_arg = 0;

	bzero(argv, sizeof(argv));

	while ((tok = strtok_r(line, delim, &save)) != NULL) {
		if (next_arg > (sizeof(argv) / sizeof(argv[0]))) {
			fprintf(stderr, "Too many arguments (> %zd)\n",
				sizeof(argv) / sizeof(argv[0]));
			exit(2);
		}
		argv[next_arg++] = tok;
		line = NULL;
	}
	do_cmd(next_arg, argv);
}

static void
do_readline(void)
{
	char *line;

	while ((line = readline("% ")) != NULL) {
		add_history(line);
		do_line(line);
		free(line);
	}
}

static void
do_other(void)
{
	char line[1024];
	char *nl;

	while (fgets(line, sizeof(line) - 1, stdin) != NULL) {
		line[sizeof(line) - 1] = '\0';
		nl = strchr(line, '\n');

		if (!nl) {
			fprintf(stderr, "String does not end in newline: %s", line);
			exit(2);
		} else {
			*nl = '\0';
		}
		do_line(line);
		line[0] = '\0';
	}
}

int
main(void)
{
	if (isatty(STDIN_FILENO)) {
		do_readline();
	} else {
		setvbuf(stdout, NULL, _IOLBF, 0);
		do_other();
	}

	return 0;
}
