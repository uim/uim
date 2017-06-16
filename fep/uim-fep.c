/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#include <stdio.h>
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
/* solarisでは term.hの前にcurses.hが必要 */
#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_STROPTS_H
#include <stropts.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_PTY_H
#include <pty.h>
#endif
#ifdef HAVE_UTMP_H
#include <utmp.h>
#endif
#ifdef HAVE_UTIL_H
#include <util.h>
#endif
#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#endif

#include <uim/uim.h>

#include "udsock.h"
#include "str.h"
#include "uim-fep.h"
#include "callbacks.h"
#include "draw.h"
#include "escseq.h"
#include "key.h"
#include "read.h"
#include "helper.h"

#define DEFAULT_STATUS LASTLINE

/* global variables */
struct opt_tag g_opt = {
  DEFAULT_STATUS, /* status_type       */
  FALSE,          /* ddskk             */
  FALSE,          /* cursor_no_reverse */
  FALSE,          /* use_civis         */
  FALSE,          /* on_the_spot       */
  UNDEFINED,      /* statusline_width  */
  0,              /* timeout           */
  FALSE,          /* no_report_cursor  */
  FALSE           /* print_key        */
};
int g_win_in = STDIN_FILENO;
int g_win_out = STDOUT_FILENO;
struct winsize *g_win;
uim_context g_context;

/* 疑似端末のmasterのファイル記述子 */
static int s_master;
/* 起動時の端末状態 */
static struct termios s_save_tios;

static char s_path_setmode[MAXPATHLEN];
static char s_path_getmode[MAXPATHLEN];
static int s_setmode_fd = -1;
#ifndef HAVE_SIG_ATOMIC_T
typedef int sig_atomic_t;
#endif
static volatile sig_atomic_t s_signal_flag;
static sigset_t s_orig_sigmask;

#define SIG_FLAG_DONE    1
#define SIG_FLAG_RECOVER (1 << 1)
#define SIG_FLAG_WINCH   (1 << 2)
#define SIG_FLAG_USR1    (1 << 3)
#define SIG_FLAG_USR2    (1 << 4)
#define SIG_FLAG_TSTP    (1 << 5)

static void init_uim(const char *engine);
static const char *get_default_im_name(void);
static int make_color_escseq(const char *instr, struct attribute_tag *attr);
static int colorname2n(const char *name);
static pid_t my_forkpty(int *amaster, struct termios *termp, struct winsize *winp);
static void main_loop(void);
static void recover_loop(void);
static struct winsize *get_winsize(void);
static void set_signal_handler(void);
static void reset_signal_handler(void);
static void signal_handler(int sig_no);
static void recover(void);
static void sigtstp_handler(void);
static void sigwinch_handler(void);
static void sigusr1_handler(void);
static void sigusr2_handler(void);
static void usage(void);
static void version(void);


/*
 * uimを初期化する
 * engine 変換エンジンの名前
 */
static void init_uim(const char *engine)
{
  int nr;
  int i;
  if (uim_init() == -1) {
    printf("uim_init error\n");
    exit(EXIT_FAILURE);
  }
  g_context = uim_create_context(NULL, get_enc(), NULL, engine, NULL, commit_cb);
  nr = uim_get_nr_im(g_context);
  for (i = 0; i < nr; i++) {
    if (strcmp(engine, uim_get_im_name(g_context, i)) == 0) {
      break;
    }
  }
  if (i == nr) {
    printf("%s is not an available input method\n\n", engine);
    usage();
    exit(EXIT_FAILURE);
  }
}

static const char *get_default_im_name(void)
{
  const char *engine;

  /* uim_get_default_im_name() requires initialized Scheme interpreter */
  if (uim_init() == -1) {
    printf("uim_init error\n");
    exit(EXIT_FAILURE);
  }

  engine = uim_get_default_im_name("");  /* instructs native locale */

  /* the string is only valid until next uim API call */
  return engine;
}

int main(int argc, char **argv)
{
  /* command will be execed on pty */
  const char **command = uim_malloc(sizeof(const char *) * (argc + 1));
  char *engine;
  char *sock_path = NULL; /* Socket for backtick */
  int gnu_screen = FALSE;
  char pid_str[30];
  struct attribute_tag attr_uim = {
    FALSE,     /* underline */
    FALSE,     /* standout */
    FALSE,     /* bold */
    FALSE,     /* blink */
    UNDEFINED, /* foreground */
    UNDEFINED  /* background */
  };
  FILE *fp;
  const char *suffix = NULL;
  char uim_dir[UNIX_PATH_MAX];
  const char *sty_str;
  const char *win_str;
  struct stat stat_buf;
  const char *errstr;

  int op;

  if ((command[0] = getenv("SHELL")) == NULL || *command[0] == '\0') {
    struct passwd *pw;
    if ((pw = getpwuid(getuid())) == NULL || *(command[0] = pw->pw_shell) == '\0') {
      command[0] = "/bin/sh";
    }
  }
  command[1] = NULL;

  init_str();
  engine = uim_strdup(get_default_im_name());

  while ((op = getopt(argc, argv, "e:s:u:b:w:t:C:f:SXciodKvh")) != -1) {
    int i;
    switch (op) {
      case 'e':
        command[0] = optarg;
        for (i = 1; i < 1 + argc - optind; i++) {
          command[i] = argv[optind - 1 + i];
        }
        command[i] = NULL;
        opterr = 0;
        getopt(argc = optind, argv, "");
        goto opt_end;

      case 's':
        if (strncmp(optarg, "none", strlen(optarg)) == 0) {
          g_opt.status_type = NONE;
        }
        else if (strncmp(optarg, "backtick", strlen(optarg)) == 0) {
          g_opt.status_type = BACKTICK;
        }
        else if (strncmp(optarg, "lastline", strlen(optarg)) == 0) {
          g_opt.status_type = LASTLINE;
        }
        else {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 'S':
        gnu_screen = TRUE;
        g_opt.no_report_cursor = TRUE;
        break;

      case 'X':
        gnu_screen = TRUE;
        break;

      case 'd':
        g_opt.ddskk = TRUE;
        break;

      case 'K':
        g_opt.print_key = TRUE;
        g_opt.status_type = NONE;
        g_opt.no_report_cursor = TRUE;
        break;

      case 'u':
        engine = uim_strdup(optarg);
        break;

      case 'c':
        g_opt.cursor_no_reverse = TRUE;
        break;

      case 'i':
        g_opt.use_civis = TRUE;
        break;

      case 'o':
        g_opt.on_the_spot = TRUE;
        break;

      case 'b':
        sock_path = optarg;
        break;

      case 'f':
        suffix = optarg;
        break;

      case 'w':
        g_opt.statusline_width = strtonum(optarg, 1, 10000, &errstr);
	if (errstr) {
	  printf("'%s' is %s", optarg, errstr);
	  return EXIT_FAILURE;
	}
        if (g_opt.statusline_width <= 0) {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 't':
        g_opt.timeout = atof(optarg) * 1000000;
        if (g_opt.timeout <= 0) {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 'C':
        if (make_color_escseq(optarg, &attr_uim) == EXIT_FAILURE) {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 'v':
        version();
        return EXIT_SUCCESS;

      case 'h':
        usage();
        return EXIT_SUCCESS;

      case '?':
        usage();
        return EXIT_FAILURE;
    }
  }
opt_end:

  if (optind != argc) {
    usage();
    return EXIT_FAILURE;
  }

  if (gnu_screen) {
    g_opt.status_type = BACKTICK;
    s_master = PROC_FILENO;
    g_win_in = WIN_IN_FILENO;
    g_win_out = WIN_OUT_FILENO;
  }

  if (g_opt.status_type == BACKTICK) {
    init_sendsocket(sock_path);
  }

  if (getenv("UIM_FEP_PID")) {
    if (gnu_screen) {
      sendline("uim-fep is already running");
    } else {
      puts("uim-fep is already running");
    }
    return EXIT_FAILURE;
  }

  if (attr_uim.foreground == UNDEFINED) {
    attr_uim.foreground = FALSE;
  }
  if (attr_uim.background == UNDEFINED) {
    attr_uim.background = FALSE;
  }

  if (!isatty(g_win_in)) {
    g_win_in = open("/dev/tty", O_RDONLY);
  }

  tcgetattr(g_win_in, &s_save_tios);
  setupterm(NULL, g_win_out, NULL);

  if (!get_ud_path(uim_dir, sizeof(uim_dir))) {
    sendline("uim-fep cannot make directory");
    /* return EXIT_FAILURE; */
  }

  sty_str = getenv("STY");
  win_str = getenv("WINDOW");
  if (gnu_screen) {
    if (!(sty_str != NULL && win_str != NULL)) {
      puts("STY and WINDOW are not defined");
      return EXIT_FAILURE;
    }
    snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s-%s", uim_dir, sty_str, win_str);
    snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s-%s", uim_dir, sty_str, win_str);
    if (stat(s_path_getmode, &stat_buf) == 0 || stat(s_path_setmode, &stat_buf) == 0) {
      char msg[100];
      snprintf(msg, sizeof(msg), "uim-fep is already running on window %s", win_str);
      sendline(msg);
      return EXIT_FAILURE;
    }
    if (suffix != NULL) {
      snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s", uim_dir, suffix);
      snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s", uim_dir, suffix);
    } else {
      snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s-%s-screen", uim_dir, sty_str, win_str);
      snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s-%s-screen", uim_dir, sty_str, win_str);
    }
  } else {
    if (sty_str != NULL && win_str != NULL) {
      snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s-%s-screen", uim_dir, sty_str, win_str);
      snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s-%s-screen", uim_dir, sty_str, win_str);
      if (stat(s_path_getmode, &stat_buf) == 0 || stat(s_path_setmode, &stat_buf) == 0) {
        printf("uim-fep is already running on window %s as filter\n", win_str);
        return EXIT_FAILURE;
      }
      if (suffix != NULL) {
        snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s", uim_dir, suffix);
        snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s", uim_dir, suffix);
      } else {
        snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s-%s", uim_dir, sty_str, win_str);
        snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s-%s", uim_dir, sty_str, win_str);
      }
    } else {
      if (suffix != NULL) {
        snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%s", uim_dir, suffix);
        snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%s", uim_dir, suffix);
      } else {
        int file_suffix = 1;
        int pid = getpid();

        snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%d", uim_dir, pid);
        snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%d", uim_dir, pid);
        while (stat(s_path_getmode, &stat_buf) == 0 || stat(s_path_setmode, &stat_buf) == 0) {
          snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/getmode-%d-%d", uim_dir, pid, file_suffix);
          snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/setmode-%d-%d", uim_dir, pid, file_suffix);
          file_suffix++;
        }
      }
    }
  }

  snprintf(pid_str, sizeof(pid_str), "%d", getpid());
  setenv("UIM_FEP_PID", pid_str, 1);

  if ((fp = fopen(s_path_getmode, "wt")) != NULL) {
    fclose(fp);
    unlink(s_path_getmode);
    setenv("UIM_FEP_GETMODE", s_path_getmode, 1);
  } else {
    s_path_getmode[0] = '\0';
  }

  unlink(s_path_setmode);
  if (mkfifo(s_path_setmode, 0600) != -1) {
    unlink(s_path_setmode);
    setenv("UIM_FEP_SETMODE", s_path_setmode, 1);
  } else {
    s_path_setmode[0] = '\0';
    s_setmode_fd = -1;
  }

  g_win = get_winsize();
  if (!gnu_screen && !g_opt.print_key) {
    pid_t child = my_forkpty(&s_master, &s_save_tios, g_win);

    if (child < 0) {
      perror("fork");
      return EXIT_FAILURE;
    }
    if (child == 0) {
      /* 子プロセス */
      if (g_win_in != STDIN_FILENO) {
        close(g_win_in);
      }
      execvp(command[0], (char *const *)command);
      perror(command[0]);
      done(EXIT_FAILURE);
    }
  }

  free(command);

  if (g_opt.status_type == BACKTICK && g_opt.statusline_width > CANDSIZE / 2) {
    g_opt.statusline_width = CANDSIZE / 2;
  }

  init_uim(engine);
  free(engine);
  if (gnu_screen) {
    uim_set_mode(g_context, 1);
  }
  init_helper();
  init_callbacks();
  focus_in();
  init_draw(s_master, s_path_getmode);
  init_escseq(&attr_uim);
  set_signal_handler();

  if (s_path_setmode[0] != '\0' && mkfifo(s_path_setmode, 0600) != -1) {
    s_setmode_fd = open(s_path_setmode, O_RDONLY | O_NONBLOCK);
#ifndef __CYGWIN32__
    open(s_path_setmode, O_WRONLY);
#endif
  } else {
    s_path_setmode[0] = '\0';
    s_setmode_fd = -1;
  }

  if (g_opt.print_key) {
    printf("Press any key.\r\n");
    printf("To exit the program, press 'q' key.\r\n");
  }
  main_loop();
  done(EXIT_SUCCESS);
  return EXIT_SUCCESS;
}

static int make_color_escseq(const char *instr, struct attribute_tag *attr)
{
  char *colon;
  if ((colon = strchr(instr, ':')) == NULL) {
    return EXIT_FAILURE;
  }
  if (instr < colon) {
    colon[0] = '\0';
    if ((attr->foreground = colorname2n(instr)) == UNDEFINED) {
      colon[0] = ':';
      return EXIT_FAILURE;
    }
    colon[0] = ':';
  }
  instr = colon + 1;
  if (instr[0] != '\0') {
    if ((attr->background = colorname2n(instr)) == UNDEFINED) {
      return EXIT_FAILURE;
    }
  }
  if (attr->foreground != UNDEFINED) {
    attr->bold = (attr->foreground & 8) == 8;
    attr->foreground = (attr->foreground & 7) + 30;
  } else {
    attr->foreground = FALSE;
  }
  if (attr->background != UNDEFINED) {
    attr->blink = (attr->background & 8) == 8;
    attr->background = (attr->background & 7) + 40;
  } else {
    attr->background = FALSE;
  }
  return EXIT_SUCCESS;
}

static int colorname2n(const char *name)
{
  if (strcasecmp(name, "black") == 0 || strcasecmp(name, "k") == 0) {
    return 0;
  }
  if (strcasecmp(name, "red") == 0 || strcasecmp(name, "r") == 0) {
    return 1;
  }
  if (strcasecmp(name, "green") == 0 || strcasecmp(name, "g") == 0) {
    return 2;
  }
  if (strcasecmp(name, "yellow") == 0 || strcasecmp(name, "y") == 0) {
    return 3;
  }
  if (strcasecmp(name, "blue") == 0 || strcasecmp(name, "b") == 0) {
    return 4;
  }
  if (strcasecmp(name, "magenta") == 0 || strcasecmp(name, "m") == 0) {
    return 5;
  }
  if (strcasecmp(name, "cyan") == 0 || strcasecmp(name, "c") == 0) {
    return 6;
  }
  if (strcasecmp(name, "white") == 0 || strcasecmp(name, "w") == 0) {
    return 7;
  }
  if (strcasecmp(name, "lightblack") == 0 || strcasecmp(name, "lk") == 0) {
    return 8;
  }
  if (strcasecmp(name, "lightred") == 0 || strcasecmp(name, "lr") == 0) {
    return 9;
  }
  if (strcasecmp(name, "lightgreen") == 0 || strcasecmp(name, "lg") == 0) {
    return 10;
  }
  if (strcasecmp(name, "lightyellow") == 0 || strcasecmp(name, "ly") == 0) {
    return 11;
  }
  if (strcasecmp(name, "lightblue") == 0 || strcasecmp(name, "lb") == 0) {
    return 12;
  }
  if (strcasecmp(name, "lightmagenta") == 0 || strcasecmp(name, "lm") == 0) {
    return 13;
  }
  if (strcasecmp(name, "lightcyan") == 0 || strcasecmp(name, "lc") == 0) {
    return 14;
  }
  if (strcasecmp(name, "lightwhite") == 0 || strcasecmp(name, "lw") == 0) {
    return 15;
  }
  return UNDEFINED;
}

#if defined(HAVE_FORKPTY)
static pid_t my_forkpty(int *amaster, struct termios *termp, struct winsize *winp)
{
  pid_t pid;
  int slave;

  tcflag_t save_iflag = termp->c_iflag;
  termp->c_iflag &= ~ISTRIP;

  if (openpty(amaster, &slave, NULL, termp, g_win) == -1) {
    perror("openpty");
    return -1;
  }

  termp->c_iflag = save_iflag;

  if ((pid = fork()) < 0) {
    return -2;
  }
  if (pid == 0) {
    /* 子プロセス */
    int redirected_stdin = dup(STDIN_FILENO);
    close(*amaster);
    login_tty(slave);
    if (g_win_in != STDIN_FILENO) {
      dup2(redirected_stdin, STDIN_FILENO);
    }
    close(redirected_stdin);
    return 0;
  } else {
    close(slave);
    return pid;
  }
}
#elif defined(__svr4__) || defined(__sgi__) || defined(__SVR4)
static pid_t my_forkpty(int *amaster, struct termios *termp, struct winsize *winp)
{
  pid_t pid;
  int slave;
  if ((*amaster = open("/dev/ptmx", O_RDWR)) < 0) {
    return -1;
  }
  if ((pid = fork()) < 0) {
    return -2;
  }
  if (pid == 0) {
    setsid();
    grantpt(*amaster);
    unlockpt(*amaster);
    if ((slave = open(ptsname(*amaster), O_RDWR)) < 0) {
      return -3;
    }
    if (ioctl(slave, I_PUSH, "ptem") < 0) {
      return -4;
    }
    if (ioctl(slave, I_PUSH, "ldterm") < 0) {
      return -5;
    }
    if (ioctl(slave, I_PUSH, "ttcompat") < 0) {
      return -6;
    }
    if (termp != NULL) {
      tcflag_t save_iflag = termp->c_iflag;
      termp->c_iflag &= ~ISTRIP;
      tcsetattr(slave, TCSAFLUSH, termp);
      termp->c_iflag = save_iflag;
    }
    if (winp != NULL) {
      ioctl(slave, TIOCSWINSZ, winp);
    }
    close(*amaster);
    if (g_win_in == STDIN_FILENO) {
      dup2(slave, STDIN_FILENO);
    }
    dup2(slave, STDOUT_FILENO);
    dup2(slave, STDERR_FILENO);
    close(slave);
    return 0;
  } else {
    return pid;
  }
}
#endif

#define BUFSIZE 4096
static void main_loop(void)
{
  char buf[BUFSIZE];
  ssize_t len;
  fd_set fds;
  int nfd;
  char *_clear_screen = cut_padding(clear_screen);
  char *_clr_eos = cut_padding(clr_eos);
  const char *errstr;

  if (g_win_in > s_master) {
    if (g_win_in > s_setmode_fd) {
      nfd = g_win_in;
    } else {
      nfd = s_setmode_fd;
    }
  } else {
    if (s_master > s_setmode_fd) {
      nfd = s_master;
    } else {
      nfd = s_setmode_fd;
    }
  }
  if (g_helper_fd > nfd) {
    nfd = g_helper_fd;
  }
  nfd++;

  while (TRUE) {
    /* コミットされたときにプリエディットがあるか */
    if (is_commit_and_preedit()) {
      struct timeval t;
      FD_ZERO(&fds);
      FD_SET(g_win_in, &fds);
      FD_SET(s_master, &fds);
      if (s_setmode_fd >= 0) {
        FD_SET(s_setmode_fd, &fds);
      }
      t.tv_sec = 0;
      /* 0.1秒 */
      t.tv_usec = 100000;
      if (my_select(nfd, &fds, &t) == 0) {
        /* タイムアウトした */
        draw_commit_and_preedit();
        debug2(("<end draw_commit_and_preedit>"));
      }
    }

    FD_ZERO(&fds);
    FD_SET(g_win_in, &fds);
    FD_SET(s_master, &fds);
    if (s_setmode_fd >= 0) {
      FD_SET(s_setmode_fd, &fds);
    }
    if (g_helper_fd >= 0) {
      FD_SET(g_helper_fd, &fds);
    }

    if (my_pselect(nfd, &fds, &s_orig_sigmask) <= 0) {
      /* signalで割り込まれたときにくる。selectの返り値は-1でerrno==EINTR */
      debug(("signal flag = %x\n", s_signal_flag));
      if ((s_signal_flag & SIG_FLAG_DONE   ) != 0) {
        s_signal_flag &= ~SIG_FLAG_DONE;
        done(1);
      }
      if ((s_signal_flag & SIG_FLAG_RECOVER) != 0) {
        s_signal_flag &= ~SIG_FLAG_RECOVER;
        recover();
      }
      if ((s_signal_flag & SIG_FLAG_WINCH  ) != 0) {
        s_signal_flag &= ~SIG_FLAG_WINCH;
        sigwinch_handler();
      }
      if ((s_signal_flag & SIG_FLAG_USR1   ) != 0) {
        s_signal_flag &= ~SIG_FLAG_USR1;
        sigusr1_handler();
      }
      if ((s_signal_flag & SIG_FLAG_USR2   ) != 0) {
        s_signal_flag &= ~SIG_FLAG_USR2;
        sigusr2_handler();
      }
      if ((s_signal_flag & SIG_FLAG_TSTP   ) != 0) {
        s_signal_flag &= ~SIG_FLAG_TSTP;
        sigtstp_handler();
      }
      continue;
    }

    /* モードを変更する */
    if (s_setmode_fd >= 0 && FD_ISSET(s_setmode_fd, &fds)) {
      int start, end;

#ifdef __CYGWIN32__
      if ((len = read(s_setmode_fd, buf, sizeof(buf) - 1)) == -1 || len == 0) {
        debug2(("pipe closed\n"));
        close(s_setmode_fd);
        s_setmode_fd = open(s_path_setmode, O_RDONLY | O_NONBLOCK);
      }
#else
      if ((len = read(s_setmode_fd, buf, sizeof(buf) - 1)) == -1 || len == 0) {
        /* XXX: fatal */
        debug2(("cannot read setmode file\n"));
        return;
      }
#endif

      for (end = len - 1; end >= 0 && !isdigit((unsigned char)buf[end]); --end);
      /* プリエディットを編集中でなければモードを変更する */
      if (end >= 0 && !g_start_preedit) {
        int mode;

        for (start = end; start > 0 && isdigit((unsigned char)buf[start - 1]); --start);
        buf[end + 1] = '\0';
        mode = strtonum(&buf[start], 0, uim_get_nr_modes(g_context) - 1, &errstr);
	if (errstr) {
	  debug2(("error: '%s' is %s\n", &buf[start], errstr));
	} else if (mode != uim_get_current_mode(g_context)) {
          debug2(("mode change %d\n", mode));
          uim_set_mode(g_context, mode);
          /* callbacks_set_mode(uim_get_current_mode(g_context)); */
          draw_statusline_restore();
        }
      }
    }


    /* キーボード(stdin)からの入力 */
    if (FD_ISSET(g_win_in, &fds)) {
      int key_state = 0;
      if (!g_focus_in) {
        focus_in();
      }

      if ((len = read_stdin(buf, sizeof(buf) - 1)) == -1 || len == 0) {
        /* ここにはこないと思う */
        return;
      }
      buf[len] = '\0';
      debug(("read \"%s\"\n", buf));

      if (len >= 10 && !g_opt.print_key) {
        /* ペーストなどで大量に入力されたときは変換しない */
        if (!g_start_preedit) {
          write(s_master, buf, len);
        }
      } else {

        int i;
        for (i = 0; i < len; i++) {
          int key_len;
          int *key_and_key_len = escape_sequence2key(buf + i, len - i);
          int key = key_and_key_len[0];

          if (key == UKey_Other) {

            int not_enough;
            key = tty2key(buf[i]);
            key_state |= tty2key_state(buf[i]);
            not_enough = key_and_key_len[1];

            if (buf[i] == ESCAPE_CODE && i == len - 1) {
              not_enough = TRUE;
            }

            if (not_enough && g_opt.timeout > 0) {
              /* 入力が足らないので再び読み出す */
              struct timeval t;
              fd_set fds;
              FD_ZERO(&fds);
              FD_SET(g_win_in, &fds);
              t.tv_sec = 0;
              t.tv_usec = g_opt.timeout;
              if (my_select(g_win_in + 1, &fds, &t) > 0) {
                ssize_t nr;

                if ((nr = read_stdin(buf + len, sizeof(buf) - len - 1)) != -1) {
                  len += nr;
                  buf[len] = '\0';
                  debug(("read_again \"%s\"\n", buf));
                  i--;
                  continue;
                } else {
                  /* XXX: fatal */
                  return;
                }
              }
            }

            if (buf[i] == ESCAPE_CODE && i < len - 1) {
              key_state = UMod_Alt;
              continue;
            }
            key_len = 1;

          } else { /* key != UKey_Other */
            key_len = key_and_key_len[1];
          }

          if (g_opt.print_key) {
            print_key(key, key_state);
          } else {
            int raw = press_key(key, key_state);
            if (!draw()) {
              if (g_opt.status_type == BACKTICK) {
                update_backtick();
              }
            }
            if (raw && !g_start_preedit) {
              if (key_state & UMod_Alt) {
                write(s_master, buf + i - 1, key_len + 1);
              } else {
                write(s_master, buf + i, key_len);
              }
            }
          }

          key_state = 0;
          i += (key_len - 1);
        }
      }
    }


    /* input from pty (child process) */
    if (!g_opt.print_key && FD_ISSET(s_master, &fds)) {
      if ((len = read(s_master, buf, sizeof(buf) - 1)) == -1 || len == 0) {
        /* 子プロセスが終了した */
        return;
      }
      buf[len] = '\0';

      /* クリアされた時にモードを再描画する */
      if (g_opt.status_type == LASTLINE) {
        char *str1 = rstrstr_len(buf, _clear_screen, len);
        char *str2 = rstrstr_len(buf, _clr_eos, len);
        if (str1 != NULL || str2 != NULL) {
          int str1_len;
          if (str2 > str1) {
            str1 = str2;
          }
          str1_len = len - (str1 - buf);
          /* str1はclear_screenかclr_eosの次の文字列を指している */
          put_pty_str(buf, len - str1_len);
          draw_statusline_force_restore();
          put_pty_str(str1, str1_len);
        } else {
          put_pty_str(buf, len);
        }
      } else {
        put_pty_str(buf, len);
      }
    }

    if (g_helper_fd >= 0 && FD_ISSET(g_helper_fd, &fds)) {
      helper_handler();
      draw();
    }
  }
}

/*
 * 何もしないフィルタ
 */
static void recover_loop(void)
{
  char buf[BUFSIZE];
  ssize_t len;
  fd_set fds;

  while (TRUE) {
    FD_ZERO(&fds);
    FD_SET(g_win_in, &fds);
    FD_SET(s_master, &fds);
    if (select(s_master + 1, &fds, NULL, NULL, NULL) <= 0) {
      /* signalで割り込まれたときにくる。selectの返り値は-1でerrno==EINTR */
      continue;
    }
    if (FD_ISSET(g_win_in, &fds)) {
      if ((len = read(g_win_in, buf, sizeof(buf))) == -1 || len == 0) {
        /* ここにはこないと思う */
        return;
      }
      write(s_master, buf, len);
    }
    if (FD_ISSET(s_master, &fds)) {
      if ((len = read(s_master, buf, sizeof(buf))) == -1 || len == 0) {
        /* 子プロセスが終了した */
        return;
      }
      write(g_win_out, buf, len);
    }
  }
}

/*
 * 現在のウィンドウサイズを返す
 * 返り値はfreeする
 */
static struct winsize *get_winsize(void)
{
  struct winsize *win = uim_malloc(sizeof(struct winsize));
  ioctl(g_win_in, TIOCGWINSZ, win);
  if (g_opt.status_type == LASTLINE) {
    win->ws_row--;
  }
#ifdef PTY_TEST
  win->ws_col = 140;
  win->ws_row = 50;
#endif
  return win;
}

/*
 * シグナルハンドラを設定する
 */
static void set_signal_handler(void)
{
  struct sigaction act;
  sigset_t sigmask;

  sigemptyset(&sigmask);
  sigaddset(&sigmask, SIGHUP);
  sigaddset(&sigmask, SIGTERM);
  sigaddset(&sigmask, SIGQUIT);
  sigaddset(&sigmask, SIGINT);
  sigaddset(&sigmask, SIGWINCH);
  sigaddset(&sigmask, SIGUSR1);
  sigaddset(&sigmask, SIGUSR2);
  sigaddset(&sigmask, SIGTSTP);
  sigaddset(&sigmask, SIGCONT);
  sigprocmask(SIG_BLOCK, &sigmask, &s_orig_sigmask);

  /* シグナルをブロックしない */
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  /* システムコールを再実行する(実装依存) */
  /* act.sa_flags = SA_RESTART; */

  act.sa_handler = signal_handler;
  sigaction(SIGHUP, &act, NULL);
  sigaction(SIGTERM, &act, NULL);
  sigaction(SIGQUIT, &act, NULL);
  sigaction(SIGINT, &act, NULL);
  sigaction(SIGWINCH, &act, NULL);
  sigaction(SIGUSR1, &act, NULL);
  sigaction(SIGUSR2, &act, NULL);
  sigaction(SIGTSTP, &act, NULL);
  sigaction(SIGCONT, &act, NULL);
}

static void reset_signal_handler(void)
{
  struct sigaction act;

  sigprocmask(SIG_SETMASK, &s_orig_sigmask, NULL);

  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_handler = SIG_DFL;
  sigaction(SIGHUP, &act, NULL);
  sigaction(SIGTERM, &act, NULL);
  sigaction(SIGQUIT, &act, NULL);
  sigaction(SIGINT, &act, NULL);
  sigaction(SIGWINCH, &act, NULL);
  sigaction(SIGUSR1, &act, NULL);
  sigaction(SIGUSR2, &act, NULL);
  sigaction(SIGTSTP, &act, NULL);
  sigaction(SIGCONT, &act, NULL);
}

static void signal_handler(int sig_no)
{
  switch (sig_no) {
    case SIGHUP:
    case SIGTERM:
    case SIGQUIT:
      s_signal_flag |= SIG_FLAG_DONE;
      break;
    case SIGINT:
      s_signal_flag |= SIG_FLAG_RECOVER;
      break;
    case SIGWINCH:
      s_signal_flag |= SIG_FLAG_WINCH;
      break;
    case SIGUSR1:
      s_signal_flag |= SIG_FLAG_USR1;
      break;
    case SIGUSR2:
      s_signal_flag |= SIG_FLAG_USR2;
      break;
    case SIGTSTP:
      s_signal_flag |= SIG_FLAG_TSTP;
      break;
  }
}

static void recover(void)
{
  reset_signal_handler();
  put_exit_attribute_mode();
  put_restore_cursor();
  put_cursor_normal();
  recover_loop();
  done(EXIT_SUCCESS);
}

/*
 * 休止シグナル
 */
static void sigtstp_handler(void)
{
  struct sigaction act;
  sigset_t sigmask;

  quit_escseq();
  put_save_cursor();
  tcsetattr(g_win_in, TCSAFLUSH, &s_save_tios);

  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_handler = SIG_DFL;
  sigaction(SIGTSTP, &act, NULL);
  sigaction(SIGCONT, &act, NULL);

  sigemptyset(&sigmask);
  sigaddset(&sigmask, SIGTSTP);
  sigaddset(&sigmask, SIGCONT);
  sigprocmask(SIG_UNBLOCK, &sigmask, NULL);

  kill(getpid(), SIGTSTP);

  sigprocmask(SIG_BLOCK, &sigmask, NULL);
  
  act.sa_handler = signal_handler;
  sigaction(SIGTSTP, &act, NULL);
  sigaction(SIGCONT, &act, NULL);
  fixtty();
}

/*
 * 端末のサイズが変わったときに仮想端末の大きさを合わせる。
 */
static void sigwinch_handler(void)
{
  struct winsize *prev_win = g_win;
  g_win = get_winsize();

  debug2(("g_win->ws_row = %d g_win->ws_col = %d\n", g_win->ws_row, g_win->ws_col));

  escseq_winch();
  callbacks_winch();
  draw_winch(prev_win);

  free(prev_win);
  ioctl(s_master, TIOCSWINSZ, g_win);
}

void done(int exit_value)
{
  uim_quit();
  quit_escseq();
  quit_helper();
  if (g_opt.status_type == BACKTICK) {
    clear_backtick();
  }
  tcsetattr(g_win_in, TCSAFLUSH, &s_save_tios);
  if (s_setmode_fd >= 0) {
    close(s_setmode_fd);
  }
  if (s_path_setmode[0] != '\0') {
    unlink(s_path_setmode);
  }
  if (s_path_getmode[0] != '\0') {
    unlink(s_path_getmode);
  }
  exit(exit_value);
}

static void sigusr1_handler(void)
{
  press_key(UKey_Private1, 0);
  draw();
}

static void sigusr2_handler(void)
{
  press_key(UKey_Private2, 0);
  draw();
}

/*
 * helpを表示する。
 */
static void usage(void)
{

  uim_context context;
  int nr;
  int i;
  int max_im_name_len = 0;

  uim_init();
  context = uim_create_context(NULL, get_enc(), NULL, NULL, NULL, commit_cb);

  printf("uim-fep version %s\n", PACKAGE_VERSION);
  printf("Usage: uim-fep [OPTIONS]\n");
  printf("  or   uim-fep [OPTIONS] -e command arg1 arg2 ...\n");
  printf("  or   uim-fep [OPTIONS] -S\n");
  printf("  or   uim-fep [-t <sec>] -K\n");
  printf("\n");
  printf("Options\n"
      "-u <input method>                         input method      [default=%s]\n"
      "-s <lastline/backtick/none>               statusline type   [default=%s]\n"
      "-b <file>                                 socket file       [default=%s]\n"
      "-w <width>                                statusline width\n"
      "%s"
      "%s"
      "-e command arg1 arg2 ...                  executed command  [default=%s]\n"
      "%s",
      get_default_im_name(),
      DEFAULT_STATUS == LASTLINE ? "lastline" :
      DEFAULT_STATUS == BACKTICK ? "backtick" : "none",
      usersockname(NULL),
      "-t <sec>                                  key timeout\n"
      "-C [<foreground color>]:[<background color>]\n"
      "-c                                        reverse cursor\n"
      "-i                                        use cursor_invisible(civis)\n"
      "-o                                        on the spot\n"
      "-d                                        ddskk like candidate style\n",
      "-f                                        file name suffix of $UIM_FEP_SETMODE and $UIM_FEP_GETMODE\n"
      "-S                                        GNU screen mode\n"
      "-K                                        show key code\n",
      getenv("SHELL") != NULL ? getenv("SHELL") : "/bin/sh",
      "-h                                        display this help\n"
      "-v                                        display version\n"
      );

  printf("\n[input method]\n");
  nr = uim_get_nr_im(context);
  for (i = 0; i < nr; i++) {
    int im_name_len = strlen(uim_get_im_name(context, i));
    im_name_len += strlen(uim_get_im_language(context, i));
    im_name_len += strlen(" ()");
#ifdef DEBUG
    im_name_len += strlen(" ");
    im_name_len += strlen(uim_get_im_encoding(context, i));
#endif
    if (im_name_len > max_im_name_len) {
      max_im_name_len = im_name_len;
    }
  }

  for (i = 0; i < nr; i++) {
    int j;
    const char *im_name = uim_get_im_name(context, i);
    int im_name_len = strlen(im_name);
    printf("%s", im_name);

    im_name = uim_get_im_language(context, i);
    im_name_len += strlen(im_name);
    printf(" (%s)", im_name);
    im_name_len += strlen(" ()");
#ifdef DEBUG
    im_name = uim_get_im_encoding(context, i);
    im_name_len += strlen(im_name);
    printf(" %s", im_name);
    im_name_len += strlen(" ");
#endif
    for (j = 0; j < max_im_name_len - im_name_len + 2; j++) {
      putchar(' ');
    }
    printf("%s", uim_get_im_short_desc(context, i));
    printf("\n");
  }

  uim_quit();
}

/*
 * versionを表示する
 */
static void version(void)
{
  printf("uim-fep version %s\n", PACKAGE_VERSION);
}

#if defined(DEBUG) && DEBUG > 1
static FILE *_log = NULL;
void _debug(const char *fmt, ...)
{
  va_list ap;
  if (_log == NULL) {
    _log = fopen("uim-fep-log", "w");
  }
  va_start(ap, fmt);
  vfprintf(_log, fmt, ap);
  va_end(ap);
  fflush(_log);
}

void _debug_write(const char *str, int len)
{
  if (_log == NULL) {
    _log = fopen("uim-fep-log", "w");
  }
  fwrite(str, 1, len, _log);
  fflush(_log);
}
#endif
