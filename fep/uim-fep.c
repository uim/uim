/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifndef DEBUG
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
#include <uim/uim.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef HAVE_TERM_H
#include <term.h>
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
#ifdef HAVE_UTIL_H
#include <util.h>
#endif
#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#endif

#include "udsock.h"
#include "str.h"
#include "uim-fep.h"
#include "callbacks.h"
#include "draw.h"
#include "escseq.h"
#include "key.h"
#include "read.h"

/* global variables */
struct winsize *g_win;
#define DEFAULT_STATUS LASTLINE
#define DEFAULT_BACKTICK_WIDTH 70

static uim_context s_context;
/* ステータスラインの種類 */
static int s_status_type = DEFAULT_STATUS;
static int s_gnu_screen = FALSE;
/* 疑似端末のmasterのファイル記述子 */
static int s_master;
int g_win_in = STDIN_FILENO;
int g_win_out = STDOUT_FILENO;
/* 起動時の端末状態 */
static struct termios s_save_tios;
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
static char s_path_setmode[MAXPATHLEN];
static char s_path_getmode[MAXPATHLEN];
static int s_setmode_fd = -1;
static int s_timeout = 0;

static void init_agent(const char *engine);
static const char *get_default_im_name(void);
static int make_color_escseq(const char *instr, struct attribute_tag *attr);
static int colorname2n(const char *name);
#ifndef HAVE_FORKPTY
static pid_t forkpty(int *amaster, char *name, struct termios *termp, struct winsize *winp);
#endif
static void main_loop(void);
static void recover_loop(void);
static struct winsize *get_winsize(void);
static void set_signal_handler(void);
static void recover(int sig_no);
static void sigwinch_handler(int sig_no);
static void sigusr1_handler(int sig_no);
static void sigusr2_handler(int sig_no);
static void usage(void);
static void version(void);


/*
 * uimを初期化する
 * engine 変換エンジンの名前
 */
static void init_agent(const char *engine)
{
  int nr;
  int i;
  if (uim_init() == -1) {
    printf("uim_init error\n");
    exit(EXIT_FAILURE);
  }
  s_context = uim_create_context(NULL, get_enc(), NULL, engine, uim_iconv, commit_cb);
  nr = uim_get_nr_im(s_context);
  for (i = 0; i < nr; i++) {
    if (strcmp(engine, uim_get_im_name(s_context, i)) == 0) {
      break;
    }
  }
  if (i == nr) {
    printf("%s is not a input method\n\n", engine);
    usage();
    exit(1);
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
  return engine;
}

int main(int argc, char **argv)
{
  /* command will be execed on pty */
  const char **command = malloc(sizeof(const char *) * (argc + 1));
  const char *engine;
  char *sock_path = NULL; /* Socket for backtick */
  pid_t child;
  int use_civis = FALSE;
  int cursor_no_reverse = FALSE;
  int statusline_width = UNDEFINED;
  int on_the_spot = FALSE;
  char *env_buf;
  struct attribute_tag attr_uim = {
    FALSE,     /* underline */
    FALSE,     /* standout */
    FALSE,     /* bold */
    FALSE,     /* blink */
    FALSE,     /* foreground */
    FALSE      /* background */
  };

  tcflag_t save_iflag;
  int op;

  if (getenv("UIM_FEP_PID")) {
    puts("uim-fep is already running");
    return EXIT_FAILURE;
  }

  if ((command[0] = getenv("SHELL")) == NULL || *command[0] == '\0') {
    struct passwd *pw;
    if ((pw = getpwuid(getuid())) == NULL || *(command[0] = pw->pw_shell) == '\0') {
      command[0] = "/bin/sh";
    }
  }
  command[1] = NULL;

  engine = get_default_im_name();

  while ((op = getopt(argc, argv, "e:s:u:b:w:t:C:Sciovh")) != -1) {
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
          s_status_type = NONE;
        }
        else if (strncmp(optarg, "backtick", strlen(optarg)) == 0) {
          s_status_type = BACKTICK;
        }
        else if (strncmp(optarg, "lastline", strlen(optarg)) == 0) {
          s_status_type = LASTLINE;
        }
        else {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 'S':
        s_gnu_screen = TRUE;
        break;

      case 'u':
        engine = optarg;
        break;

      case 'c':
        cursor_no_reverse = TRUE;
        break;

      case 'i':
        use_civis = TRUE;
        break;

      case 'o':
        on_the_spot = TRUE;
        break;

      case 'b':
        sock_path = optarg;
        break;

      case 'w':
        statusline_width = atoi(optarg);
        if (statusline_width <= 0) {
          usage();
          return EXIT_FAILURE;
        }
        break;

      case 't':
        s_timeout = atof(optarg) * 1000000;
        if (s_timeout <= 0) {
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

  if (s_gnu_screen) {
    s_status_type = BACKTICK;
    s_master = PROC_FILENO;
    g_win_in = WIN_IN_FILENO;
    g_win_out = WIN_OUT_FILENO;
  }

  /* exit if stdin is redirected */
  if (!isatty(g_win_in)) {
    FILE *tty;
    if ((tty = fopen("/dev/tty", "w")) != NULL) {
      fprintf(tty, "stdin is not a terminal\n");
    }
    return EXIT_FAILURE;
  }

  init_str();

  tcgetattr(g_win_in, &s_save_tios);
  setupterm(NULL, g_win_out, NULL);

  if (getenv("TMP")) {
    struct passwd *pw = getpwuid(getuid());
    /* Generate get mode filepath */
    snprintf(s_path_getmode, sizeof(s_path_getmode), "%s/uim-fep-%s-%d-getmode", getenv("TMP"), pw->pw_name, getpid());
    /* Generate set mode filepath */
    snprintf(s_path_setmode, sizeof(s_path_setmode), "%s/uim-fep-%s-%d-setmode", getenv("TMP"), pw->pw_name, getpid());
  } else {
    struct passwd *pw = getpwuid(getuid());
    /* Generate get mode filepath */
    snprintf(s_path_getmode, sizeof(s_path_getmode), "/tmp/uim-fep-%s-%d-getmode", pw->pw_name, getpid());
    /* Generate set mode filepath */
    snprintf(s_path_setmode, sizeof(s_path_setmode), "/tmp/uim-fep-%s-%d-setmode", pw->pw_name, getpid());
  }

  env_buf = malloc(30);
  snprintf(env_buf, 30, "UIM_FEP_PID=%d", getpid());
  putenv(env_buf);

  if (fopen(s_path_getmode, "wt") != NULL) {
    unlink(s_path_getmode);
    env_buf = malloc(strlen("UIM_FEP_GETMODE=") + strlen(s_path_getmode) + 1);
    sprintf(env_buf, "UIM_FEP_GETMODE=%s", s_path_getmode);
    putenv(env_buf);
  } else {
    s_path_getmode[0] = '\0';
  }

  if (mkfifo(s_path_setmode, 0600) != -1) {
    unlink(s_path_setmode);
    env_buf = malloc(strlen("UIM_FEP_SETMODE=") + strlen(s_path_setmode) + 1);
    sprintf(env_buf, "UIM_FEP_SETMODE=%s", s_path_setmode);
    putenv(env_buf);
  } else {
    s_path_setmode[0] = '\0';
    s_setmode_fd = -1;
  }

  g_win = get_winsize();
  if (!s_gnu_screen) {
    save_iflag = s_save_tios.c_iflag;
    s_save_tios.c_iflag &= ~ISTRIP;
    child = forkpty(&s_master, NULL, &s_save_tios, g_win);
    s_save_tios.c_iflag = save_iflag;

    if (child < 0) {
      perror("fork");
      return EXIT_FAILURE;
    }
    if (child == 0) {
      /* 子プロセス */
      execvp(command[0], (char *const *)command);
      perror(command[0]);
      done(EXIT_FAILURE);
    }
  }

  free(command);

  if (s_status_type == BACKTICK && statusline_width > CANDSIZE / 2) {
    statusline_width = CANDSIZE / 2;
  }

  if (s_status_type == BACKTICK) {
    init_sendsocket(sock_path);
  }
  init_agent(engine);
  init_callbacks(s_context, s_status_type, cursor_no_reverse, statusline_width);
  init_draw(s_context, on_the_spot, s_status_type, s_gnu_screen, s_master, s_path_getmode);
  init_escseq(use_civis, on_the_spot, s_status_type, s_gnu_screen, &attr_uim);
  set_signal_handler();

  if (s_path_setmode[0] != '\0' && mkfifo(s_path_setmode, 0600) != -1) {
    s_setmode_fd = open(s_path_setmode, O_RDONLY | O_NONBLOCK);
  } else {
    s_path_setmode[0] = '\0';
    s_setmode_fd = -1;
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

#ifndef HAVE_FORKPTY
static pid_t forkpty(int *amaster, char *name, struct termios *termp, struct winsize *winp)
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
      tcsetattr(slave, TCSAFLUSH, termp);
    }
    if (winp != NULL) {
      ioctl(slave, TIOCSWINSZ, winp);
    }
    close(*amaster);
    dup2(slave, STDIN_FILENO);
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
  nfd++;

  while (TRUE) {
    /* コミットされたときにプリエディットがあるか */
    if (is_commit_and_preedit()) {
      struct timeval t;
      FD_ZERO(&fds);
      FD_SET(g_win_in, &fds);
      FD_SET(s_master, &fds);
      if (s_setmode_fd > 0) {
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
    if (s_setmode_fd > 0) {
      FD_SET(s_setmode_fd, &fds);
    }
    if (my_select(nfd, &fds, NULL) <= 0) {
      /* signalで割り込まれたときにくる。selectの返り値は-1でerrno==EINTR */
      continue;
    }


    /* モードを変更する */
    if (s_setmode_fd > 0 && FD_ISSET(s_setmode_fd, &fds)) {
      int start, end;
      if ((len = read(s_setmode_fd, buf, sizeof(buf) - 1)) <= 0) {
        debug2(("pipe closed\n"));
        close(s_setmode_fd);
        s_setmode_fd = open(s_path_setmode, O_RDONLY | O_NONBLOCK);
      }
      for (end = len - 1; end >= 0 && !isdigit(buf[end]); --end);
      /* プリエディットを編集中でなければモードを変更する */
      if (end >= 0 && !g_start_preedit) {
        int mode;
        for (start = end; start > 0 && isdigit(buf[start - 1]); --start);
        buf[end + 1] = '\0';
        mode = atoi(&buf[start]);
        if (mode != uim_get_current_mode(s_context)) {
          debug2(("mode change %d\n", mode));
          uim_set_mode(s_context, mode);
          callbacks_set_mode(uim_get_current_mode(s_context));
          draw_statusline_restore();
        }
      }
    }


    /* キーボード(stdin)からの入力 */
    if (FD_ISSET(g_win_in, &fds)) {
      int i;
      int key;
      int key_state = 0;
      int key_len;
      int raw;

      if ((len = read_stdin(buf, sizeof(buf) - 1)) <= 0) {
        /* ここにはこないと思う */
        return;
      }
      buf[len] = '\0';
      debug(("read \"%s\"\n", buf));

      if (len >= 10) {
        /* ペーストなどで大量に入力されたときは変換しない */
        if (!g_start_preedit) {
          write(s_master, buf, len);
        }
      } else {

        for (i = 0; i < len; i++) {
          key = tty2key(buf[i]);
          if (key == UKey_Escape) {
            int *key_and_key_len = escape_sequence2key(buf + i);
            key = key_and_key_len[0];
            if (key == UKey_Escape) {
              if (i + 1 < len) {
                /* Alt+キー */
                key_state = UMod_Alt;
                continue;
              } else if (s_timeout > 0) {
                struct timeval t;
                FD_ZERO(&fds);
                FD_SET(g_win_in, &fds);
                t.tv_sec = 0;
                t.tv_usec = s_timeout;
                if (my_select(nfd, &fds, &t) > 0) {
                  len += read_stdin(buf + len, sizeof(buf) - len - 1);
                  buf[len] = '\0';
                  debug(("read_again \"%s\"\n", buf));
                  i--;
                  continue;
                }
              }
            }
            key_len = key_and_key_len[1];
          } else {
            key_state += tty2key_state(buf[i]);
            key_len = 1;
          }
          raw = press_key(key, key_state);
          draw();
          if (raw && !g_start_preedit) {
            if (key_state & UMod_Alt) {
              write(s_master, buf + i - 1, key_len + 1);
            } else {
              write(s_master, buf + i, key_len);
            }
          }
          key_state = 0;
          i += (key_len - 1);
        }
      }
    }


    /* input from pty (child process) */
    if (FD_ISSET(s_master, &fds)) {
      if ((len = read(s_master, buf, sizeof(buf) - 1)) <= 0) {
        /* 子プロセスが終了した */
        return;
      }
      buf[len] = '\0';

      /* クリアされた時にモードを再描画する */
      if (s_status_type == LASTLINE) {
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
      if ((len = read(g_win_in, buf, sizeof(buf))) <= 0) {
        /* ここにはこないと思う */
        return;
      }
      write(s_master, buf, len);
    }
    if (FD_ISSET(s_master, &fds)) {
      if ((len = read(s_master, buf, sizeof(buf))) <= 0) {
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
  struct winsize *win = malloc(sizeof(struct winsize));
  ioctl(g_win_in, TIOCGWINSZ, win);
  if (s_status_type == LASTLINE) {
    win->ws_row--;
  }
  return win;
}

/*
 * シグナルハンドラを設定する
 */
static void set_signal_handler(void)
{
  struct sigaction act;
  /* シグナルをブロックしない */
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  /* システムコールを再実行する(実装依存) */
  /* act.sa_flags = SA_RESTART; */

  act.sa_handler = done;
  sigaction(SIGHUP, &act, NULL);
  sigaction(SIGTERM, &act, NULL);

  /* act.sa_handler = reload_uim; */
  sigaction(SIGQUIT, &act, NULL);

  act.sa_handler = recover;
  sigaction(SIGINT, &act, NULL);


  act.sa_handler = sigwinch_handler;
  sigaction(SIGWINCH, &act, NULL);

  act.sa_handler = sigusr1_handler;
  sigaction(SIGUSR1, &act, NULL);

  act.sa_handler = sigusr2_handler;
  sigaction(SIGUSR2, &act, NULL);
}

static void recover(int sig_no)
{
  put_exit_attribute_mode();
  put_exit_standout_mode();
  put_exit_underline_mode();
  put_restore_cursor();
  put_cursor_normal();
  recover_loop();
  done(EXIT_SUCCESS);
}

/*
 * 端末のサイズが変わったときに仮想端末の大きさを合わせる。
 */
static void sigwinch_handler(int sig_no)
{
  struct winsize *prev_win = g_win;
  g_win = get_winsize();
  debug2(("g_win->ws_row = %d g_win->ws_col = %d\n", g_win->ws_row, g_win->ws_col));
  escseq_winch();
  callbacks_winch();
  draw_winch();
  if (s_status_type == LASTLINE) {
    put_save_cursor();
    put_cursor_invisible();
    if (g_win->ws_row > prev_win->ws_row) {
      struct winsize save_win = *g_win;
      g_win->ws_row = prev_win->ws_row;
      clear_lastline();
      *g_win = save_win;
    }
    draw_statusline_force_no_restore();
    put_change_scroll_region(0, g_win->ws_row - 1);
    put_restore_cursor();
    put_cursor_normal();
  }
  free(prev_win);
  ioctl(s_master, TIOCSWINSZ, g_win);
}

void done(int exit_value)
{
  uim_quit();
  quit_escseq();
  if (s_status_type == BACKTICK) {
    clear_backtick();
  }
  tcsetattr(g_win_in, TCSAFLUSH, &s_save_tios);
  if (s_setmode_fd > 0) {
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

static void sigusr1_handler(int sig_no)
{
  press_key(UKey_Private1, 0);
  draw();
}

static void sigusr2_handler(int sig_no)
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
  context = uim_create_context(NULL, NULL, NULL, NULL, uim_iconv, commit_cb);

  printf("usage: uim-fep [OPTIONS]\n"
      "\n"
      "-u <input method>                         input method      [default=%s]\n"
      "-s <lastline/backtick/none>               statusline type   [default=%s]\n"
      "-b <file>                                 socket file       [default=%s]\n"
      "-w <width>                                statusline width  [default=%d]\n"
      "%s"
      "-e command arg1 arg2 ...                  executed command  [default=%s]\n"
      "%s",
      get_default_im_name(),
      DEFAULT_STATUS == LASTLINE ? "lastline" :
      DEFAULT_STATUS == BACKTICK ? "backtick" : "none",
      usersockname(NULL),
      DEFAULT_BACKTICK_WIDTH,
      "-t <sec>                                  key timeout\n"
      "-C [<foreground color>]:[<background color>]\n"
      "-c                                        reverse cursor\n"
      "-i                                        use cursor_invisible(civis)\n"
      "-o                                        on the spot\n"
      "-S                                        GNU screen mode\n",
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
  printf("uim-fep %s\n", PACKAGE_VERSION);
}

#if defined(DEBUG) && DEBUG > 1
void _debug(const char *fmt, ...)
{
  static FILE *log = NULL;
  va_list ap;
  if (log == NULL) {
    log = fopen("uim-fep-log", "w");
  }
  va_start(ap, fmt);
  vfprintf(log, fmt, ap);
  va_end(ap);
  fflush(log);
}
#endif
