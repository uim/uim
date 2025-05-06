/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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
#  include <config.h>
#endif
#include <gtk/gtk.h>

#include <locale.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/gettext.h>

#define BUTTON_H_ALIGN 5

/* written in unicode */
gchar *alphabet_capital[] = {
  "Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ",
  "Ｆ", "Ｇ", "Ｈ", "Ｉ", "Ｊ",
  "Ｋ", "Ｌ", "Ｍ", "Ｎ", "Ｏ",
  "Ｐ", "Ｑ", "Ｒ", "Ｓ", "Ｔ",
  "Ｕ", "Ｖ", "Ｗ", "Ｘ", "Ｙ",
  "Ｚ", NULL
};

gchar *alphabet_small[] = {
  "ａ", "ｂ", "ｃ", "ｄ", "ｅ",
  "ｆ", "ｇ", "ｈ", "ｉ", "ｊ",
  "ｋ", "ｌ", "ｍ", "ｎ", "ｏ",
  "ｐ", "ｑ", "ｒ", "ｓ", "ｔ",
  "ｕ", "ｖ", "ｗ", "ｘ", "ｙ",
  "ｚ", NULL
};

gchar *numbers[] = {
  "０", "１", "２", "３", "４",
  "５", "６", "７", "８", "９",
  NULL
};

gchar *symbols[] = {
  "−", "，", "．", "！", "？",
  NULL
};

gchar *katakana[] = {
  "ア", "イ", "ウ", "エ", "オ",
  "カ", "キ", "ク", "ケ", "コ",
  "サ", "シ", "ス", "セ", "ソ",
  "タ", "チ", "ツ", "テ", "ト",
  "ナ", "ニ", "ヌ", "ネ", "ノ",
  "ハ", "ヒ", "フ", "ヘ", "ホ",
  "マ", "ミ", "ム", "メ", "モ",
  "ヤ", "",   "ユ", "",   "ヨ",
  "ラ", "リ", "ル", "レ", "ロ",
  "ワ", "ヰ", "",   "ヱ", "ヲ",
  "ン", "ヴ", "",   "゛", "゜",
  "ガ", "ギ", "グ", "ゲ", "ゴ",
  "ザ", "ジ", "ズ", "ゼ", "ゾ",
  "ダ", "ヂ", "ヅ", "デ", "ド",
  "バ", "ビ", "ブ", "ベ", "ボ",
  "パ", "ピ", "プ", "ペ", "ポ",
  "ァ", "ィ", "ゥ", "ェ", "ォ",
  "ャ", "",   "ュ", "",   "ョ",
  "ッ", "ヮ", "ヵ", "ヶ",
  NULL
};

gchar *hiragana[] = {
  "あ", "い", "う", "え", "お",
  "か", "き", "く", "け", "こ",
  "さ", "し", "す", "せ", "そ",
  "た", "ち", "つ", "て", "と",
  "な", "に", "ぬ", "ね", "の",
  "は", "ひ", "ふ", "へ", "ほ",
  "ま", "み", "む", "め", "も",
  "や", "",   "ゆ", "",   "よ",
  "ら", "り", "る", "れ", "ろ",
  "わ", "ゐ", "",   "ゑ", "を",
  "ん", "う゛", "", "゛", "゜",
  "が", "ぎ", "ぐ", "げ", "ご",
  "ざ", "じ", "ず", "ぜ", "ぞ",
  "だ", "ぢ", "づ", "で", "ど",
  "ば", "び", "ぶ", "べ", "ぼ",
  "ぱ", "ぴ", "ぷ", "ぺ", "ぽ",
  "ぁ", "ぃ", "ぅ", "ぇ", "ぉ",
  "ゃ", "",   "ゅ", "",   "ょ",
  "っ", "ゎ",
  NULL
};

gchar *kana_symbols[] = {
  "ー", "、", "。", "！", "？",
  NULL
};

gchar *kigou[] = {
  "　", "￣", "＿", "‖", "｜",
  "♂", "♀", "＃", "＆", "＊",
  "＠", "※", "〒", "〓", "☆",
  "★", "○", "●", "◎", "◇",
  "◆", "□", "■", "△", "▲",
  "▽", "▼", "♯", "♭", "♪",
  "§", "†", "‡", "¶", "◯",
  NULL
};

gchar *bracket[] = {
  "‘", "’", "“", "”", "（",
  "）", "〔", "〕", "［", "］",
  "｛", "｝", "〈", "〉", "《",
  "》", "「", "」", "『", "』",
  "【", "】", "〝", "〟",
  NULL
};

gchar *arrow[] = {
  "→", "←", "↑", "↓",
  NULL
};

gchar *omission[] = {
  "㍻", "№", "㏍", "℡", "㊤",
  "㊥", "㊦", "㊧", "㊨", "㈱",
  "㈲", "㈹", "㍾", "㍽", "㍼",
  "™", "©", "®",
  NULL
};

gchar *unit[] = {
  "℃", "￥", "＄", "¢", "£",
  "％", "‰", "°", "′", "″",
  "㍉", "㌔", "㌢", "㍍", "㌘",
  "㌧", "㌃", "㌶", "㍑", "㍗",
  "㌍", "㌦", "㌣", "㌫", "㍊",
  "㌻", "㎜", "㎝", "㎞", "㎎",
  "㎏", "㏄", "ℓ", "㎟", "㎠",
  "㎡", "㎢", "㎣", "㎤", "㎥",
  "㎦", "Å",
  NULL
};

gchar *dot[] = {
  "、", "。", "，", "．", "・",
  "：", "；", "？", "！", "゛",
  "゜", "´", "｀", "¨", "＾",
  "ヽ", "ヾ", "ゝ", "ゞ", "〃",
  "仝", "々", "〆", "〇", "ー",
  "―", "‐", "／", "＼", "〜",
  "…", "‥", "°", "′", "″",
  NULL
};

gchar *academic[] = {
  "＋", "－", "±", "×", "÷",
  "＝", "≠", "≒", "≡", "∽",
  "＜", "＞", "≦", "≧", "∞",
  "∴", "∵", "∫", "∬", "∮",
  "∂", "∇", "≪", "≫", "√",
  "∝", "∑", "∠", "⊥", "⌒",
  "∟", "⊿", "∈", "∋", "⊆",
  "⊇", "⊂", "⊃", "∪", "∩",
  "∧", "∨", "￢", "⇒", "⇔",
  "∀", "∃",
  NULL
};

gchar *number[] = {
  "①", "②", "③", "④", "⑤",
  "⑥", "⑦", "⑧", "⑨", "⑩",
  "⑪", "⑫", "⑬", "⑭", "⑮",
  "⑯", "⑰", "⑱", "⑲", "⑳",
  "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ",
  "Ⅵ", "Ⅶ", "Ⅷ", "Ⅸ", "Ⅹ",
  "ⅰ", "ⅱ", "ⅲ", "ⅳ", "ⅴ",
  "ⅵ", "ⅶ", "ⅷ", "ⅸ", "ⅹ",
  NULL
};

gchar *greek_capital[] = {
  "Α", "Β", "Γ", "Δ", "Ε",
  "Ζ", "Η", "Θ", "Ι", "Κ",
  "Λ", "Μ", "Ν", "Ξ", "Ο",
  "Π", "Ρ", "Σ", "Τ", "Υ",
  "Φ", "Χ", "Ψ", "Ω",
  NULL
};

gchar *greek_small[] = {
  "α", "β", "γ", "δ", "ε",
  "ζ", "η", "θ", "ι", "κ",
  "λ", "μ", "ν", "ξ", "ο",
  "π", "ρ", "σ", "τ", "υ",
  "φ", "χ", "ψ", "ω",
  NULL
};

gchar *cyrillic_capital[] = {
  "А", "Б", "В", "Г", "Д",
  "Е", "Ё", "Ж", "З", "И",
  "Й", "К", "Л", "М", "Н",
  "О", "П", "Р", "С", "Т",
  "У", "Ф", "Х", "Ц", "Ч",
  "Ш", "Щ", "Ъ", "Ы", "Ь",
  "Э", "Ю", "Я",
  NULL
};

gchar *cyrillic_small[] = {
  "а", "б", "в", "г", "д",
  "е", "ё", "ж", "з", "и",
  "й", "к", "л", "м", "н",
  "о", "п", "р", "с", "т",
  "у", "ф", "х", "ц", "ч",
  "ш", "щ", "ъ", "ы", "ь",
  "э", "ю", "я",
  NULL
};

gchar *line[] = {
  "─", "│", "┼", "",   "",
  "┌", "┐", "┘", "└", "",
  "├", "┬", "┤", "┴", "",
  "━", "┃", "╋", "",   "",
  "┏", "┓", "┛", "┗", "",
  "┣", "┳", "┫", "┻", "",
  "┠", "┯", "┨", "┷", "┿",
  "┝", "┰", "┥", "┸", "╂",
  NULL
};

static int uim_fd = -1;
static unsigned int read_tag;

static GtkWidget *buttontable_create(char **table, int tablelen);
static GtkWidget *create_hiragana_tab(void);
static GtkWidget *create_katakana_tab(void);
static GtkWidget *create_eisu_tab(void);
static GtkWidget *create_symbol_tab(void);
static GtkWidget *create_greek_tab(void);
static GtkWidget *create_cyrillic_tab(void);

static void       check_helper_connection(void);
static void       helper_disconnect_cb(void);
static void       input_pad_create(void);
static GtkWidget *input_table_create(gchar *localename);
static void       padbutton_clicked(GtkButton *button, gpointer user_data);


static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  gchar *msg;
  int fd = g_io_channel_unix_get_fd(channel);

  uim_helper_read_proc(fd);

  while ((msg = uim_helper_get_message())) {
    /* do nothing */
    free(msg);
  }

  return TRUE;
}

static void
check_helper_connection(void)
{
  if (uim_fd < 0) {
    uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    if (uim_fd >= 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				fd_read_cb, NULL);
      g_io_channel_unref(channel);
    }
  }
}

static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  g_source_remove(read_tag);
}

static void
padbutton_clicked(GtkButton *button, gpointer user_data)
{
  GString *tmp;
  const gchar *str = gtk_button_get_label(GTK_BUTTON(button));
  if (!str)
    return;

  tmp = g_string_new("commit_string\n");
  g_string_append(tmp, str);
  g_string_append(tmp, "\n");

  uim_helper_send_message(uim_fd, tmp->str);

  g_string_free(tmp, TRUE);
}

static GtkWidget *
buttontable_create(gchar **table, int len)
{
  GtkWidget *_table;
  GtkWidget *button;
  gint i,j;
  gint rows = ((len-2)/ BUTTON_H_ALIGN)+1;

#if GTK_CHECK_VERSION(3, 4, 0)
  _table = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(_table), 3);
  gtk_grid_set_column_spacing(GTK_GRID(_table), 3);
#else
  _table = gtk_table_new(rows,
			 BUTTON_H_ALIGN,
			 TRUE);
  gtk_table_set_row_spacings(GTK_TABLE(_table), 3);
  gtk_table_set_col_spacings(GTK_TABLE(_table), 3);
#endif

  for (i=0; i < rows; i++) {
    for (j=0; j < BUTTON_H_ALIGN; j++) {
      if (table[i*BUTTON_H_ALIGN + j] == NULL)
	goto out;
      if (strcmp(table[i*BUTTON_H_ALIGN + j], "") == 0)
	continue;

      button = gtk_button_new_with_label(table[i*BUTTON_H_ALIGN + j]);
      g_signal_connect(button, "clicked",
		       G_CALLBACK(padbutton_clicked), "button");

#if GTK_CHECK_VERSION(3, 4, 0)
      gtk_widget_set_hexpand(button, TRUE);
      gtk_widget_set_vexpand(button, TRUE);
      gtk_grid_attach(GTK_GRID(_table), button, j, i, 1, 1);
#else
      gtk_table_attach_defaults(GTK_TABLE(_table),
				button,
				j, j + 1,
				i, i + 1);
#endif
    }
  }
 out:
  return _table;
}

static GtkWidget *
create_tab(gchar *table[], guint len)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(table, len),
		     FALSE, FALSE, 0);

  return vbox;
}

static GtkWidget *
create_hiragana_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(hiragana, sizeof(hiragana)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(kana_symbols, sizeof(kana_symbols)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;
}

static GtkWidget *
create_katakana_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(katakana, sizeof(katakana)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(kana_symbols, sizeof(kana_symbols)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;
}

static GtkWidget *
create_eisu_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(alphabet_capital, sizeof(alphabet_capital)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(alphabet_small, sizeof(alphabet_small)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(numbers, sizeof(numbers)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(symbols, sizeof(symbols)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;

}

static GtkWidget *
create_symbol_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(dot, sizeof(dot)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(kigou, sizeof(kigou)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(bracket, sizeof(bracket)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(arrow, sizeof(arrow)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;

}

static GtkWidget *
create_greek_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(greek_capital, sizeof(greek_capital)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(greek_small, sizeof(greek_small)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;

}

static GtkWidget *
create_cyrillic_tab(void)
{
  GtkWidget *vbox;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox = gtk_vbox_new(FALSE, 10);
#endif

  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(cyrillic_capital, sizeof(cyrillic_capital)/sizeof(gchar*)),
		     FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox),
		     buttontable_create(cyrillic_small, sizeof(cyrillic_small)/sizeof(gchar*)),
		     FALSE, FALSE, 0);

  return vbox;

}

static void
input_pad_create(void)
{
  GtkWidget *input_pad_win;
  GtkWidget *input_table;

  input_pad_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(input_pad_win),
		       _("ja-pad"));
  gtk_window_set_accept_focus(GTK_WINDOW(input_pad_win), FALSE);
  g_signal_connect(G_OBJECT(input_pad_win), "destroy",
		   G_CALLBACK(gtk_main_quit), NULL);


  input_table = input_table_create(NULL);

  gtk_container_add(GTK_CONTAINER(input_pad_win), input_table);

  gtk_widget_show_all(input_pad_win);

}

static GtkWidget *
input_table_create(gchar *localename)
{
  GtkWidget *notebook;

  notebook = gtk_notebook_new();
  gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TRUE);
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_hiragana_tab(),
			   gtk_label_new(_("hiragana")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_katakana_tab(),
			   gtk_label_new(_("katakana")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_eisu_tab(),
			   gtk_label_new(_("eisu")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_symbol_tab(),
			   gtk_label_new(_("symbol")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_tab(omission, sizeof(omission)/sizeof(gchar*)),
			   gtk_label_new(_("omission")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_tab(unit, sizeof(unit)/sizeof(gchar*)),
			   gtk_label_new(_("unit")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_tab(number, sizeof(number)/sizeof(gchar*)),
			   gtk_label_new(_("number")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_tab(academic, sizeof(academic)/sizeof(gchar*)),
			   gtk_label_new(_("academic")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_greek_tab(),
			   gtk_label_new(_("greek")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_cyrillic_tab(),
			   gtk_label_new(_("cyrillic")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			   create_tab(line, sizeof(line)/sizeof(gchar*)),
			   gtk_label_new(_("line")));
  return notebook;
}

int
main(int argc, char *argv[])
{
  setlocale(LC_ALL, "");
  bindtextdomain( PACKAGE, LOCALEDIR );
  textdomain( PACKAGE );
  bind_textdomain_codeset( PACKAGE, "UTF-8");

  gtk_init(&argc, &argv);

  /* create GUI parts */
  input_pad_create();

  /* confirm helper connection */
  check_helper_connection();

  gtk_main ();
  return 0;
}
