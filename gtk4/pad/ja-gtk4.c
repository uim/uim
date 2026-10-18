/*

  ja-gtk4.c: Japanese input pad for GTK 4

  Copyright (c) 2004-2026 uim Project https://github.com/uim/uim

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

  This pad only ever emits characters through uim's generic
  "commit_string" helper message, so like the toolbar and switcher it
  has no notion of which input method engine is currently active.
*/

#include <config.h>
#include <gtk/gtk.h>

#include <locale.h>
#include <string.h>
#include <stdlib.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/gettext.h>

#define BUTTON_H_ALIGN 5

/* written in unicode */
static gchar *alphabet_capital[] = {
  "Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ",
  "Ｆ", "Ｇ", "Ｈ", "Ｉ", "Ｊ",
  "Ｋ", "Ｌ", "Ｍ", "Ｎ", "Ｏ",
  "Ｐ", "Ｑ", "Ｒ", "Ｓ", "Ｔ",
  "Ｕ", "Ｖ", "Ｗ", "Ｘ", "Ｙ",
  "Ｚ", NULL
};

static gchar *alphabet_small[] = {
  "ａ", "ｂ", "ｃ", "ｄ", "ｅ",
  "ｆ", "ｇ", "ｈ", "ｉ", "ｊ",
  "ｋ", "ｌ", "ｍ", "ｎ", "ｏ",
  "ｐ", "ｑ", "ｒ", "ｓ", "ｔ",
  "ｕ", "ｖ", "ｗ", "ｘ", "ｙ",
  "ｚ", NULL
};

static gchar *numbers[] = {
  "０", "１", "２", "３", "４",
  "５", "６", "７", "８", "９",
  NULL
};

static gchar *symbols[] = {
  "−", "，", "．", "！", "？",
  NULL
};

static gchar *katakana[] = {
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

static gchar *hiragana[] = {
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

static gchar *kana_symbols[] = {
  "ー", "、", "。", "！", "？",
  NULL
};

static gchar *kigou[] = {
  "　", "￣", "＿", "‖", "｜",
  "♂", "♀", "＃", "＆", "＊",
  "＠", "※", "〒", "〓", "☆",
  "★", "○", "●", "◎", "◇",
  "◆", "□", "■", "△", "▲",
  "▽", "▼", "♯", "♭", "♪",
  "§", "†", "‡", "¶", "◯",
  NULL
};

static gchar *bracket[] = {
  "‘", "’", "“", "”", "（",
  "）", "〔", "〕", "［", "］",
  "｛", "｝", "〈", "〉", "《",
  "》", "「", "」", "『", "』",
  "【", "】", "〝", "〟",
  NULL
};

static gchar *arrow[] = {
  "→", "←", "↑", "↓",
  NULL
};

static gchar *omission[] = {
  "㍻", "№", "㏍", "℡", "㊤",
  "㊥", "㊦", "㊧", "㊨", "㈱",
  "㈲", "㈹", "㍾", "㍽", "㍼",
  "™", "©", "®",
  NULL
};

static gchar *unit[] = {
  "℃", "￥", "＄", "¢", "£",
  "％", "‰", "°", "′", "″",
  "㍉", "㌔", "㌢", "㍍", "㌘",
  "㌧", "㌃", "㌶", "㍑", "㍗",
  "㌍", "㌦", "㌣", "㌫", "㍊",
  "㌻", "㎜", "㎝", "㎞", "㎎",
  "㎏", "㏄", "ℓ", "㎟", "㎠",
  "㎡", "㎢", "㎣", "㎤", "㎥",
  "㎦", "Å",
  NULL
};

static gchar *dot[] = {
  "、", "。", "，", "．", "・",
  "：", "；", "？", "！", "゛",
  "゜", "´", "｀", "¨", "＾",
  "ヽ", "ヾ", "ゝ", "ゞ", "〃",
  "仝", "々", "〆", "〇", "ー",
  "―", "‐", "／", "＼", "〜",
  "…", "‥", "°", "′", "″",
  NULL
};

static gchar *academic[] = {
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

static gchar *number[] = {
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

static gchar *greek_capital[] = {
  "Α", "Β", "Γ", "Δ", "Ε",
  "Ζ", "Η", "Θ", "Ι", "Κ",
  "Λ", "Μ", "Ν", "Ξ", "Ο",
  "Π", "Ρ", "Σ", "Τ", "Υ",
  "Φ", "Χ", "Ψ", "Ω",
  NULL
};

static gchar *greek_small[] = {
  "α", "β", "γ", "δ", "ε",
  "ζ", "η", "θ", "ι", "κ",
  "λ", "μ", "ν", "ξ", "ο",
  "π", "ρ", "σ", "τ", "υ",
  "φ", "χ", "ψ", "ω",
  NULL
};

static gchar *cyrillic_capital[] = {
  "А", "Б", "В", "Г", "Д",
  "Е", "Ё", "Ж", "З", "И",
  "Й", "К", "Л", "М", "Н",
  "О", "П", "Р", "С", "Т",
  "У", "Ф", "Х", "Ц", "Ч",
  "Ш", "Щ", "Ъ", "Ы", "Ь",
  "Э", "Ю", "Я",
  NULL
};

static gchar *cyrillic_small[] = {
  "а", "б", "в", "г", "д",
  "е", "ё", "ж", "з", "и",
  "й", "к", "л", "м", "н",
  "о", "п", "р", "с", "т",
  "у", "ф", "х", "ц", "ч",
  "ш", "щ", "ъ", "ы", "ь",
  "э", "ю", "я",
  NULL
};

static gchar *line[] = {
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
static guint read_tag;
static GtkWidget *pad_window;

static void check_helper_connection(void);
static void helper_disconnect_cb(void);

static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  gchar *msg;
  int fd = g_io_channel_unix_get_fd(channel);

  uim_helper_read_proc(fd);
  while ((msg = uim_helper_get_message()))
    free(msg); /* nothing to react to */

  return G_SOURCE_CONTINUE;
}

static void
check_helper_connection(void)
{
  if (uim_fd >= 0)
    return;

  uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (uim_fd >= 0) {
    GIOChannel *channel = g_io_channel_unix_new(uim_fd);

    read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
                              fd_read_cb, NULL);
    g_io_channel_unref(channel);
  }
}

static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  if (read_tag) {
    g_source_remove(read_tag);
    read_tag = 0;
  }
}

static void
padbutton_clicked(GtkButton *button, gpointer user_data)
{
  const gchar *str = gtk_button_get_label(button);
  GString *tmp;

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
  GtkWidget *grid;
  gint i, j;
  gint rows = ((len - 2) / BUTTON_H_ALIGN) + 1;

  grid = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 3);

  for (i = 0; i < rows; i++) {
    for (j = 0; j < BUTTON_H_ALIGN; j++) {
      GtkWidget *button;

      if (table[i * BUTTON_H_ALIGN + j] == NULL)
        return grid;
      if (!*table[i * BUTTON_H_ALIGN + j])
        continue;

      button = gtk_button_new_with_label(table[i * BUTTON_H_ALIGN + j]);
      g_signal_connect(button, "clicked",
                       G_CALLBACK(padbutton_clicked), NULL);
      gtk_widget_set_hexpand(button, TRUE);
      gtk_widget_set_vexpand(button, TRUE);
      gtk_grid_attach(GTK_GRID(grid), button, j, i, 1, 1);
    }
  }
  return grid;
}

static GtkWidget *
create_tab(gchar *table[], guint len)
{
  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  gtk_box_append(GTK_BOX(vbox), buttontable_create(table, len));
  return vbox;
}

static GtkWidget *
create_two_row_tab(gchar *table1[], guint len1, gchar *table2[], guint len2)
{
  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  gtk_box_append(GTK_BOX(vbox), buttontable_create(table1, len1));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(table2, len2));
  return vbox;
}

#define TABLEN(t) (sizeof(t) / sizeof(gchar *))

static GtkWidget *
create_hiragana_tab(void)
{
  return create_two_row_tab(hiragana, TABLEN(hiragana),
                            kana_symbols, TABLEN(kana_symbols));
}

static GtkWidget *
create_katakana_tab(void)
{
  return create_two_row_tab(katakana, TABLEN(katakana),
                            kana_symbols, TABLEN(kana_symbols));
}

static GtkWidget *
create_eisu_tab(void)
{
  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  gtk_box_append(GTK_BOX(vbox), buttontable_create(alphabet_capital, TABLEN(alphabet_capital)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(alphabet_small, TABLEN(alphabet_small)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(numbers, TABLEN(numbers)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(symbols, TABLEN(symbols)));
  return vbox;
}

static GtkWidget *
create_symbol_tab(void)
{
  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  gtk_box_append(GTK_BOX(vbox), buttontable_create(dot, TABLEN(dot)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(kigou, TABLEN(kigou)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(bracket, TABLEN(bracket)));
  gtk_box_append(GTK_BOX(vbox), buttontable_create(arrow, TABLEN(arrow)));
  return vbox;
}

static GtkWidget *
create_greek_tab(void)
{
  return create_two_row_tab(greek_capital, TABLEN(greek_capital),
                            greek_small, TABLEN(greek_small));
}

static GtkWidget *
create_cyrillic_tab(void)
{
  return create_two_row_tab(cyrillic_capital, TABLEN(cyrillic_capital),
                            cyrillic_small, TABLEN(cyrillic_small));
}

static GtkWidget *
input_table_create(void)
{
  GtkWidget *notebook = gtk_notebook_new();

  gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TRUE);
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_hiragana_tab(),
                           gtk_label_new(_("hiragana")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_katakana_tab(),
                           gtk_label_new(_("katakana")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_eisu_tab(),
                           gtk_label_new(_("eisu")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_symbol_tab(),
                           gtk_label_new(_("symbol")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
                           create_tab(omission, TABLEN(omission)),
                           gtk_label_new(_("omission")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
                           create_tab(unit, TABLEN(unit)),
                           gtk_label_new(_("unit")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
                           create_tab(number, TABLEN(number)),
                           gtk_label_new(_("number")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
                           create_tab(academic, TABLEN(academic)),
                           gtk_label_new(_("academic")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_greek_tab(),
                           gtk_label_new(_("greek")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), create_cyrillic_tab(),
                           gtk_label_new(_("cyrillic")));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
                           create_tab(line, TABLEN(line)),
                           gtk_label_new(_("line")));
  return notebook;
}

static void
activate_cb(GtkApplication *app, gpointer user_data)
{
  pad_window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(pad_window), _("ja-pad"));
  /* GTK4 removed gtk_window_set_accept_focus() (and the whole
   * accept-focus property) with no portable replacement, so this
   * window can no longer refuse focus on its own. */
  gtk_window_set_child(GTK_WINDOW(pad_window), input_table_create());
  gtk_window_present(GTK_WINDOW(pad_window));
}

int
main(int argc, char *argv[])
{
  GtkApplication *app;
  int status;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  check_helper_connection();

  app = gtk_application_new("org.uim.InputPadJaGtk4", G_APPLICATION_NON_UNIQUE);
  g_signal_connect(app, "activate", G_CALLBACK(activate_cb), NULL);
  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  return status;
}
