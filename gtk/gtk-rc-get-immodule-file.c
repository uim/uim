#include <gtk/gtk.h>
#include <stdio.h>

int main() {
	gchar *path;

	path = gtk_rc_get_im_module_file();
	printf("%s", path);

	return 0;
}
