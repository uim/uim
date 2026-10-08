#!/bin/sh
#
# Usage: update-po.sh pot|update-po SOURCE_DIR BUILD_DIR VERSION \
#          XGETTEXT MSGMERGE MSGATTRIB
#
# Generates uim.pot in BUILD_DIR, and updates the .po files in
# SOURCE_DIR with it for update-po. Both Meson and Autotools use this.
#
# xgettext accepts a single --from-code per run, but some scm/*.scm
# files are still EUC-JP. Those are listed in POTFILES.eucjp.in and
# extracted separately into eucjp.pot, which is then fed into the main
# UTF-8 run as an additional input. Remove all of this once every
# source file is UTF-8.

set -eu

command=$1
source_dir=$2
build_dir=$3
version=$4
xgettext=$5
msgmerge=$6
msgattrib=$7

# GETTEXTDATADIRS lets xgettext find its/*.loc, which describe how to
# extract strings from our GMenu XML files.
# --keyword=Description: an extra key to extract from
# gtk3/toolbar/UimApplet.panel-applet.desktop.in.in.
xgettext_run() {
  GETTEXTDATADIRS="${source_dir}/po" "${xgettext}" \
    --default-domain=uim \
    --add-comments=TRANSLATORS: \
    --keyword=_ \
    --keyword=N_ \
    --keyword=NC_:1c,2 \
    --keyword=Description \
    --package-name=uim \
    --package-version="${version}" \
    --copyright-holder='uim Developers' \
    --msgid-bugs-address='uim-en@googlegroups.com' \
    --directory="${source_dir}" \
    "$@"
}

xgettext_run \
  --from-code=EUC-JP \
  --files-from="${source_dir}/po/POTFILES.eucjp.in" \
  --output="${build_dir}/eucjp.pot"
xgettext_run \
  --from-code=UTF-8 \
  --files-from="${source_dir}/po/POTFILES.in" \
  --output="${build_dir}/uim.pot" \
  "${build_dir}/eucjp.pot"

if [ "${command}" = "update-po" ]; then
  for lang in $(sed -e '/^#/d' "${source_dir}/po/LINGUAS"); do
    po="${source_dir}/po/${lang}.po"
    "${msgmerge}" --update --backup=none "${po}" "${build_dir}/uim.pot"
    "${msgattrib}" --no-obsolete --output-file="${po}" "${po}"
  done
fi
