#!/bin/sh
#
# Usage: update-po.sh pot|update-po SOURCE_DIR BUILD_DIR VERSION \
#          XGETTEXT MSGMERGE MSGATTRIB
#
# Generates uim.pot in BUILD_DIR, and updates the .po files in
# SOURCE_DIR with it for update-po. Both Meson and Autotools use this.

set -eu

command=$1
source_dir=$2
build_dir=$3
version=$4
xgettext=$5
msgmerge=$6
msgattrib=$7

xgettext_run() {
  "${xgettext}" \
    --default-domain=uim \
    --add-comments=TRANSLATORS: \
    --keyword=_ \
    --keyword=N_ \
    --keyword=NC_:1c,2 \
    --package-name=uim \
    --package-version="${version}" \
    --copyright-holder='uim Developers' \
    --msgid-bugs-address='uim-en@googlegroups.com' \
    --directory="${source_dir}" \
    "$@"
}

xgettext_run \
  --from-code=UTF-8 \
  --files-from="${source_dir}/po/POTFILES.in" \
  --output="${build_dir}/uim.pot"

if [ "${command}" = "update-po" ]; then
  for lang in $(sed -e '/^#/d' "${source_dir}/po/LINGUAS"); do
    po="${source_dir}/po/${lang}.po"
    "${msgmerge}" --update --backup=none "${po}" "${build_dir}/uim.pot"
    "${msgattrib}" --no-obsolete --output-file="${po}" "${po}"
  done
fi
