# NEWS

## 2.0.0 - 2026-11-12

### Incompatible changes

  * Used UTF-8 as the character codec. Strings in Scheme are UTF-8
    and `string-length`, `substring` and so on work on characters
    instead of bytes. All input methods use UTF-8 internally.
    * [GH-520](https://github.com/uim/uim/issues/520)
    * [GH-522](https://github.com/uim/uim/issues/522)
    * [GH-524](https://github.com/uim/uim/issues/524)
    * [GH-526](https://github.com/uim/uim/issues/526)
    * [GH-528](https://github.com/uim/uim/issues/528)
    * [GH-535](https://github.com/uim/uim/issues/535)

  * Japanese: Deprecated the definitions with the "-utf8" suffix such
    as `ja-rk-rule-basic-utf8`. They're aliases of the ones without
    the suffix now. `~/.uim` should use the ones without the suffix.
    * [GH-529](https://github.com/uim/uim/issues/529)

  * Anthy: Merged anthy-utf8 into anthy. The anthy plugin uses UTF-8
    with libanthy that can handle it and converts strings to EUC-JP
    otherwise. So it works with libanthy whose default encoding is
    UTF-8 too. The anthy-utf8 plugin is removed. The anthy-utf8 input
    method is kept for compatibility.
    * [GH-477](https://github.com/uim/uim/issues/477)
    * [GH-531](https://github.com/uim/uim/issues/531)
    * Reported by yamam

  * Disabled SIOD compatibility of SigScheme. For example, `set-car!`
    doesn't return the given value anymore. `symbol-value`,
    `set-symbol-value!` and `verbose` are still available.
    * [GH-468](https://github.com/uim/uim/issues/468)
    * [GH-470](https://github.com/uim/uim/issues/470)
    * [GH-474](https://github.com/uim/uim/issues/474)
    * [GH-476](https://github.com/uim/uim/issues/476)

  * Removed the Wnn input method.
    * [GH-405](https://github.com/uim/uim/issues/405)

  * Removed the baidu-olime-jp input method.
    * [GH-509](https://github.com/uim/uim/issues/509)

  * Removed the knotify3, knotify4 and Growl plugins of uim-notify.
    * [GH-412](https://github.com/uim/uim/issues/412)

  * Stopped installing `uim-m17nlib-relink-icons`. `make install`
    makes the links of the m17n-lib icons. Packages that list it need
    to drop it.
    * [GH-402](https://github.com/uim/uim/issues/402)

  * Emacs: Removed XEmacs support.
    * [GH-418](https://github.com/uim/uim/issues/418)

  * GTK 3: Removed the GNOME 3 panel applet.
    * [GH-466](https://github.com/uim/uim/issues/466)

### Improvements

  * Added Meson build support. Autotools is still supported.
    * [GH-395](https://github.com/uim/uim/issues/395)
    * [GH-397](https://github.com/uim/uim/issues/397)
    * [GH-399](https://github.com/uim/uim/issues/399)
    * [GH-400](https://github.com/uim/uim/issues/400)
    * [GH-401](https://github.com/uim/uim/issues/401)
    * [GH-404](https://github.com/uim/uim/issues/404)
    * [GH-407](https://github.com/uim/uim/issues/407)
    * [GH-413](https://github.com/uim/uim/issues/413)
    * [GH-414](https://github.com/uim/uim/issues/414)
    * [GH-417](https://github.com/uim/uim/issues/417)
    * [GH-459](https://github.com/uim/uim/issues/459)
    * [GH-460](https://github.com/uim/uim/issues/460)
    * [GH-461](https://github.com/uim/uim/issues/461)
    * [GH-462](https://github.com/uim/uim/issues/462)
    * [GH-463](https://github.com/uim/uim/issues/463)
    * [GH-464](https://github.com/uim/uim/issues/464)
    * [GH-465](https://github.com/uim/uim/issues/465)
    * [GH-467](https://github.com/uim/uim/issues/467)
    * [GH-469](https://github.com/uim/uim/issues/469)
    * [GH-480](https://github.com/uim/uim/issues/480)
    * [GH-483](https://github.com/uim/uim/issues/483)
    * [GH-494](https://github.com/uim/uim/issues/494)

  * Added an option to enable or disable tests.
    * [GH-416](https://github.com/uim/uim/issues/416)

  * Wayland: Added support for the input method v2 protocol
    (`zwp_input_method_v2`) used by Sway, labwc and so on.
    * [GH-392](https://github.com/uim/uim/issues/392)
    * [GH-492](https://github.com/uim/uim/issues/492)
    * [GH-493](https://github.com/uim/uim/issues/493)
    * [GH-544](https://github.com/uim/uim/issues/544)
    * [GH-548](https://github.com/uim/uim/issues/548)
    * Reported by tista500

  * Wayland: Added support for dark mode in the candidate window.
    * [GH-545](https://github.com/uim/uim/issues/545)

  * Wayland: Added support for fractional scale in the candidate
    window.
    * [GH-546](https://github.com/uim/uim/issues/546)

  * Wayland: Set the default cursor over the candidate window.
    * [GH-547](https://github.com/uim/uim/issues/547)

  * SKK: Added support for completion and dynamic completion while
    romaji is pending.
    * [GH-538](https://github.com/uim/uim/issues/538)
    * Patch by yamam

  * SKK: Used the composing rule changed after `skk.scm` is loaded.
    * [GH-534](https://github.com/uim/uim/issues/534)

  * elatin: Showed all translations of a duplicated key sequence.
    * [GH-172](https://github.com/uim/uim/issues/172)
    * [GH-537](https://github.com/uim/uim/issues/537)
    * Reported by Keith Bowes

  * FEP: Added a libvterm backend to restore the background under the
    preedit.
    * [GH-540](https://github.com/uim/uim/issues/540)
    * [GH-543](https://github.com/uim/uim/issues/543)
    * Patch by yamam

  * Showed the candidate window at the first conversion key press when
    the candidate operation count is 0.
    * [GH-478](https://github.com/uim/uim/issues/478)
    * [GH-490](https://github.com/uim/uim/issues/490)
    * Patch by yamam

  * Annotation: Deferred connections to annotation backends until
    their first use.
    * [GH-434](https://github.com/uim/uim/issues/434)
    * Patch by yamam

  * uim-notify: Translated messages and made them UTF-8.
    * [GH-430](https://github.com/uim/uim/issues/430)
    * [GH-436](https://github.com/uim/uim/issues/436)
    * [GH-439](https://github.com/uim/uim/issues/439)
    * Patch by yamam

  * Notified when initialization of a custom filter fails.
    * [GH-435](https://github.com/uim/uim/issues/435)
    * Patch by yamam

  * Notified errors in files loaded by `try-load` and `try-require`.
    * [GH-481](https://github.com/uim/uim/issues/481)

  * Icons: Drew the uim mark in icons of input methods without their
    own.
    * [GH-398](https://github.com/uim/uim/issues/398)

  * Converted the tcode and trycode sources to UTF-8.
    * [GH-488](https://github.com/uim/uim/issues/488)
    * [GH-489](https://github.com/uim/uim/issues/489)
    * Patch by yamam

  * Updated SigScheme to 1.0.0.
    * [GH-508](https://github.com/uim/uim/issues/508)
    * [GH-510](https://github.com/uim/uim/issues/510)
    * [GH-549](https://github.com/uim/uim/issues/549)

  * Japanese: Updated translation.
    * [GH-433](https://github.com/uim/uim/issues/433)
    * [GH-495](https://github.com/uim/uim/issues/495)

  * Georgian, Korean and French: Updated translations. They aren't
    reviewed by native speakers yet.
    * [GH-499](https://github.com/uim/uim/issues/499)
    * [GH-500](https://github.com/uim/uim/issues/500)
    * [GH-501](https://github.com/uim/uim/issues/501)

### Fixes

  * Fixed a bug that an invalid UTF-8 sequence in a string disabled
    all functionality of libuim.
    * [GH-542](https://github.com/uim/uim/issues/542)

  * SKK: Fixed the okuri position before annotations in the preedit.
    * [GH-513](https://github.com/uim/uim/issues/513)
    * Patch by yamam

  * SKK: Fixed annotations of candidates with okuri and `(concat ...)`
    bodies.
    * [GH-519](https://github.com/uim/uim/issues/519)
    * Patch by yamam

  * Canna: Fixed candidate state corruption.
    * [GH-497](https://github.com/uim/uim/issues/497)
    * Patch by yamam

  * Mozc: Fixed a bug that the focused candidate wasn't highlighted
    when Mozc's focus differs.
    * [GH-403](https://github.com/uim/uim/issues/403)
    * Patch by shitamo

  * Look and DICT: Fixed annotation formatting and the default
    database.
    * [GH-429](https://github.com/uim/uim/issues/429)
    * Patch by yamam

  * DICT: Handled annotation connection failures safely.
    * [GH-411](https://github.com/uim/uim/issues/411)
    * Patch by yamam

  * XIM: Initialized a fallback encoding for clients.
    * [GH-168](https://github.com/uim/uim/issues/168)
    * [GH-514](https://github.com/uim/uim/issues/514)
    * Patch by yamam

  * XIM: Ignored `UTF8_STRING` in encoding negotiation.
    * [GH-515](https://github.com/uim/uim/issues/515)

  * XIM: Handled an empty client locale name.
    * [GH-516](https://github.com/uim/uim/issues/516)

  * Qt 6: Fixed a build error without the private header to get the X
    display.
    * [GH-506](https://github.com/uim/uim/issues/506)
    * [GH-507](https://github.com/uim/uim/issues/507)
    * Reported by Nobutarou

  * GTK 4: Pre-fetched the target page when a page button is clicked
    in the candidate window.
    * [GH-502](https://github.com/uim/uim/issues/502)
    * Patch by shitamo

  * FEP: Guarded escape sequence parsing against buffer overrun.
    * [GH-541](https://github.com/uim/uim/issues/541)
    * Patch by yamam

  * helper: Fixed duplicated `uim-helper-server` processes and
    handling of client disconnects.
    * [GH-438](https://github.com/uim/uim/issues/438)
    * [GH-441](https://github.com/uim/uim/issues/441)
    * [GH-443](https://github.com/uim/uim/issues/443)
    * [GH-444](https://github.com/uim/uim/issues/444)
    * Patch by yamam

  * Fixed stale installed module registrations after reconfiguration.
    * [GH-484](https://github.com/uim/uim/issues/484)
    * Patch by yamam

  * Fixed a bug that the Scheme file of a module was loaded more than
    once.
    * [GH-487](https://github.com/uim/uim/issues/487)
    * Reported by yamam

  * uim-notify: Fixed leaks, long messages and the build of the
    libnotify plugin.
    * [GH-408](https://github.com/uim/uim/issues/408)
    * [GH-410](https://github.com/uim/uim/issues/410)

  * Checked `dlopen()` failures by its return value.
    * [GH-409](https://github.com/uim/uim/issues/409)

### Thanks

  * yamam
  * shitamo
  * Keith Bowes
  * Nobutarou
  * tista500

## 1.9.9 - 2026-10-05

### Improvements

  * Wayland: Added support for passing the text around the cursor.
    * [GH-352](https://github.com/uim/uim/issues/352)

  * Wayland: Added support for content type.
    * [GH-355](https://github.com/uim/uim/issues/355)

  * Wayland: Added support for mouse in the candidate window.
    * [GH-357](https://github.com/uim/uim/issues/357)

  * Wayland: Added support for HiDPI.
    * [GH-359](https://github.com/uim/uim/issues/359)

  * Wayland: Added support for system font in the candidate window.
    * [GH-360](https://github.com/uim/uim/issues/360)

  * IBus: Added IBus engine.
    * [GH-362](https://github.com/uim/uim/issues/362)
    * [GH-363](https://github.com/uim/uim/issues/363)
    * [GH-366](https://github.com/uim/uim/issues/366)
    * [GH-372](https://github.com/uim/uim/issues/372)
    * [GH-379](https://github.com/uim/uim/issues/379)
    * [GH-382](https://github.com/uim/uim/issues/382)
    * [GH-388](https://github.com/uim/uim/issues/388)

  * Mozc: Dropped character palette and hand-writing menu.
    * [GH-353](https://github.com/uim/uim/issues/353)
    * Patch by Kentaro Hayashi

  * Mozc: Improved page turning performance.
    * [GH-358](https://github.com/uim/uim/issues/358)

  * Georgian: Added translation.
    * [GH-373](https://github.com/uim/uim/issues/373)
    * Patch by Ekaterine Papava

  * SKK: Improved prompt in char-code mode.
    * [GH-376](https://github.com/uim/uim/issues/376)
    * Patch by SATO Tatsuya

  * FEP: Improved status line.
    * [GH-383](https://github.com/uim/uim/issues/383)
    * Patch by yamam

  * Removed Wubi86 and Zhengma table data.
    * [GH-386](https://github.com/uim/uim/issues/386)
    * Patch by dai

  * Emacs: Changed to use lexical binding.
    * [GH-364](https://github.com/uim/uim/issues/364)
    * [GH-387](https://github.com/uim/uim/issues/387)
    * Reported by Akira TAGOH

  * Made the description of the "on" mode translatable.
    * [GH-389](https://github.com/uim/uim/issues/389)

### Fixes

  * Mozc: Fixed page shift in the candidate window.
    * [GH-356](https://github.com/uim/uim/issues/356)
    * Patch by Kentaro Hayashi

  * Mozc: Stopped undoing a commit into another field.
    * [GH-384](https://github.com/uim/uim/issues/384)

  * TUT-Code: Fixed mazegaki conversion.
    * [GH-354](https://github.com/uim/uim/issues/354)
    * Patch by yamam

  * TUT-Code: Fixed bushu lookup for EUC-JP dictionaries.
    * [GH-374](https://github.com/uim/uim/issues/374)
    * Patch by yamam

  * Emacs: Fixed IM list delivery.
    * [GH-367](https://github.com/uim/uim/issues/367)
    * Patch by yamam

### Thanks

  * Kentaro Hayashi
  * yamam
  * Ekaterine Papava
  * SATO Tatsuya
  * dai
  * Akira TAGOH

## 1.9.8 - 2026-09-23

### Improvements

  * `uim-im-switcher-cli`: Added.
    * [GH-170](https://github.com/uim/uim/issues/170)
    * [GH-333](https://github.com/uim/uim/issues/333)
    * Reported by vas
    * Patch by yamam

  * `uim-custom-cli`: Added.
    * [GH-127](https://github.com/uim/uim/issues/127)
    * [GH-335](https://github.com/uim/uim/issues/335)
    * Reported by KimBomm
    * Patch by yamam

  * Wayland: Added support for the input method v1 protocol.
    * [GH-339](https://github.com/uim/uim/issues/339)
    * [GH-341](https://github.com/uim/uim/issues/341)
    * [GH-343](https://github.com/uim/uim/issues/343)
    * [GH-345](https://github.com/uim/uim/issues/345)
    * [GH-349](https://github.com/uim/uim/issues/349)

  * `process`: Added support for environments where standard I/O is
    closed.
    * [GH-340](https://github.com/uim/uim/issues/340)

### Fixes

  * Emacs: Fixed Delete key being processed as Backspace.
    * [GH-52](https://github.com/uim/uim/issues/52)
    * [GH-336](https://github.com/uim/uim/issues/336)
    * Patch by yamam

  * SKK: Fixed a bug in UTF-8 dictionary registration.
    * [GH-337](https://github.com/uim/uim/issues/337)
    * Patch by yamam

  * Source archive: Added missing `sigscheme/libgcroots/autogen.sh`.
    * [GH-342](https://github.com/uim/uim/issues/342)
    * [GH-344](https://github.com/uim/uim/issues/344)
    * Reported by NOKUBI Takatsugu

  * Fixed use of temporary strings after they become invalid.
    * [GH-346](https://github.com/uim/uim/issues/346)

### Thanks

  * vas
  * yamam
  * KimBomm
  * NOKUBI Takatsugu

## 1.9.7 - 2026-09-20

### Improvements

  * Qt3: Removed `KSeparator`
    * [GH-252](https://github.com/uim/uim/issues/252)
    * Patch by OBATA Akio

  * TQt: Added support for TDE (Trinity Desktop Environment) and TQt.
    * [GH-255](https://github.com/uim/uim/issues/255)
    * Patch by OBATA Akio

  * Anthy: Added support for debian/anthy as UTF-8 supported Anthy.
    * [GH-259](https://github.com/uim/uim/issues/259)

  * GTK 4: Added support for GTK 4 IM module.
    * [GH-173](https://github.com/uim/uim/issues/173)
    * [GH-263](https://github.com/uim/uim/issues/263)
    * [GH-321](https://github.com/uim/uim/issues/321)

  * fep: Improved ncurses detection.
    * [GH-274](https://github.com/uim/uim/issues/274)
    * Patch by Nicolas PARLANT

  * KDE5: Added `--disable-kde5-applet`.
    * [GH-274](https://github.com/uim/uim/issues/274)
    * Patch by Nicolas PARLANT

  * sigscheme: Updated bundled sigscheme to 0.9.5.
    * [GH-281](https://github.com/uim/uim/issues/281)

  * fep: Improved.
    * [GH-282](https://github.com/uim/uim/issues/282)
    * [GH-285](https://github.com/uim/uim/issues/285)
    * [GH-286](https://github.com/uim/uim/issues/286)
    * [GH-287](https://github.com/uim/uim/issues/287)
    * [GH-288](https://github.com/uim/uim/issues/288)
    * [GH-290](https://github.com/uim/uim/issues/290)
    * [GH-291](https://github.com/uim/uim/issues/291)
    * [GH-299](https://github.com/uim/uim/issues/299)
    * [GH-300](https://github.com/uim/uim/issues/300)
    * [GH-301](https://github.com/uim/uim/issues/301)
    * [GH-324](https://github.com/uim/uim/issues/324)
    * [GH-325](https://github.com/uim/uim/issues/325)
    * Patch by yamam

  * GTK+ 2: Removed again.
    * [GH-294](https://github.com/uim/uim/issues/294)
    * Patch by dai

  * SKK: Improved.
    * [GH-296](https://github.com/uim/uim/issues/296)
    * [GH-298](https://github.com/uim/uim/issues/298)
    * Patch by yamam

  * SKK: Added support for UTF-8.
    * [GH-302](https://github.com/uim/uim/issues/302)
    * See the following documents for how to use UTF-8:
      * https://github.com/uim/uim/blob/master/doc/skk-utf8-migration-guide.md
      * https://github.com/uim/uim/blob/master/doc/skk-utf8-migration-guide-ja.md
    * Patch by tattsan
    * Patch by yamam

  * Removed unused components.
    * Ajax IME: [GH-306](https://github.com/uim/uim/issues/306)
    * Yahoo! JAPAN web API: [GH-306](https://github.com/uim/uim/issues/306)
    * curl plugin: [GH-305](https://github.com/uim/uim/issues/305)
    * Composer framework: [GH-308](https://github.com/uim/uim/issues/308)
    * Zaurus: [GH-316](https://github.com/uim/uim/issues/316)
    * PRIME: [GH-314](https://github.com/uim/uim/issues/314)
    * Mana: [GH-314](https://github.com/uim/uim/issues/314)
    * SJ3: [GH-314](https://github.com/uim/uim/issues/314)
    * Scheme debugger: [GH-315](https://github.com/uim/uim/issues/315)
    * Browser preferences UI: [GH-312](https://github.com/uim/uim/issues/312)
    * Patch by dai

  * Emacs: Suppressed leim related warnings.
    * [GH-304](https://github.com/uim/uim/issues/304)
    * Patch by yamam

  * OpenSSL: Added support for recent OpenSSL.
    * [GH-311](https://github.com/uim/uim/issues/311)

  * gettext: Added support for gettext 0.23 or later.
    * [GH-221](https://github.com/uim/uim/issues/221)
    * [GH-313](https://github.com/uim/uim/issues/313)
    * Reported by NOKUBI Takatsugu

  * Mozc: Imported Mozc support from MacUIM.
    * [GH-226](https://github.com/uim/uim/issues/226)
    * [GH-318](https://github.com/uim/uim/issues/318)
    * [GH-330](https://github.com/uim/uim/issues/330)
    * Patch by yamam

  * icon: Added `uim-icon.svg`.
    * [GH-128](https://github.com/uim/uim/issues/128)
    * [GH-328](https://github.com/uim/uim/issues/328)

### Fixes

  * Qt3: Fixed missing include path.
    * [GH-251](https://github.com/uim/uim/issues/251)
    * Patch by OBATA Akio

  * Qt: Fixed `bushu.t` install failure.
    * [GH-254](https://github.com/uim/uim/issues/254)
    * Patch by OBATA Akio

  * notify: Added missing include path.
    * [GH-274](https://github.com/uim/uim/issues/274)
    * Patch by Nicolas PARLANT

  * SKK: Made SKK really optional.
    * [GH-274](https://github.com/uim/uim/issues/274)
    * Patch by Nicolas PARLANT

  * XKB: Avoided building XKB without X detection.
    * [GH-274](https://github.com/uim/uim/issues/274)
    * Patch by Nicolas PARLANT

  * SKK: Fixed socket leak.
    * [GH-279](https://github.com/uim/uim/issues/279)
    * Patch by Takahiro Yoshizawa

  * fep: Fixed OOB write.
    * [GH-293](https://github.com/uim/uim/issues/293)
    * Patch by mox

  * Emacs: Suppressed false-positive "invalid IM engine" error.
    * [GH-310](https://github.com/uim/uim/issues/310)
    * Patch by yamam

  * GTK+ 3: Fixed candidate window staying visible on Wayland.
    * [GH-320](https://github.com/uim/uim/issues/320)

  * Emacs: Fixed a resource leak.
    * [GH-331](https://github.com/uim/uim/issues/331)
    * Patch by yamam

### Thanks

  * OBATA Akio
  * Nicolas PARLANT
  * Takahiro Yoshizawa
  * yamam
  * dai
  * mox
  * tattsan
  * NOKUBI Takatsugu

## 1.9.6 - 2025-05-16

### Improvements

  * `configure`: Changed the default value of
    `--enable-default-toolkit`. It detects the default toolkit
    automatically by default.

  * GTK+ 3: Simplified.

  * GTK+ 2: Added again.
    * [GH-236](https://github.com/uim/uim/issues/236)
    * Patch by hazen2215

### Fixes

  * sqlite3: Added missing `libuim-scm.la` dependency.

### Thanks

  * hazen2215

## 1.9.5 - 2025-05-06

### Fixes

  * GTK+ 3: Fixed a build problem.
    * [GH-237](https://github.com/uim/uim/issues/237)
    * Patch by hazen2215

### Thanks

  * hazen2215

## 1.9.4 - 2025-05-06

### Improvements

  * Suppressed `incompatible-pointer-types` warnings.
    * Reported by SteelDynamite

  * Updated bundled SigScheme to 0.9.3.

  * Dropped support for GTK+ 2.

### Thanks

  * SteelDynamite

## 1.9.3 - 2025-05-04

### Improvements

  * Updated bundled SigScheme to 0.9.2 that is C23 ready. GCC 15 uses
    C23 by default.
    * Reported by SteelDynamite

### Thanks

  * SteelDynamite

## 1.9.2 - 2025-05-04

### Improvements

  * Qt: Changed to use `metadata.json` from `metadata.desktop` because
    `metadata.desktop` is deprecated.
    * [GH-230](https://github.com/uim/uim/issues/230)
    * Patch by 이신혁

  * Qt: Specified `cmake_minimum_required()` explicitly for CMake 4.
    * [GH-231](https://github.com/uim/uim/issues/231)
    * Reported by SteelDynamite

### Thanks

  * 이신혁

  * SteelDynamite

## 1.9.1 - 2025-04-13

### Improvements

  * Anthy: Added support for anthy-unicode.
    * [GH-166](https://github.com/uim/uim/issues/166)
    * [GH-189](https://github.com/uim/uim/issues/189)
    * Patch by Takao Fujiwara

### Fixes

  * Qt: Fixed a bug that Qt5 candidate windows is used with Qt6
    applications.
    * [GH-227](https://github.com/uim/uim/issues/227)
    * Patch by yyyjajp

### Thanks

  * Takao Fujiwara

  * yyyjajp

## 1.9.0 - 2025-02-23

### Summary

  * Improve Qt support including Qt6 support
  * Improve NetBSD support

### Improvements

  * Emacs: Changed to use `set-face-underline` because
    `set-face-underline-p` is removed in Emacs 29.

  * NetBSD: Added workaround for NetBSD-i386-9.1 with gcc-7.5.0 and
    `-O2`.
    * [GH-140](https://github.com/uim/uim/issues/140)
    * Reported by OBATA Akio

  * NetBSD: Improved `sockcred` detection.
    * [GH-209](https://github.com/uim/uim/issues/209)
    * Reported by OBATA Akio

  * NetBSD: Avoided to redefine `_OPENBSD_SOURCE`.
    * [GH-210](https://github.com/uim/uim/issues/210)
    * Reported by OBATA Akio

  * NetBSD: Added support for NetBSD 5.0 or later.
    * [GH-211](https://github.com/uim/uim/issues/211)
    * Reported by OBATA Akio

  * Qt4: Added support for surrogate pairs.
    * [GH-213](https://github.com/uim/uim/issues/213)
    * Patch by OBATA Akio

  * Qt4: Changed to use Qt style input focus handling.
    * [GH-214](https://github.com/uim/uim/issues/214)
    * Patch by OBATA Akio

  * Qt3: Improved input pad.
    * [GH-215](https://github.com/uim/uim/issues/215)
    * Patch by OBATA Akio

  * configure: Improved OpenSSL checks.
    * [GH-198](https://github.com/uim/uim/issues/198)
    * Patch by Sam James

  * Qt6: Added support for Qt 6.
    * [GH-194](https://github.com/uim/uim/issues/194)
    * [GH-218](https://github.com/uim/uim/issues/218)
    * Reported by toimine
    * Patch by yyyjajp

### Fixes

  * SKK: Fixed numeric converted string not null terminated.
    * [GH-175](https://github.com/uim/uim/issues/175)
    * [GH-186](https://github.com/uim/uim/issues/186)
    * Reported by toshjp
    * Patch by SATO Tatsuya

  * Fixed `snprintf()` check with C99 compilers.
    * [GH-187](https://github.com/uim/uim/issues/187)
    * Patch by Florian Weimer

  * Qt5: Fixed a crash bug with Wayland.
    * [GH-155](https://github.com/uim/uim/issues/155)
    * [GH-201](https://github.com/uim/uim/issues/201)
    * Reported by Lasath Fernando
    * Patch by Keith Bowes

  * Fixed wrong cast for `isspace()`.
    * [GH-202](https://github.com/uim/uim/issues/202)
    * Patch by OBATA Akio

  * GTK: Fixed a bug that mode indicator uses wrong background color.
    * [GH-203](https://github.com/uim/uim/issues/203)
    * [GH-204](https://github.com/uim/uim/issues/204)
    * Patch by Kusanagi Kouichi

  * Qt: Fixed a bug that multiarch isn't detected correctly.
    * [GH-205](https://github.com/uim/uim/issues/205)
    * Patch by Eli Schwartz

  * Qt3: Fixed an off-by-one error.
    * [GH-208](https://github.com/uim/uim/issues/208)
    * Patch by OBATA Akio

  * Qt3: Set missing captions.
    * [GH-212](https://github.com/uim/uim/issues/212)
    * Patch by OBATA Akio

  * ipa-x-sampa: Fixed wrong defintions.
    * [GH-216](https://github.com/uim/uim/issues/216)
    * [GH-217](https://github.com/uim/uim/issues/217)
    * Patch by mwgamera

### Thanks

  * toshjp
  * SATO Tatsuya
  * Florian Weimer
  * OBATA Akio
  * Lasath Fernando
  * Keith Bowes
  * Kusanagi Kouichi
  * Eli Schwartz
  * Sam James
  * toimine
  * yyyjajp

## 1.8.9 - 2022-08-19

### Summary

  * Improve build processs
  * Add support for newer Emacs
  * Improve Qt support
  * Improve uim-fep to support WSL and Neovim

### Fixes

  * build
    * Fix a portability bug in configure.ac.
      [GitHub #133][Patch by OBATA Akio]
    * Fix a bug that wrong libedit is detected.
      [GitHub #134][Patch by OBATA Akio]
    * Add support for building with clang.
      [GitHub #135][Patch by Haelwenn Monnier]
    * Remove a C++ style comment.
      [GitHub #137][Patch by OBATA Akio]
    * Improve term related potability.
      [GitHub #138][Patch by OBATA Akio]
    * Fix a build issue on NetBSD 8.
      [GitHub #139][Patch by OBATA Akio]
    * Don't use needless -fno-strict-aliasing for GCC 5 or later.
      [GitHub #140][Patch by OBATA Akio]
    * Add workaround intltoolize.
      [GitHub #179][Reported by calimeroteknik]
  * uim.el
    * Fix a bug that C-x keys aren't usable with Emacs 26.
  * uim-pref
    * Fix a bug that GTK based uim-pref is always used.
      [GitHub #151][Patch by OBATA Akio]
  * uim-qt5
    * Fix a crash bug by losing focus.
      [GitHub #148][Reported by ThisIsPIRI]
  * uim-fep
    * Fix a build error by missing "#include \<termious.h>".
      [GitHub #152][Patch by OBATA Akio]
    * Fix a bug that "wsl" command is hang on uim-fep.
      [GitHub #159][Patch by yamam]
  * uim-gtk3
    * Suppress a "instance has no handler with id" warning.
      [GitHub #158][Reported by isaki68k]
  * uim-toolbar-gtk3
    * Fix a crash bug when input method list is empty.
      [GitHub #162][Reported by 0x005c]
    * Fix a bug that toolbar is grabbed by right click.
      [GitHub #163][Reported by 0x005c]
  * uim-help
    * Fix a bug that wrong URL is opened.
      [GitHub #164][Reported by 0x005c]
  * uim-ipc
    * Fix a bug that wrong character is compared.
      [GitHub #169][Reported by Izumi Tsutsui]

### Enhancements

  * build
    * Drop support for regenerating scm/json-parser-expanded.scm.
      [GitHub #26][Reported by Reuben Thomas]
    * Add support for buliding with --disable-nls without intltool.
      [GitHub #176][Patch by atheik]
  * uim-qt5
    * Add Plasma 5 applet.
      [GitHub #154][Patch by Lasath Fernando]
    * Add support for uim-pref.
      [GitHub #157][Patch by ahigerd]
  * uim.el
    * Add support for Emacs 27.1.
      [GitHub #141][Patch by WJ Liu]
    * Change Korean encoding to utf-8 from euc-kr.
  * uim-tcode
    * Add support for Katakana-mode.
      [GitHub #165][Patch by 0x005c]
  * uim-xim
    * Add support for customizing fontsets.
      [GitHub #177][Patch by atheik]
  * uim-fep
    * Add support for 24bit color.
      [GitHub #180][Patch by yamam]
    * Add support for Neovim.
      [GitHub #180][Patch by yamam]

## 1.8.8 - 2018-05-12

### Summary

  * Fix build error with --with-libgcroots=installed
  * Fix build error of uim-qt5

### Fixes

  * build
    * Fix a build error with --with-libgcroots=installed
      [GitHub #130][Reported by HIGUCHI Daisuke]
  * uim-qt5
    * Fix a build error
      [GitHub #129][Reported by HIGUCHI Daisuke]

## 1.8.7 - 2018-05-10

### Summary

  * Support uim-gtk3 on Wayland
  * Remove uim-social-ime
  * Improve uim-qt5

### Fixes

  * uim-skk
    * Fix a socket clean bug
      [GitHub #32][Reported by HIGUCHI Daisuke]
    * Fix wrong close call
      [GitHub #55][Patch by zephyrus00jp]
  * uim-qt4
    * Add a missing include
      [GitHub #38][Patch by Raphael Kubo da Costa]
  * uim.el
    * Fix a bug that other minor mode key bindings are ignored on Emacs 24
      [GitHub #51][Reported by Araeos]
    * Fix a bug that Shift+Arrow doesn't work
      [GitHub #116][Reported by Vladimir Panteleev]
  * uim-gtk3
    * Fix a bug that the candidate windows isn't shown
      [GitHub #76][Patch by Konosuke Watanab]

### Enhancements

  * all
    * Add "Keywords" to .desktop
      [GitHub #57][Patch by HIGUCHI Daisuke]
  * uim-gtk
    * Improve GTK+ 3 support
      [GitHub #35][Patch by e-kato]
    * Support libpanel-applet 3.14 or later
      [GitHub #48][Patch by Dmitry Shachnev]
    * Support Wayland backend
      [GitHub #71][Reported by Thibaut Girka]
  * uim-fep
    * Use UTF-8 for comment encoding
      [GitHub #46][Patch by Felix Janda]
  * uim-qt5
    * Make workable
  * uim-social-ime
    * Remove because the service was disruption
  * uim.el
    * Support autoload
      [GitHub #121][Patch by Arun Isaac]

## 1.8.6 - 2013-06-30

### Summary

  * Add XDG_RUNTIME_DIR support
  * Fix LINGUAS
  * Avoid a crash when a Canna server name is specified
  * Add an option to uim-m17nlib
  * Update trycode composition table

### Fixes

  * libuim
    * Add XDG_RUNTIME_DIR support (by tagoh)
  * po
    * List language names on each line in LINGUAS (by naota)
  * uim-canna
    * Avoid a crash when a Canna server name is specified (by Kouichi ONO)

### Enhancements

  * uim-m17nlib
    * Add an option to set a candidate windows to be visiable or hidden
  * uim-tutcode
    * Updated trycode composition table retrieved 2013-06-26

## 1.8.5 - 2013-03-31

### Summary

  * Avoid an infinite recursion in uim-gtk when no preedit is started
  * Fix uim-tutcode behavior of postfix katakana conversion

### Fixes

  * uim-gtk2
  * uim-gtk3
    * Avoid an infinite recursion when no preedit is started (by tagoh)

  * uim-tutcode
    * Fix behavior of postfix katakana conversion

## 1.8.4 - 2012-12-30

### Summary

  * Add Hungarian locale support to uim-xim
  * Improved uim-tutcode

### Enhancements

  * uim-xim
    * Add Hungarian locale support (by Kővágó Zoltán)
  * uim-tutcode
    * Enhance postfix katakana conversion
      * Take characters while hiragana continues
      * Exclude specified number of characters
      * Shrink last postfix katakana conversion
    * Updated trycode composition table retrieved 2012-11-22

## 1.8.3 - 2012-09-30

### Summary

  * Avoid SEGV in uim-toolbar-gtk-systray
  * Fix behavior of uim-tutcode on Google Chrome

### Fixes

  * uim-toolbar-gtk-systray
  * uim-toolbar-gtk3-systray
    * Avoid SEGV in some environments ([uim-ja 337])
    * Fix typo ([uim-ja 339])

  * uim-tutcode
    * Fix bug: \<Shift>left/right key erases current selection text
      on Google Chrome

## 1.8.2 - 2012-07-28

### Summary

  * Support sticky key for uim-skk
  * Fix GTK+3 bridge behavior
  * Improved uim-tutcode

### Fixes

  * uim-gtk3
    * Avoid problem with the wrap mode
    * Prevent selection if needed

  * uim-tutcode
    * Fix error when label key is typed just after entering
      interactive bushu mode
    * Fix error on auto help after bushu conversion for some kanji
    * Fix mismatch between preedit display and pending keys after focus out

### Enhancements

  * uim-skk
    * Support sticky key (by anyakichi)

### Others

  * uim-candwin-qt4
  * uim-chardict-qt4
  * uim-pref-qt4
  * uim-im-switcher-qt4
  * uim-toolbar-qt4
  * uim-qt4
    * Honor CPPFLAGS in compilation

  * uim-candiwn-gtk3
  * uim-dict-gtk3
  * uim-input-pad-ja-gtk3
  * uim-pref-gtk3
  * uim-im-switcher-gtk3
  * uim-toolbar-gtk3
  * uim-gtk3
    * Replace deprecated functions

## 1.8.1 - 2012-05-27

### Summary

  * Bug fix for uim-notify with recent versions of glib
  * Improved uim-tutcode

### Fixes

  * uim-toolbar-gtk3-systray
  * uim-toolbar-gtk-systray
    * Fix bug: prevent from becoming a zombie process

  * uim-candwin-gtk3
  * uim-candwin-gtk
    * Redraw unselected text with normal color

  * uim-tutcode
    * Fix bug: key which is not in rule (ex. "5")
      is not entered on (interactive) bushu conversion

  * po
    * Correct the path to GNOME_UimApplet.server.in in POTFILES.skip

  * uim-qt4
    * Honor the environment variable LDFLAGS during build

  * uim-notify
    * Fix compilation with glib >= 2.32

### Enhancements

  * uim-tutcode
    * New customs
      * Show pending key sequences
      * New custom group for tutcode-rule.scm: support TUT+ code
        which has rules for Shin Joyo Kanji
      * Key sequences to change katakana/hiragana mode (not toggle)
    * New command to dump last help content shown on candidate window
    * Pseudo table style for bridge which does not support
      table style candidate window
    * Updated trycode composition table retrieved 2012-05-26

### Others

  * GTK+2 immodule
  * uim-xim
    * Replace deprecated XKeycodeToKeysym() with XkbKeycodeToKeysym()

## 1.8.0 - 2012-03-30

### Summary

  * New IM uim-google-cgiapi-jp
  * New IM uim-baidu-olime-jp
  * Delay showing candidate window
  * Horizontal candidate window
  * Japanese Romaji-Kana composing rule in uim-pref
  * Many new features in uim-tutcode

### Fixes

  * libgcroots
    * Fix version ([uim-en 438])
  * doc
    * Fix release manual.

## 1.8.0-beta - 2012-03-25

### Fixes

  * libuim
    * Fix version number
  * po
    * Fix build failure with intltool 0.50 again (#58097)
  * uim-toolbar-gtk
  * uim-toolbar-gtk3
  * uim-toolbar-qt4
    * Change behavior when toolbar-display-time is "mode"
  * uim-chardict-qt
  * uim-chardict-qt4
    * Update Unicode block list
  * ChangeLog
    * Add ChangeLog.old.2

## 1.8.0-alpha - 2012-03-18

### New features

  * uim-google-cgiapi-jp (new IM)
    * A web-based Japanese multi-segment input method
  * uim-baidu-olime-jp (new IM)
    * A web-based Japanese multi-segment input method
  * osx-dcs
    * OS X Dictionary Services support
  * uim-knotify4
    * notify agent for KDE4

### Fixes

  * uim-xim
    * Check if libXext is installed at configure
  * uim-gtk2
    * Check if gdk-x11-2.0 is installed at configure (#36024)
  * po
    * Fix build failure with intltool 0.50 (#58097)
  * pixmaps
    * Use rsvg-convert instead of rsvg (#47204)

### Enhancements

  * libuim
    * New custom type "table"

  * libuim
  * Gtk+ bridge
  * Qt4 bridge
  * XIM bridge (uim-xim)
    * New API for delay showing candidate window

  * Gtk+ bridge
  * Qt4 bridge
  * XIM bridge (uim-xim)
    * Add horizontal candidate window

  * uim-candwin-gtk
    * Add previous and next page buttons

  * uim-chardict-qt
  * uim-chardict-qt4
    * Merge translation files for chardict into the main translation file

  * uim-m17nlib
    * Make toolbar settings configurable in uim-pref

  * uim-pref-gtk
  * uim-pref-gtk3
  * uim-pref-qt4
    * Support custom type "table"
    * Make Japanese Romaji-Kana composing rule configurable

  * uim-skk
    * Kanji code (JIS/Kuten/Unicode) input mode
    * Add skkserver completion feature (by @eagletmt)

  * uim-tutcode
    * Delay showing candidate window
    * New postfix conversions: katakana,
      kanji to key sequence and key sequence to kanji
    * Conversion on selection: mazegaki, katakana,
      kanji to key sequence and key sequence to kanji
    * New help functions: for character at current position
      and for string in clipboard
    * Clipboard support
      * Paste from clipboard to preedit
      * Key sequence to kanji conversion from clipboard
    * Undo last commit
    * Treat button click on stroke help window as key press
    * New customs
      * Add choice of bushu conversion algorithm:
        tc-2.3.1 and Kanchoku Win
      * New custom to exclude specified number of candidates
        from mazegaki learning
      * New custom to keep key sequence not convertible to kanji
      * Add choice about behavior of commit candidate by label key
      * Support binding user defined procedure to key sequence
    * Updated trycode composition table retrieved 2012-02-18

## 1.7.3 - 2012-02-14

### Fixes

  * Qt4 bridge
    * Fix im-delete-text in QTextEdit
    * Build fix for NetBSD ([uim-ja 322])

  * uim-m17nlib
    * Don't discard key release event when input mode is off (#45734)
    * Fix crashes with some IMs (regressed in 1.7.0-alpha).

### Others

  * Generic build fix for GNU/Hurd ([uim-ja 310])

## 1.7.2 - 2012-01-11

### Fixes

  * libuim
    * Fix crash in im-acquire-text with selection

  * uim-pref-qt4
    * Fix build failure when --as-needed is used (#40385, Julian Taylor)

### Enhancements

  * Qt4 bridge
    * Support text acquisition API with QTextEdit

### Others

  * Plug very minor memory leak in uim-xim
  * Plug very minor file descriptor leak in libuim-custom

## 1.7.1 - 2011-07-26

### Fixes

  * uim-tutcode
    * Fix behavior when on-key and off-key are different

  * uim-dict-gtk
  * uim-dict-gtk3
    * Fix compilation with Canna >= 3.7

  * fileio.scm
    * Fix for files with no newline at the end

### Enhancements

  * uim-byeoru
    * Speedup in conversion by change of system dictionary format
    * Introduce a new way to define a personal dictionary

  * uim-tutcode
    * Show stroke help temporarily by keys in kanji combination guide
    * Support bushu composition which consists of three or more bushu
      on auto help
    * Use bushu.help file on auto help and bushu conversion

  * uim-toolbar-gtk3-systray
  * uim-toolbar-gtk-systray
    * gnome-shell support

  * uim-notify
    * Support dynamic reconfiguration of the agent selection

### Others

  * Make uim functional when --disable-notify is set at configure

## 1.7.0 - 2011-05-17

### Fixes

  * Generic IM
    * Fix sort order of Wubi86 and Zhengma tables in 1.7.0-alpha

  * GTK+3 bridge
    * Avoid repeated changes of subwindow size in some annotation text

## 1.7.0-alpha - 2011-05-10

### New features

  * GTK+3 bridge
  * uim-candwin-gtk3
  * uim-candwin-tbl-gtk3
  * uim-dict-gtk3
  * uim-im-switcher-gtk3
  * uim-input-pad-ja-gtk3
  * uim-pref-gtk3
  * uim-toolbar-gtk3
  * uim-toolbar-gtk3-systray
    * Support GTK+3

  * uim-toolbar-applet-gnome3
    * Support GNOME3

### Fixes

  * uim Scheme library
    * Fix im-acquire-text brokeness

  * uim-toolbar-qt4
    * Fix bug: Can't set starting position of Qt4 toolbar (#34034)

  * uim-notify
    * Fix compilation with libnotify >= 0.7.0

  * uim-tutcode
    * Fix behavior with interactive bushu conversion

  * uim-skk
    * Fix crash upon an illegal line in SKK dictionary

  * Emacs bridge
    * Don't disturb other leim's modelines (#35318)

  * Qt4 bridge
    * Fix crash of applications upon exit when widget style of
      QGtkStyle is used

  * uim-dict-gtk
    * Fix a minor bug

### Enhancements

  * uim-tutcode
    * Postfix mazegaki conversion
    * Postfix bushu conversion
    * Add inflection support for postfix and prefix mazegaki conversion
    * Kanji code (JIS/Kuten/Unicode) input mode
    * History input mode

  * libuim
    * Support locale with EUC-JIS-2004 and EUC-JISX0213 encodings

  * generic-predict
    * Add support for completion with Google Suggest

### Others

  * GTK+2 bridge
    * Discard support for version below 2.14.

## 1.6.1 - 2011-01-07

### Fixes

  * IM management system
    * Fix crash while switching to the IM which has been once used but
      then disabled and re-enabled again by "Enabled input methods"
      widget of uim-pref.

  * uim-skk
    * Don't crash with malformed words having brackets which are not
      intended for skk-henkan-strict-okuri-precedence.

  * libuim-skk.so
    * Fix possible contamination of dictionary files of SKK-JISYO and
      tutcode's mazegaki dictionary when user switches between uim-skk
      and uim-tutcode dynamically.

### Enhancements

  * uim-tutcode
    * Interactive bushu conversion
    * Postfix bushu conversion in yomi of mazegaki.
    * Completion
    * Input prediction in mazegaki conversion
    * Input prediction in bushu conversion
    * Kanji combination guide on completion and input prediction
    * Another symbol (kigou) input mode like "KI"
    * Add function to redisplay last auto help
    * Show kanji on auto help
    * Show hint kanji for intermediate sequence on virtual keyboard

  * uim-pref
    * Add configuration 'uim-pref-suppress-dot-uim-warning-dialog?'
      (Set this variable #t in ~/.uim if you'd like to suppress the
      dialog)

  * uim.el
    * Support Emacs24 (by Kan-Ru Chen, #31539)

### Others

  * Enable compilation even when the home directory of chrooted build
    environment is non-writable (e.g. nobody user)

  * tcode, trycode, and hangul modules are no longer enabled by
    default.  Use more sophisticated tutcode and byeoru modules
    instead.

## 1.6.0 - 2010-08-11

### Fixes

  * Fix potential duplication of items in enabled-im-list

  * uim-chardict-qt4
    * Fix font settings

### Enhancements

  * Add indicator icons for toolbar with dark background

### Others

  * Qt4 bridge and tools
    * Don't strip binaries

## 1.6.0-beta - 2010-08-05

### Fixes

  * Generic IM
    * Fix crash in some cases when generic-show-prediction-candidates?
      is #f

  * uim-prime
  * uim-annotation-filter
    * Fix crash when command is not found

  * uim-tutcode
    * Fix some recursive learning mode behavior
    * Show correct auto help sequences when tutcode-rule-userconfig is
      used

  * uim-look
    * Fix crash when dictionary file is not found

  * uim-toolbar-{gtk,gtk-systray,applet}
    * Fix freezing toolbar when tool buttons are clicked

  * uim-pref-qt4
    * Fix compilation error due to the wrong version of uic is used in
      some environments

### Enhancements

  * uim-tutcode
    * Change to be able to purge mazegaki entry which is only one
      candidate

  * uim-prime
    * Enable customization of prime command path

### Others

  * Run unittest in test directory only when --enable-debug=yes and
    required tools (Gauche and GaUnit) are installed

  * Use --disable-kde4-applet by default in the configure script

  * Fix compilation on Solaris 10

## 1.6.0-alpha - 2010-07-28

### New features

  * uim-social-ime (new IM)
    A web based Japanese multi-segment input method

  * uim-ajax-ime (new IM)
    A web based Japanese single-segment input method

  * uim-yahoo-jp (new IM)
    A web based Japanese multi-segment input method

  * Generic IM
    * Add Wubi86 and Zhengma tables

  * uim-annotation
    Annotation system

  * generic-predict
    Generic completion input support in Japanese multi-segment IMs
    (used in AjaxIME, Canna, SJ3, Social-IME, Wnn, Yahoo-JP).

  * Qt4 bridge
    * --enable-qt4-qt3support option
    * Support for preedit color configuration
    * A caret state indicator

  * Qt4 bridge
  * Gtk+ bridge
  * uim-candwin-tbl-gtk
    * Table style candidate window

### Enhancements

  * uim-elatin
    * LaTeX-like rules
    * preedit completion

  * Japanese multi-segment IMs
    * Support ACT and KZIK input rules
    * Support user configuration to keep unmatched consonant characters
      in Romaji input

  * Generic IM
    * Add an option to show input prediction with expected key sequences

  * uim-im-switcher
    * Don't run multiple switcher instances in a desktop (#2112)

  * uim-tutcode
    * Register/purge entry in Mazegaki dictionary
    * Alphabet conversion (SKK abbrev) mode
    * Virtual keyboard help
    * Auto help
    * Add kanji to kigoudic

  * uim-trycode
    * Updated the composition table to 2010-04-27 version

### Fixes

  * Qt4 bridge
    * Fix bug: Unwanted IM reset on focus-out of Qt4 immodule (#13910)
    * Fix bug: Candidate window does not follow main window move (#13911)

  * uim-toolbar-gtk-systray
    * Fix transparent and pixmap background with latest gnome-panel (#28999)

### Others

  * testing framework update (requires GaUnit 0.1.6)
  * Update SigScheme to 0.8.5 to support multiple load paths with
    LIBUIM_SCM_FILES.

## 1.5.7 - 2009-11-22

### New features

  * uim-chardict-qt4
  * uim-toolbar-qt4
  * uim-pref-qt4
  * uim-candwin-qt4
  * uim-applet-kde4
    * Experimental support of Qt4 tools by Muneyuki Noguchi (#24123,
      #24620)

### Enhancement

  * uim-toolbar-gtk-systray
    * Support transparent background

### Fixes

  * GTK+ bridge
    * Fix crash in combination with Firefox and flashplayer plugin
      (#25139)

  * uim-fep
    * Fix compilation on NetBSD current ([uim-ja 148])

  * uim.el
    * Fix bug: uim.el doesn't work when linum-mode is activated and
      the inline candidates displaying mode is enabled ([uim-en 245])
    * Fix bug: Some special keys (such as arrow keys) cannot be used
      when uim.el is enabled on emacs23 with -nw option.

  * Qt4 bridge
    * Don't enable knotify3 when qt4-immodule is enabled in configure
      time (#19877)
    * Fix compilation when srcdir != builddir

  * uim-notify
    * Disable uim-notify on build phase to prevent dependency library
      mismatch

  * uim-chardict-qt
    * Install translation file properly ([uim-ja 150])

## 1.5.6 - 2009-06-23

### Enhancement

  * uim-dict-gtk
    * Support UTF-8 characters with Anthy personal dictionary

### Fixes

  * XIM bridge (uim-xim)
    * Fix segfault with latest X.Org
    * Fix segfault with PRIME

### Others

  * Apply build fix from Fedora

## 1.5.5 - 2009-01-21

### Enhancements

  * uim-anthy
  * uim-anthy-utf8
    * Learn transposing Katakana/Hiragana string (bug #18630)

  * misc Japanese IM
    * AZIK roma-kana table update
    * Don't use "nwa", "nwi", "nwu", "nwe", "nwo" roma-kana sequences
      by default

  * uim-pref-gtk
  * uim-pref-qt
    * Reduced startup time

### Fixes

  * GTK+ bridge
  * uim.el
  * uim-skk
  * uim-look
    * Plug memory leaks

  * uim-dict-gtk
    * Fix warning with GTK+-2.14.x

## 1.5.4 - 2008-10-24

### Enhancements

  * Generic IM
    * Cache candidates to improve the performance
    * Add an option to show candidate window automatically when
      multiple candidates are found

  * uim-tutcode
    * New symbol (kigou) input mode

  * GTK+ bridge
  * Qt{3,4} bridge
  * XIM bridge
    * Improve the performance upon activating the window with large
      number of candidates

### Fixes

  * uim.el
    * Plug memory leak.
    * Ignore 'd' command if valid uim-default-im-engine is defined

  * uim-fep
    * Fix compilation with Solaris' cc

  * uim-skk
    * Fix handling in non-matching upper case character in
      skk-proc-state-direct.

  * uim-pref-gtk
    * Fix warning with GTK+-2.14.x

## 1.5.3 - 2008-09-07

### Fixes

  * Qt4 bridge (experimental)
    * [CRITICAL] Fix bug with Qt4 >= 4.4.1, which makes applications
      infinite loop at the startup
    * [CRITICAL] Fix uim-helper handling which could cause unexpected crashes
    * Fix key press with shift, control, alt, and meta keys

  * uim-xim
    * Fix some minor protocol errors

  * uim.el
    * Improve minibuffer editing

  * uim-tutcode
    * Fix out-of-range error when candidates are longer than
      tutcode-heading-label-char-list

### Others

  * uim-toolbar-gtk-systray
    * Adjust ythickness of the buttons

  * uim-pref-gtk
    * Add vertical scrollbar for the setting pane

## 1.5.2 - 2008-08-03

### Enhancements

  * Enable input method rotation by hotkeys

  * uim-trycode
    * Updated the composition table to 2008-04-18 version

### Fixes

  * uim.el
    * [CRITICAL] Fix breaking minor-mode-map-alist in some cases.
    * [CRITICAL] Fix uim-el-helper-agent
    * Fix displaying candidates when a right-hand neighbor
       character of the cursor is a horizontal-tab

  * uim-xim
    * Fix Japanese kana-keyboard input hack to distinguish 'kana_RO'
      key and backslash key

  * uim-canna
    * Don't disable libuim functionality even when canna server is
      stopped.

  * uim-elatin
    * Fix problem when only elatin IM is loaded.

  * uim-prime
    * Fix crash when input mode is forced to set as app-mode by
      toolbar menu while "Enable vi-cooperative mode" is off.

## 1.5.1 - 2008-05-17

### Fixes

  * uim Scheme library
    * [CRITICAL] Fix unescaped strings in generated loader.scm which
      causes uim initialization error on m17n-vi-han (bug #15901)

  * uim-pref-qt
    * [CRITICAL] Fix improper uim initialization which causes
      uim-pref-qt unworking (bug #15853)

## 1.5.0 - 2008-05-06

### Fixes

  * Fix absense of 'uim' feature provision in early initialization stages

## 1.5.0-beta2 - 2008-04-27

### Enhancements

  * uim.el
    * Improve candidates displaying on small window
    * Improve candidates displaying when resizing of the echo region
      is limited
    * Improve buffer scrolling

  * Update translations

### Fixes

  * uim.el
    * Fix serious bug and a related problem:
      When several buffers on different frames display preedits at the
      same time and user switches the focus from some frame to another
      frame, the buffers are crashed.

  * Qt4 bridge (experimental)
    * Fix X11 compose input support

  * Build problems
    * Fix stdint definitions on Debian etch and gcc4
    * Fix 'make check' failure in po caused by intltool 0.36.2
    * Update SigScheme to 0.8.3 to fix indifinite 'make check' failure
      on GC tests

  * uim-wnn
    * Fix misused arguments of jl_kanji_len()
    * Fix inappropriate message output to stdout that confuses uim.el

  * uim-notify
    * Fix buffer size calculation

## 1.5.0-beta - 2008-04-06

### New features

  * Provide 'uim' on Scheme (See "Feature provision of 'uim'" of
    doc/COMPATIBILITY for further information)

### Fixed

  * uim-wnn
    * Check file existence of rcfile
    * Set correct place of WNNLIBDIR when --without-wnn-libraries

  * uim-sh
    * Fix falling into eval loop after script execution

  * Update translations

## 1.5.0-alpha - 2008-03-29

### New features

  * uim-wnn (new IM)
    A Japanese multi-segment input method

  * uim-sj3 (new IM)
    A Japanese multi-segment input method

  * uim-elatin (new IM)
    Emacs-style Latin characters input method

  * uim-look (new IM)
    Dictionary-based completion input method

  * uim-anthy-utf8 (new IM)
    UTF-8 -based uim-anthy

  * uim-tutcode
    * Updated the composition table
    * Bushu composition
    * Mazegaki conversion with learning capability
    * Candidate window support
    * Commit by label key
    * Dvorak keyboard support

  * uim-trycode
    * Updated the composition table to 2007-12-10 version

  * uim-skk
    * IPv6 skkserver support

  * uim-byeoru
    * Korean-Chinese dictionary newly includes word table in addition to
      character information
    * Added a 2-beol layout which works like MS-Windows IME, i.e.,
      it maps an undefined shifted key to a jamo rather than an alphabet
      (ChangBeom Yoon)
    * a 2-beol layout can enter a compound jongseong

  * uim-m17nlib
    * 'uim-m17nlib-relink-icons' command to import icons from m17n-db

  * Qt4 bridge (experimental)
    * Basically working but still having some known bugs (#13910, #13911)

  * GTK+ bridge
  * Qt3 bridge
  * XIM bridge
    * Japanese kana-keyboard input hack to distinguish 'kana_RO' key
      and backslash key

  * uim-notify (experimental)
    uim to user message notification system

  * Emergency disablement of all uim functionality on fatal errors to
    protect user application data

  * uim-sh
    * Accept script file and arguments for SRFI-22 'main' procedure
      after uim-sh options
    * --require-module option (-r for short)
    * --expression option (-e for short)
    * --version option (-V for short)

  * uim-scm
    * SIOD compatibility has been disabled
    * Multibyte character processing
    * Character codec handling procedures
    * All string procedures of R5RS
    * R5RS characters
    * R5RS vectors
    * R5RS promises (delay and force)
    * SRFI-0  Feature-based conditional expansion construct
    * SRFI-1  List Library (full-featured)
    * SRFI-2  An AND with local bindings, a guarded LET* special form
    * SRFI-8  Binding to multiple values
    * SRFI-9  Defining Record Types
    * SRFI-43 Vector library
    * SRFI-55 require-extension
    * SRFI-69 Basic hash tables
    * SRFI-95 Sorting and Merging
    * define-macro
    * let-optionals* for optional argument processing

### Fixed

  * uim-skk
    * Fix showing okuri-ari word with an annotation
    * Don't add preedit-cursor when skk-show-cursor-on-preedit? is #f

  * uim-tcode
    * Fix composition rules ([uim-ja 29])

  * uim-prime
    * Fix to be able to select URL candidate at Firefox URL field

  * uim-m17nlib
    * Fix opening all of registered m17n IM at startup unnecessarily

  * uim-scim
    * Fix compilation failure

  * Japanese composition table
    * Fix some rules
    * Replace fullwidth yen sign for romaji-halfwidth-kana-mode and
      alphanumeric-mode since ordinary Japanese users just expect to
      input character code 134 rather than yen sign symbol

  * uim-sh
    * Fix infinite loop on a non-recoverable read error such as
      non-ASCII char in token on a non-Unicode port
    * Fix broken editline feature by SIOD dependencies
    * Fix EOF (^d) ignorance on editline feature is enabled
    * Fix ignorance of multiple expressions on a line on editline
      feature is enabled
    * Fix read error on multi-line expression such as
      "(define\nfoo\n3)" on editline feature is enabled
    * Fix pseudo -r (require module) option for editline feature with
      real require-module

  * GTK+ bridge
    * Fix broken gtk.immodules installation depending on GTK+ location

  * uim-fep
    * Fix the unupdated IM-name of the status line on IM-switching by
      an external IM switcher
    * Fix hardcoded temporary directory location

  * uim.el
    * Fix function-key-map handling
    * Fix cursor position bug caused by using uim-input-pad-ja
    * Fix font-face configuration problem
    * Fix buffer destroying bug caused by using proportional fonts with
      inline candidate display mode
    * Support event-apply-*-modifier
    * Support delete-selection-mode
    * Support Emacs-23.x experimentally
    * Support shifted-motion-keys-select-region feature of XEmacs
    * Support the commands terminated by help-char on XEmacs
    * Support Multi_key on XEmacs
    * Support some minor keysyms
    * Support horizontally scrolled buffer

### Others

  * Split libuim-scm off from libuim
  * Various API and ABI changes (see doc/COMPATIBILITY)

## 1.4.2 - 2008-02-20

### Fixed

  * uim Scheme library
    * Fix memory alignment of symbol name, which can be a problem
      with jemalloc in FreeBSD 7 and Firefox 3
    * Fix string buffer usage
    * Fix possible memory leak
    * Fix compilation on Mac OS X 10.5
    * Support OpenBSD PowerPC and x86_64 in libgcroots

  * libuim
    * Fix leak in file descriptors when uim-helper-server is not
      working (Christian Biere)

  * uim-prime
    * Handle unix domain socket timeout

  * uim-m17nlib
    * Fix listing incomplete IMs

  * Japanese composition table
    * Fix fullwidth middle dot in halfwidth katakana table for
      Japanese kana-keyboard

  * GTK+ bridge
    * Fix token buffer usage of X11 Compose
    * Fix freeing strings allocated by glib
  * Qt bridge
    * Fix token buffer usage of X11 Compose
  * XIM bridge
    * Fix token buffer usage of X11 Compose
    * Fix compilation with GCC-4.3

  * uim-toolbar-gtk-systray
    * Adjust focus padding in systray

## 1.4.1 - 2007-03-14

### Fixed

  * libuim
    * Fix crash in a context using toggled IM when updating the custom
      setting by uim-pref

  * Anthy, Canna, Mana
    * Fix crash with Kana input mode
    * Fix crash upon converting a preedit string into alphanumeric
      characters

  * Pinyin (Simplified, Unicode, Traditional)
    * Fix crash upon selecting a candidate with numeric keys

  * uim Scheme library
    * Support SIOD compatible behavior with syntax 'if'

  * Fix compilation in qt directory with --disable-nls option

## 1.4.0 - 2007-01-13

### Fixed

  * Fix the license terms of replace/strsep.c and replace/setenv.c
    accidencially mis-modified in r790

  * libuim
    * Fix unimplemented return value of uim_input_string()
    * Fix uneffected --disable-nls

  * uim Scheme library
    * Fix lacking heap preallocation for heavy job. This makes uim-pref startup
      faster

## 1.4.0-beta - 2007-01-06

### Fixed

  * XIM bridge
    * Startup error of uim-xim due to the 1.3.x-incompatible behavior of
      uim_scm_symbol_value()

  * libuim
    * Fix corrupted return value by uim_scm_return_value() after uim_scm_eval()

  * uim-sh
    * Fix broken 'uim-sh -B' error handling
    * Fix broken backtrace printing

  * Broken compilation on SPARC and IA-64 (NIIBE Yutaka)
  * Suppress warnings

## 1.4.0-alpha - 2006-12-29

### New features

  * libuim
    * The modified version of SIOD interpreter has been replaced with SigScheme
      0.7.2

### Fixed

  * [CRITICAL] Fix GC crash on SPARC, IA-64, m68k, and possibly MIPS, Alpha, by
    introduction of libgcroots

## 1.3.1 - 2006-12-22

### Fixes

  * SKK
    * Fix specific key handling in wide-latin direct input mode

  * uim-pref-qt
    * Fix compilation on non-immodule version of Qt3

### Others

  * uim-toolbar-{gtk,gtk-systray,applet}
    * Avoid potential misbehavior in property activation by popup menu
  * uim_panelapplet
    * Improve layout upon resize of the KDE panel
  * uim-candwin-qt
    * Handle disconnect from uim-xim sanely

## 1.3.0 - 2006-12-15

### Fixed

  * XIM bridge
    * Fix compilation on pre-X11R6.7 environment.

## 1.3.0-beta - 2006-12-08

### Fixed

  * libuim
    * Minor fix for text acquisition and injection
    * Add caps-lock, scroll-lock, and num-lock keysyms

  * Tcode
  * Try-code
    * Update compose table

  * uim-toolbar-qt
    * Suppress runtime warning when icon files are missing

  * GTK+ bridge
  * Qt bridge
  * XIM bridge
    * Support caps-lock, scroll-lock, and num-lock keysyms
    * Fix page shift behavior

## 1.3.0-alpha - 2006-12-01

### New features

  * libuim
    * Obsolete experimental surrounding text API
    * Add text acquisition API and support it in GTK+ and Qt bridges
    * Add text injection API (uim_input_string())
    * Support focus-in and focus-out handler in IM context
    * More keysyms support
    * See doc/COMPATIBILITY for further details

  * Anthy
    * Kana keyboard support (experimental)
  * Latin IM
    * Support deadkeys with preedit enabled
  * Try-Code IM
    * New Japanese IM based on T-Code

  * uim_panelapplet
    * New toolbar applet for KDE panel
  * uim-toolbar-gtk
    * Double click to shrink the window (Masatake Yamato)
  * uim-toolbar-qt
    * Double click to shrink the window

  * uim-pref-gtk
  * uim-pref-qt
    * Support more keysyms in key editor

  * GTK+ bridge
  * Qt bridge
  * XIM bridge
    * More keysyms support

### Fixes

  * Prime
    * Fix key handling in word registration mode (bug #8984)
  * Tcode
    * Update compose table
  * Generic (Tcode, Hangul, PY)
    * Fix key handling

  * uim-toolbar-gtk
    * More icons (Shun-ichi TAHARA)
  * uim-toolbar-qt
    * More icons
    * Use smaller toolbar icons
    * Fix in movement with mouse pointer

  * GTK+ bridge
    * Use workaround for gtk_im_context_reset()
  * Qt bridge
    * Fix for preedit relocation with Japanese IM
  * XIM bridge
    * Fix for java applications with incorrect font setting
    * Workaround for Flash Player 9 plugin

### Others

  * Input method icons
    * Replace with inputmethod-icons 1.1.1
  * uim-module-manager
    * Set newly registering module names to the head of IM list
  * uim-input-pad-ja
    * Reorginize pad buttons (Shun-ichi TAHARA)

## 1.2.1 - 2006-08-14

### Fixes

  * Prime
    * Sync prime.scm with prime-1.0.0.1.
    * Improve connection using unix domain socket.
    * Use unix domain socket connection by default.

  * uim-candwin-qt
    * Fix displaying candidates in input prediction.

  * Qt immodule
    * Fix displaying candidates in input prediction.
    * Fix key handling with control modifier.

## 1.2.0 - 2006-08-04

### Fixes

  * Minor
    * Prime
      * Fix initialization of the context upon IM switch (Konosuke).

    * uim-pref-qt
      * Handle Meta modifier.
    * uim-toolbar-qt
      * Fix to shrink toolbar when any of the buttons are removed.

    * uim.el
      * Fix displaying candidates in input prediction (Konosuke).
      * Fix displaying candidates with appendix on uim-prime (Konosuke).

### Others

  * Bump libuim version.

## 1.2.0-beta - 2006-07-28

### Fixes

  * Minor
    * Anthy
    * Canna
    * Mana
      * Commit appropriate space letter in halfwidth_alnum input mode.

    * Qt immodule
      * Fix getting default IM in some environments.

## 1.2.0-alpha - 2006-07-21

### New features

  * Anthy
    * Input prediction support.
    * Half-width and full-width alphanumeric conversion with a specific
      segment.
    * Add half-width alphanumeric input mode.
    * Change definition of some input-mode transition keys.

  * Canna
  * Mana
    * Hiragana, katakana, half-width katakana, half- and full-width
      alphanumeric conversion with a specific segment.
    * Add half-width alphanumeric input mode.
    * Change definition of some input-mode transition keys.

  * uim-dict-gtk
    * Support Canna private dictionary.

  * Gtk+ immodule
  * Qt immodule
    * Support locale dependent Compose file and ~/.XCompose

### Fixes

  * Anthy, Canna, Mana
    * Don't insert full-width space on half-width katakana mode (bug #502).

  * uim-xim
    * Support ja (EUC) locale in Solaris 10 (bug #7558, KIHARA Hideto).

  * general
    * Handle several input mode keys in Japanese keyboard.

## 1.1.1 - 2006-07-07

### Fixes

  * Anthy
  * Mana
    * Fix hang in latin conversion with incomplete roma-kana sequence,
      but the behavior is still corrupted and won't fix in 1.1 series.
  * Canna
    * Fix key bindings for kana input rule.
    * Improve preedit handing on direct mode.
    * Fix IM context reset.
    * Don't hang if Canna server close the connection.

  * uim.el
    * Reflect updated custom preferences on-the-fly even without focus.

  * uim-pref-qt
    * Enable to remove all the key bindings in a setting.
  * uim-toolbar-gtk
  * uim-toolbar-gtk-systray
  * uim-toolbar-applet
    * Fix the position of the menu if it is too large.

## 1.1.0 - 2006-06-09

### Fixes

  * Critical
    * libuim
      * fix a GC corruption that causes SEGV (YamaKen)

  * Minor
    * Qt immodule
      * proper deadkey and compose key handling (bug #3988, Etsushi)
      * adapted to the menu-based IM switcher (Etsushi)

    * m17n-lib IM
      * fix key handling with modifier keys (Etsushi)

### Others

  * tidy up automake/autoconf related scripts

## 1.1.0-beta - 2006-06-02

### New features

  * Minor
    * Anthy
      * hankaku katakana conversion with Anthy-7802 (Etsushi)

### Fixes

  * Minor
    * uim-toolbar-gtk
      * prevent startup hang in a rare case (Etsushi)

    * Latin IM
      * update table (Etsushi)

## 1.1.0-alpha - 2006-05-26

### New features

  * Major
    * icon support for toolbar (Etsushi, YamaKen)
    * menu-based quick IM switching from toolbar (YamaKen, Etsushi)

  * Minor
    * hotkey-based IM toggle has been able to hold per-context state
      (YamaKen)

  * IM related
    * Anthy, Canna, Mana
      * circular Hiragana, Katakana, Hankaku-kana conversion with
        "Muhenkan" key (Shun-ichi TAHARA)
    * Mana
      * support conversion-result learning with mana-0.2.0 (Shun-ichi
        TAHARA, Masanari)

### Fixes

  * Minor
    * general
      * handle 'Kanji' and 'Hiragana_Katakana' keys (Shun-ichi TAHARA)

    * uim-dict-gtk
      * refresh the word list when removing and updating the existing
        items (Etsushi)
    * uim-im-switcher-gtk
      * respond to the cursor movement from keyboard (Etsushi)
    * uim-pref-gtk
      * handle \<Meta>, \<Super>, and \<Hyper> modifiers in the key
        capture widget (Etsushi)
      * distinguish directories and regular files on pathname custom
        widget (Etsushi, YamaKen)
    * uim-pref-qt
      * distinguish directories and regular files on pathname custom
        widget (Etsushi, YamaKen)

    * GTK+ immodule
      * enable \<Alt>, \<Meta>, \<Super>, and \<Hyper> modifier key
        handling on X11 (bug #2468, Etsushi)
      * disuse key snooper to improve shortcut key handling (Etsushi)
    * uim-xim
      * fix event handling with flashplayer plugin (Etsushi)
    * uim-fep
      * fix "commit_string" handling and make uim-input-ja working
        again (Konosuke, Masanari)
    * uim.el
      * handle "commit_string" from helper programs and make
        uim-input-ja working (Konosuke)

### Others

  * improve code properness with -Werror flag

  * contributed and new input method icons (SHIMODA Hiroshi, FUJITA
    Yuji, Jae-hyeon, YamaKen)

  * new icons for input state indicator in the toolbar (YamaKen,
    Jae-hyeon)

  * strip IM name multiplexed in iconic-label of input modes of skk and
    tutcode that confuses users (YamaKen)

  * update translations
    * ja (Daichi Kawahata, YamaKen)
    * ko (Jae-hyeon)

## 1.0.1 - 2005-12-24

### Fixes

  * Minor
    * uim-xim
      * Workaround fix for preventing SEGV if compiled with g++-4.x.

## 1.0.0 - 2005-12-23

### Fixes

  * Minor
    * uim.el
      * Sync with uim-el 0.0.6-beta4
    * gtk+ immodule
      * fix SEGV with gvim when the caret state indicator is enabled
        with a timeout value (Etsushi)
      * workaround fix for preedit handling on gvim when use-with-vi?
        is enabled with anthy and mana (Masanari, YamaKen, Etsushi)
    * SKK
      * improve completion behavior (Masanari, Etsushi)

## 1.0.0-beta - 2005-12-16

### New features

  * IM related
    * SKK
      * enable word completion with UNIX look command (Etsushi)

### Fixes

  * Critical
    * uim-pref-qt
      * fix behavior of custom widget with integer number when the
        value in the stored file is greater than 99 (Etsushi)

  * Minor
    * update configure script (Etsushi)
      * don't require XML::Parser
      * more strict check for canna
    * Qt immodule
      * fix to respond to uim-pref-gtk's custom messages on the fly in
        addition to uim-pref-qt's (Etsushi)
    * uim-pref-qt
      * code cleanup (Kazuki)

    * Canna
      * fix behavior of backspace key in conversion state (Etsushi)

  * Others
    * Update translations

## 1.0.0-alpha - 2005-12-09

### New features

  * Main features
    * uim.el (bridge for Emacs) (Konosuke)
    * byeoru Hangul input suite (Jae-hyeon)
    * mana Japanese input (Masanari)

  * IM related
    * toggle-im facility (Etsushi, YamaKen)
    * Anthy
      * vi-cooperative mode (Masanari)
    * Canna
      * vi-cooperative mode (Masanari)
    * SKK (Etsushi)
      * dynamic completion
      * ddskk compatible behavior with mistypes while starting
        kanji-mode, e.g. "kAnnji" for "Kannji"
      * improved numeric conversion
      * support commit with opposite case in latin-conversion
      * support completion with conversion and/or commit at once

### Fixes

  * Critical
    * fix segmentation fault if uim is compiled with '--enabled-debug'
      (Etsushi, dai \<d+r AT vdr.jp>)
    * fix IPC (connection to PRIME) in some rare cases (Masanari)
    * fix possible GC crash with anti-stack smashing compilers such as
      GCC4 (Jun, YamaKen)

  * Minor
    * preedit reset with IM switch (Masanari)
    * some build fixes (Masanari)

    * gtk+ immodule
      * disable warnings (Etsushi)
    * uim-xim
      * support changes in X modifier keys on-the-fly (Etsushi)

    * Anthy
      * improve kana-input mode (Masanari)
    * SKK
      * improve completion behavior (Etsushi)
      * improve skkserv connection (Masahito, Etsushi)
    * PRIME
      * proper custom setting (YamaKen)

    * uim-im-switcher-qt
      * show list of IM properly (Etsushi)
    * uim-pref-qt
      * compatible behavior with uim-pref-gtk (Etsushi)

### Others

  * obsolete table API
  * translations
    * add ko (Jae-hyeon)
    * update ja (Daichi Kawahata, YamaKen)
