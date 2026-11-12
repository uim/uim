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
