# NEWS

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
