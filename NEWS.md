# NEWS

## 1.9.4 - 2025-05-06

### Improvements

  * Suppressed `incompatible-pointer-types` warnings.
    * Reported by SteelDynamite

  * Updated bundled SigScheme to 0.9.3.

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
