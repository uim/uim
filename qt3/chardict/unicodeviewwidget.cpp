/*

Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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
#include "unicodeviewwidget.h"
#include "chargridview.h"

#include <qlayout.h>
#include <qvbox.h>
#include <qlabel.h>
#include <qheader.h>
#include <qsettings.h>

#include "qtgettext.h"

class UnicodeBlock
{
public:
    UnicodeBlock( const QString &name, uint startHex, uint endHex )
    {
        m_name = name;
        m_startHex = startHex;
        m_endHex = endHex;
    }

    QString getName() const { return m_name; }
    uint getStartHex() const { return m_startHex; }
    uint getEndHex() const { return m_endHex; }

protected:
    QString m_name;
    uint m_startHex;
    uint m_endHex;
};

static UnicodeBlock uBlocks[] = {
            UnicodeBlock( NC_( "chardict", "Basic Latin" ), 0x0000, 0x007F ),
            UnicodeBlock( NC_( "chardict", "Latin-1 Supplement" ), 0x0080, 0x00FF ),
            UnicodeBlock( NC_( "chardict", "Latin Extended-A" ), 0x0100, 0x017F ),
            UnicodeBlock( NC_( "chardict", "Latin Extended-B" ), 0x0180, 0x024F ),
            UnicodeBlock( NC_( "chardict", "IPA Extensions" ), 0x0250, 0x02AF ),
            UnicodeBlock( NC_( "chardict", "Spacing Modifier Letters" ), 0x02B0, 0x02FF ),
            UnicodeBlock( NC_( "chardict", "Combining Diacritical Marks" ), 0x0300, 0x036F ),
            UnicodeBlock( NC_( "chardict", "Greek and Coptic" ), 0x0370, 0x03FF ),
            UnicodeBlock( NC_( "chardict", "Cyrillic" ), 0x0400, 0x04FF ),
            UnicodeBlock( NC_( "chardict", "Cyrillic Supplement" ), 0x0500, 0x052F ),
            UnicodeBlock( NC_( "chardict", "Armenian" ), 0x0530, 0x058F ),
            UnicodeBlock( NC_( "chardict", "Hebrew" ), 0x0590, 0x05FF ),
            UnicodeBlock( NC_( "chardict", "Arabic" ), 0x0600, 0x06FF ),
            UnicodeBlock( NC_( "chardict", "Syriac" ), 0x0700, 0x074F ),
            UnicodeBlock( NC_( "chardict", "Arabic Supplement" ), 0x0750, 0x077F ),
            UnicodeBlock( NC_( "chardict", "Thaana" ), 0x0780, 0x07BF ),
            UnicodeBlock( NC_( "chardict", "NKo" ), 0x07C0, 0x07FF ),
            UnicodeBlock( NC_( "chardict", "Samaritan" ), 0x0800, 0x083F ),
            UnicodeBlock( NC_( "chardict", "Mandaic" ), 0x0840, 0x085F ),
            UnicodeBlock( NC_( "chardict", "Arabic Extended-A" ), 0x08A0, 0x08FF ),
            UnicodeBlock( NC_( "chardict", "Devanagari" ), 0x0900, 0x097F ),
            UnicodeBlock( NC_( "chardict", "Bengali" ), 0x0980, 0x09FF ),
            UnicodeBlock( NC_( "chardict", "Gurmukhi" ), 0x0A00, 0x0A7F ),
            UnicodeBlock( NC_( "chardict", "Gujarati" ), 0x0A80, 0x0AFF ),
            UnicodeBlock( NC_( "chardict", "Oriya" ), 0x0B00, 0x0B7F ),
            UnicodeBlock( NC_( "chardict", "Tamil" ), 0x0B80, 0x0BFF ),
            UnicodeBlock( NC_( "chardict", "Telugu" ), 0x0C00, 0x0C7F ),
            UnicodeBlock( NC_( "chardict", "Kannada" ), 0x0C80, 0x0CFF ),
            UnicodeBlock( NC_( "chardict", "Malayalam" ), 0x0D00, 0x0D7F ),
            UnicodeBlock( NC_( "chardict", "Sinhala" ), 0x0D80, 0x0DFF ),
            UnicodeBlock( NC_( "chardict", "Thai" ), 0x0E00, 0x0E7F ),
            UnicodeBlock( NC_( "chardict", "Lao" ), 0x0E80, 0x0EFF ),
            UnicodeBlock( NC_( "chardict", "Tibetan" ), 0x0F00, 0x0FFF ),
            UnicodeBlock( NC_( "chardict", "Myanmar" ), 0x1000, 0x109F ),
            UnicodeBlock( NC_( "chardict", "Georgian" ), 0x10A0, 0x10FF ),
            UnicodeBlock( NC_( "chardict", "Hangul Jamo" ), 0x1100, 0x11FF ),
            UnicodeBlock( NC_( "chardict", "Ethiopic" ), 0x1200, 0x137F ),
            UnicodeBlock( NC_( "chardict", "Ethiopic Supplement" ), 0x1380, 0x139F ),
            UnicodeBlock( NC_( "chardict", "Cherokee" ), 0x13A0, 0x13FF ),
            UnicodeBlock( NC_( "chardict", "Unified Canadian Aboriginal Syllabics" ), 0x1400, 0x167F ),
            UnicodeBlock( NC_( "chardict", "Ogham" ), 0x1680, 0x169F ),
            UnicodeBlock( NC_( "chardict", "Runic" ), 0x16A0, 0x16FF ),
            UnicodeBlock( NC_( "chardict", "Tagalog" ), 0x1700, 0x171F ),
            UnicodeBlock( NC_( "chardict", "Hanunoo" ), 0x1720, 0x173F ),
            UnicodeBlock( NC_( "chardict", "Buhid" ), 0x1740, 0x175F ),
            UnicodeBlock( NC_( "chardict", "Tagbanwa" ), 0x1760, 0x177F ),
            UnicodeBlock( NC_( "chardict", "Khmer" ), 0x1780, 0x17FF ),
            UnicodeBlock( NC_( "chardict", "Mongolian" ), 0x1800, 0x18AF ),
            UnicodeBlock( NC_( "chardict", "Unified Canadian Aboriginal Syllabics Extended" ), 0x18B0, 0x18FF ),
            UnicodeBlock( NC_( "chardict", "Limbu" ), 0x1900, 0x194F ),
            UnicodeBlock( NC_( "chardict", "Tai Le" ), 0x1950, 0x197F ),
            UnicodeBlock( NC_( "chardict", "New Tai Lue" ), 0x1980, 0x19DF ),
            UnicodeBlock( NC_( "chardict", "Khmer Symbols" ), 0x19E0, 0x19FF ),
            UnicodeBlock( NC_( "chardict", "Buginese" ), 0x1A00, 0x1A1F ),
            UnicodeBlock( NC_( "chardict", "Tai Tham" ), 0x1A20, 0x1AAF ),
            UnicodeBlock( NC_( "chardict", "Balinese" ), 0x1B00, 0x1B7F ),
            UnicodeBlock( NC_( "chardict", "Sundanese" ), 0x1B80, 0x1BBF ),
            UnicodeBlock( NC_( "chardict", "Batak" ), 0x1BC0, 0x1BFF ),
            UnicodeBlock( NC_( "chardict", "Lepcha" ), 0x1C00, 0x1C4F ),
            UnicodeBlock( NC_( "chardict", "Ol Chiki" ), 0x1C50, 0x1C7F ),
            UnicodeBlock( NC_( "chardict", "Sundanese Supplement" ), 0x1CC0, 0x1CCF ),
            UnicodeBlock( NC_( "chardict", "Vedic Extensions" ), 0x1CD0, 0x1CFF ),
            UnicodeBlock( NC_( "chardict", "Phonetic Extensions" ), 0x1D00, 0x1D7F ),
            UnicodeBlock( NC_( "chardict", "Phonetic Extensions Supplement" ), 0x1D80, 0x1DBF ),
            UnicodeBlock( NC_( "chardict", "Combining Diacritical Marks Supplement" ), 0x1DC0, 0x1DFF ),
            UnicodeBlock( NC_( "chardict", "Latin Extended Additional" ), 0x1E00, 0x1EFF ),
            UnicodeBlock( NC_( "chardict", "Greek Extended" ), 0x1F00, 0x1FFF ),
            UnicodeBlock( NC_( "chardict", "General Punctuation" ), 0x2000, 0x206F ),
            UnicodeBlock( NC_( "chardict", "Superscripts and Subscripts" ), 0x2070, 0x209F ),
            UnicodeBlock( NC_( "chardict", "Currency Symbols" ), 0x20A0, 0x20CF ),
            UnicodeBlock( NC_( "chardict", "Combining Diacritical Marks for Symbols" ), 0x20D0, 0x20FF ),
            UnicodeBlock( NC_( "chardict", "Letterlike Symbols" ), 0x2100, 0x214F ),
            UnicodeBlock( NC_( "chardict", "Number Forms" ), 0x2150, 0x218F ),
            UnicodeBlock( NC_( "chardict", "Arrows" ), 0x2190, 0x21FF ),
            UnicodeBlock( NC_( "chardict", "Mathematical Operators" ), 0x2200, 0x22FF ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Technical" ), 0x2300, 0x23FF ),
            UnicodeBlock( NC_( "chardict", "Control Pictures" ), 0x2400, 0x243F ),
            UnicodeBlock( NC_( "chardict", "Optical Character Recognition" ), 0x2440, 0x245F ),
            UnicodeBlock( NC_( "chardict", "Enclosed Alphanumerics" ), 0x2460, 0x24FF ),
            UnicodeBlock( NC_( "chardict", "Box Drawing" ), 0x2500, 0x257F ),
            UnicodeBlock( NC_( "chardict", "Block Elements" ), 0x2580, 0x259F ),
            UnicodeBlock( NC_( "chardict", "Geometric Shapes" ), 0x25A0, 0x25FF ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Symbols" ), 0x2600, 0x26FF ),
            UnicodeBlock( NC_( "chardict", "Dingbats" ), 0x2700, 0x27BF ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Mathematical Symbols-A" ), 0x27C0, 0x27EF ),
            UnicodeBlock( NC_( "chardict", "Supplemental Arrows-A" ), 0x27F0, 0x27FF ),
            UnicodeBlock( NC_( "chardict", "Braille Patterns" ), 0x2800, 0x28FF ),
            UnicodeBlock( NC_( "chardict", "Supplemental Arrows-B" ), 0x2900, 0x297F ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Mathematical Symbols-B" ), 0x2980, 0x29FF ),
            UnicodeBlock( NC_( "chardict", "Supplemental Mathematical Operators" ), 0x2A00, 0x2AFF ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Symbols and Arrows" ), 0x2B00, 0x2BFF ),
            UnicodeBlock( NC_( "chardict", "Glagolitic" ), 0x2C00, 0x2C5F ),
            UnicodeBlock( NC_( "chardict", "Latin Extended-C" ), 0x2C60, 0x2C7F ),
            UnicodeBlock( NC_( "chardict", "Coptic" ), 0x2C80, 0x2CFF ),
            UnicodeBlock( NC_( "chardict", "Georgian Supplement" ), 0x2D00, 0x2D2F ),
            UnicodeBlock( NC_( "chardict", "Tifinagh" ), 0x2D30, 0x2D7F ),
            UnicodeBlock( NC_( "chardict", "Ethiopic Extended" ), 0x2D80, 0x2DDF ),
            UnicodeBlock( NC_( "chardict", "Cyrillic Extended-A" ), 0x2DE0, 0x2DFF ),
            UnicodeBlock( NC_( "chardict", "Supplemental Punctuation" ), 0x2E00, 0x2E7F ),
            UnicodeBlock( NC_( "chardict", "CJK Radicals Supplement" ), 0x2E80, 0x2EFF ),
            UnicodeBlock( NC_( "chardict", "Kangxi Radicals" ), 0x2F00, 0x2FDF ),
            UnicodeBlock( NC_( "chardict", "Ideographic Description Characters" ), 0x2FF0, 0x2FFF ),
            UnicodeBlock( NC_( "chardict", "CJK Symbols and Punctuation" ), 0x3000, 0x303F ),
            UnicodeBlock( NC_( "chardict", "Hiragana" ), 0x3040, 0x309F ),
            UnicodeBlock( NC_( "chardict", "Katakana" ), 0x30A0, 0x30FF ),
            UnicodeBlock( NC_( "chardict", "Bopomofo" ), 0x3100, 0x312F ),
            UnicodeBlock( NC_( "chardict", "Hangul Compatibility Jamo" ), 0x3130, 0x318F ),
            UnicodeBlock( NC_( "chardict", "Kanbun" ), 0x3190, 0x319F ),
            UnicodeBlock( NC_( "chardict", "Bopomofo Extended" ), 0x31A0, 0x31BF ),
            UnicodeBlock( NC_( "chardict", "CJK Strokes" ), 0x31C0, 0x31EF ),
            UnicodeBlock( NC_( "chardict", "Katakana Phonetic Extensions" ), 0x31F0, 0x31FF ),
            UnicodeBlock( NC_( "chardict", "Enclosed CJK Letters and Months" ), 0x3200, 0x32FF ),
            UnicodeBlock( NC_( "chardict", "CJK Compatibility" ), 0x3300, 0x33FF ),
            UnicodeBlock( NC_( "chardict", "CJK Unified Ideographs Extension A" ), 0x3400, 0x4DB5 ),
            UnicodeBlock( NC_( "chardict", "Yijing Hexagram Symbols" ), 0x4DC0, 0x4DFF ),
            UnicodeBlock( NC_( "chardict", "CJK Unified Ideographs" ), 0x4E00, 0x9FCC ),
            UnicodeBlock( NC_( "chardict", "Yi Syllables" ), 0xA000, 0xA48F ),
            UnicodeBlock( NC_( "chardict", "Yi Radicals" ), 0xA490, 0xA4CF ),
            UnicodeBlock( NC_( "chardict", "Lisu" ), 0xA4D0, 0xA4FF ),
            UnicodeBlock( NC_( "chardict", "Vai" ), 0xA500, 0xA63F ),
            UnicodeBlock( NC_( "chardict", "Cyrillic Extended-B" ), 0xA640, 0xA69F ),
            UnicodeBlock( NC_( "chardict", "Bamum" ), 0xA6A0, 0xA6FF ),
            UnicodeBlock( NC_( "chardict", "Modifier Tone Letters" ), 0xA700, 0xA71F ),
            UnicodeBlock( NC_( "chardict", "Latin Extended-D" ), 0xA720, 0xA7FF ),
            UnicodeBlock( NC_( "chardict", "Syloti Nagri" ), 0xA800, 0xA82F ),
            UnicodeBlock( NC_( "chardict", "Common Indic Number Forms" ), 0xA830, 0xA83F ),
            UnicodeBlock( NC_( "chardict", "Phags-pa" ), 0xA840, 0xA87F ),
            UnicodeBlock( NC_( "chardict", "Saurashtra" ), 0xA880, 0xA8DF ),
            UnicodeBlock( NC_( "chardict", "Devanagari Extended" ), 0xA8E0, 0xA8FF ),
            UnicodeBlock( NC_( "chardict", "Kayah Li" ), 0xA900, 0xA92F ),
            UnicodeBlock( NC_( "chardict", "Rejang" ), 0xA930, 0xA95F ),
            UnicodeBlock( NC_( "chardict", "Hangul Jamo Extended-A" ), 0xA960, 0xA97F ),
            UnicodeBlock( NC_( "chardict", "Javanese" ), 0xA980, 0xA9DF ),
            UnicodeBlock( NC_( "chardict", "Cham" ), 0xAA00, 0xAA5F ),
            UnicodeBlock( NC_( "chardict", "Myanmar Extended-A" ), 0xAA60, 0xAA7F ),
            UnicodeBlock( NC_( "chardict", "Tai Viet" ), 0xAA80, 0xAADF ),
            UnicodeBlock( NC_( "chardict", "Meetei Mayek Extensions" ), 0xAAE0, 0xAAFF ),
            UnicodeBlock( NC_( "chardict", "Ethiopic Extended-A" ), 0xAB00, 0xAB2F ),
            UnicodeBlock( NC_( "chardict", "Meetei Mayek" ), 0xABC0, 0xABFF ),
            UnicodeBlock( NC_( "chardict", "Hangul Syllables" ), 0xAC00, 0xD7A3 ),
            UnicodeBlock( NC_( "chardict", "Hangul Jamo Extended-B" ), 0xD7B0, 0xD7FF ),
            UnicodeBlock( NC_( "chardict", "High Surrogates" ), 0xD800, 0xDB7F ),
            UnicodeBlock( NC_( "chardict", "High Private Use Surrogates" ), 0xDB80, 0xDBFF ),
            UnicodeBlock( NC_( "chardict", "Low Surrogates" ), 0xDC00, 0xDFFF ),
            UnicodeBlock( NC_( "chardict", "Private Use Area" ), 0xE000, 0xF8FF ),
            UnicodeBlock( NC_( "chardict", "CJK Compatibility Ideographs" ), 0xF900, 0xFAFF ),
            UnicodeBlock( NC_( "chardict", "Alphabetic Presentation Forms" ), 0xFB00, 0xFB4F ),
            UnicodeBlock( NC_( "chardict", "Arabic Presentation Forms-A" ), 0xFB50, 0xFDFF ),
            UnicodeBlock( NC_( "chardict", "Variation Selectors" ), 0xFE00, 0xFE0F ),
            UnicodeBlock( NC_( "chardict", "Vertical Forms" ), 0xFE10, 0xFE1F ),
            UnicodeBlock( NC_( "chardict", "Combining Half Marks" ), 0xFE20, 0xFE2F ),
            UnicodeBlock( NC_( "chardict", "CJK Compatibility Forms" ), 0xFE30, 0xFE4F ),
            UnicodeBlock( NC_( "chardict", "Small Form Variants" ), 0xFE50, 0xFE6F ),
            UnicodeBlock( NC_( "chardict", "Arabic Presentation Forms-B" ), 0xFE70, 0xFEFF ),
            UnicodeBlock( NC_( "chardict", "Halfwidth and Fullwidth Forms" ), 0xFF00, 0xFFEF ),
            UnicodeBlock( NC_( "chardict", "Specials" ), 0xFFF0, 0xFFFF ),
            UnicodeBlock( NC_( "chardict", "Linear B Syllabary" ), 0x10000, 0x1007F ),
            UnicodeBlock( NC_( "chardict", "Linear B Ideograms" ), 0x10080, 0x100FF ),
            UnicodeBlock( NC_( "chardict", "Aegean Numbers" ), 0x10100, 0x1013F ),
            UnicodeBlock( NC_( "chardict", "Ancient Greek Numbers" ), 0x10140, 0x1018F ),
            UnicodeBlock( NC_( "chardict", "Ancient Symbols" ), 0x10190, 0x101CF ),
            UnicodeBlock( NC_( "chardict", "Phaistos Disc" ), 0x101D0, 0x101FF ),
            UnicodeBlock( NC_( "chardict", "Lycian" ), 0x10280, 0x1029F ),
            UnicodeBlock( NC_( "chardict", "Carian" ), 0x102A0, 0x102DF ),
            UnicodeBlock( NC_( "chardict", "Old Italic" ), 0x10300, 0x1032F ),
            UnicodeBlock( NC_( "chardict", "Gothic" ), 0x10330, 0x1034F ),
            UnicodeBlock( NC_( "chardict", "Ugaritic" ), 0x10380, 0x1039F ),
            UnicodeBlock( NC_( "chardict", "Old Persian" ), 0x103A0, 0x103DF ),
            UnicodeBlock( NC_( "chardict", "Deseret" ), 0x10400, 0x1044F ),
            UnicodeBlock( NC_( "chardict", "Shavian" ), 0x10450, 0x1047F ),
            UnicodeBlock( NC_( "chardict", "Osmanya" ), 0x10480, 0x104AF ),
            UnicodeBlock( NC_( "chardict", "Cypriot Syllabary" ), 0x10800, 0x1083F ),
            UnicodeBlock( NC_( "chardict", "Imperial Aramaic" ), 0x10840, 0x1085F ),
            UnicodeBlock( NC_( "chardict", "Phoenician" ), 0x10900, 0x1091F ),
            UnicodeBlock( NC_( "chardict", "Lydian" ), 0x10920, 0x1093F ),
            UnicodeBlock( NC_( "chardict", "Meroitic Hieroglyphs" ), 0x10980, 0x1099F ),
            UnicodeBlock( NC_( "chardict", "Meroitic Cursive" ), 0x109A0, 0x109FF ),
            UnicodeBlock( NC_( "chardict", "Kharoshthi" ), 0x10A00, 0x10A5F ),
            UnicodeBlock( NC_( "chardict", "Old South Arabian" ), 0x10A60, 0x10A7F ),
            UnicodeBlock( NC_( "chardict", "Avestan" ), 0x10B00, 0x10B3F ),
            UnicodeBlock( NC_( "chardict", "Inscriptional Parthian" ), 0x10B40, 0x10B5F ),
            UnicodeBlock( NC_( "chardict", "Inscriptional Pahlavi" ), 0x10B60, 0x10B7F ),
            UnicodeBlock( NC_( "chardict", "Old Turkic" ), 0x10C00, 0x10C4F ),
            UnicodeBlock( NC_( "chardict", "Rumi Numeral Symbols" ), 0x10E60, 0x10E7F ),
            UnicodeBlock( NC_( "chardict", "Brahmi" ), 0x11000, 0x1107F ),
            UnicodeBlock( NC_( "chardict", "Kaithi" ), 0x11080, 0x110CF ),
            UnicodeBlock( NC_( "chardict", "Sora Sompeng" ), 0x110D0, 0x110FF ),
            UnicodeBlock( NC_( "chardict", "Chakma" ), 0x11100, 0x1114F ),
            UnicodeBlock( NC_( "chardict", "Sharada" ), 0x11180, 0x111DF ),
            UnicodeBlock( NC_( "chardict", "Takri" ), 0x11680, 0x116CF ),
            UnicodeBlock( NC_( "chardict", "Cuneiform" ), 0x12000, 0x123FF ),
            UnicodeBlock( NC_( "chardict", "Cuneiform Numbers and Punctuation" ), 0x12400, 0x1247F ),
            UnicodeBlock( NC_( "chardict", "Egyptian Hieroglyphs" ), 0x13000, 0x1342F ),
            UnicodeBlock( NC_( "chardict", "Bamum Supplement" ), 0x16800, 0x16A3F ),
            UnicodeBlock( NC_( "chardict", "Miao" ), 0x16F00, 0x16F9F ),
            UnicodeBlock( NC_( "chardict", "Kana Supplement" ), 0x1B000, 0x1B0FF ),
            UnicodeBlock( NC_( "chardict", "Byzantine Musical Symbols" ), 0x1D000, 0x1D0FF ),
            UnicodeBlock( NC_( "chardict", "Musical Symbols" ), 0x1D100, 0x1D1FF ),
            UnicodeBlock( NC_( "chardict", "Ancient Greek Musical Notation" ), 0x1D200, 0x1D24F ),
            UnicodeBlock( NC_( "chardict", "Tai Xuan Jing Symbols" ), 0x1D300, 0x1D35F ),
            UnicodeBlock( NC_( "chardict", "Counting Rod Numerals" ), 0x1D360, 0x1D37F ),
            UnicodeBlock( NC_( "chardict", "Mathematical Alphanumeric Symbols" ), 0x1D400, 0x1D7FF ),
            UnicodeBlock( NC_( "chardict", "Arabic Mathematical Alphabetic Symbols" ), 0x1EE00, 0x1EEFF ),
            UnicodeBlock( NC_( "chardict", "Mahjong Tiles" ), 0x1F000, 0x1F02F ),
            UnicodeBlock( NC_( "chardict", "Domino Tiles" ), 0x1F030, 0x1F09F ),
            UnicodeBlock( NC_( "chardict", "Playing Cards" ), 0x1F0A0, 0x1F0FF ),
            UnicodeBlock( NC_( "chardict", "Enclosed Alphanumeric Supplement" ), 0x1F100, 0x1F1FF ),
            UnicodeBlock( NC_( "chardict", "Enclosed Ideographic Supplement" ), 0x1F200, 0x1F2FF ),
            UnicodeBlock( NC_( "chardict", "Miscellaneous Symbols and Pictographs" ), 0x1F300, 0x1F5FF ),
            UnicodeBlock( NC_( "chardict", "Emoticons" ), 0x1F600, 0x1F64F ),
            UnicodeBlock( NC_( "chardict", "Transport and Map Symbols" ), 0x1F680, 0x1F6FF ),
            UnicodeBlock( NC_( "chardict", "Alchemical Symbols" ), 0x1F700, 0x1F77F ),
            UnicodeBlock( NC_( "chardict", "CJK Unified Ideographs Extension B" ), 0x20000, 0x2A6D6 ),
            UnicodeBlock( NC_( "chardict", "CJK Unified Ideographs Extension C" ), 0x2A700, 0x2B734 ),
            UnicodeBlock( NC_( "chardict", "CJK Unified Ideographs Extension D" ), 0x2B740, 0x2B81D ),
            UnicodeBlock( NC_( "chardict", "CJK Compatibility Ideographs Supplement" ), 0x2F800, 0x2FA1F ),
            UnicodeBlock( NC_( "chardict", "Tags" ), 0xE0000, 0xE007F ),
            UnicodeBlock( NC_( "chardict", "Variation Selectors Supplement" ), 0xE0100, 0xE01EF ),
            UnicodeBlock( NC_( "chardict", "Supplementary Private Use Area-A" ), 0xFFF80, 0xFFFFF ),
            UnicodeBlock( NC_( "chardict", "Supplementary Private Use Area-B" ), 0x10FF80, 0x10FFFF )
        };

UnicodeViewWidget::UnicodeViewWidget( QWidget *parent, const char *name )
        : CharDictViewBase( parent, name )
{
    uBlockMap.clear();

    setupWidgets();

    readConfig();
}

UnicodeViewWidget::~UnicodeViewWidget()
{
    writeConfig();
}

void UnicodeViewWidget::setupWidgets()
{
    m_mainSplitter = new QSplitter( this );

    QVBox *leftVBox = new QVBox( m_mainSplitter );
    QLabel *unicodeBlockLabel = new QLabel( leftVBox );
    unicodeBlockLabel->setText( _( "UnicodeBlock List" ) );
    unicodeBlockLabel->setAlignment( Qt::AlignHCenter );
    m_unicodeBlockListView = new QListView( leftVBox );
    m_unicodeBlockListView->setSorting( -1 );
    m_unicodeBlockListView->setSelectionMode( QListView::Single );
    m_unicodeBlockListView->addColumn( "0" );
    m_unicodeBlockListView->header() ->setStretchEnabled( true, 0 );
    m_unicodeBlockListView->header() ->hide();
    m_unicodeBlockListView->setColumnWidthMode( 1, QListView::Maximum );
    m_unicodeBlockListView->setHScrollBarMode( QScrollView::AlwaysOff );
    m_unicodeBlockListView->setAllColumnsShowFocus( true );
    QObject::connect( m_unicodeBlockListView, SIGNAL( selectionChanged( QListViewItem * ) ),
                      this, SLOT( slotUnicodeBlockSelected( QListViewItem * ) ) );
    // add Item
    for ( int i = sizeof( uBlocks ) / sizeof ( uBlocks[0] ); i >= 0; i-- )
    {
        const QString uBlockName = uBlocks[ i ].getName();
        QString originalStr;
        originalStr += "chardict" GETTEXT_CONTEXT_GLUE + uBlockName;
        const char *original = originalStr;
        QString translation = mygettext( original );
        if ( translation == original )
            translation = uBlockName;
        QListViewItem *item = new QListViewItem( m_unicodeBlockListView, translation );
        uBlockMap[ item ] = &uBlocks[ i ];
    }

    m_charGridView = new CharGridView( 10, 0, m_mainSplitter );
    QObject::connect( m_charGridView, SIGNAL( charSelected( const QString & ) ),
                      this, SIGNAL( charSelected( const QString & ) ) );

    // main layout
    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->addWidget( m_mainSplitter );
}

void UnicodeViewWidget::slotUnicodeBlockSelected( QListViewItem *item )
{
    UnicodeBlock * block = uBlockMap[ item ];

    QStringList charList;
    for ( uint d = block->getStartHex(); d < block->getEndHex(); d++ )
    {
        charList.append( QString( QChar( d ) ) );
    }

    m_charGridView->setCharacters( charList );
}

void UnicodeViewWidget::writeConfig()
{
    QSettings settings;

    // splitter
    QString str;
    QTextOStream out( &str );
    out << *m_mainSplitter;
    settings.writeEntry( "/uim-kdehelper/chardict/unicodeview/splitter", str );
}
void UnicodeViewWidget::readConfig()
{
    QSettings settings;
    QString str;

    // splitter
    str = settings.readEntry( "/uim-kdehelper/chardict/unicodeview/splitter" );
    if ( !str.isEmpty() )
    {
        QTextIStream in( &str );
        in >> *m_mainSplitter;
    }
}

void UnicodeViewWidget::setFont( const QFont &font )
{
    m_charGridView->setFont( font );
}

#include "unicodeviewwidget.moc"
