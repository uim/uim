/*

Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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

#include <klocale.h>


class UnicodeBlock
{
public:
    UnicodeBlock( const QString &name, ushort startHex, ushort endHex )
    {
        m_name = name;
        m_startHex = startHex;
        m_endHex = endHex;
    }

    QString getName() const { return m_name; }
    ushort getStartHex() const { return m_startHex; }
    ushort getEndHex() const { return m_endHex; }

protected:
    QString m_name;
    ushort m_startHex;
    ushort m_endHex;
};

#define UBLOCK_SIZE 125
static UnicodeBlock uBlocks[ UBLOCK_SIZE ] = {
            UnicodeBlock( i18n( "Basic Latin" ), 0x0000, 0x007F ),
            UnicodeBlock( i18n( "Latin-1 Supplement" ), 0x0080, 0x00FF ),
            UnicodeBlock( i18n( "Latin Extended-A" ), 0x0100, 0x017F ),
            UnicodeBlock( i18n( "Latin Extended-B" ), 0x0180, 0x024F ),
            UnicodeBlock( i18n( "IPA Extensions" ), 0x0250, 0x02AF ),
            UnicodeBlock( i18n( "Spacing Modifier Letters" ), 0x02B0, 0x02FF ),
            UnicodeBlock( i18n( "Combining Diacritical Marks" ), 0x0300, 0x036F ),
            UnicodeBlock( i18n( "Greek and Coptic" ), 0x0370, 0x03FF ),
            UnicodeBlock( i18n( "Cyrillic" ), 0x0400, 0x04FF ),
            UnicodeBlock( i18n( "Cyrillic Supplement" ), 0x0500, 0x052F ),
            UnicodeBlock( i18n( "Armenian" ), 0x0530, 0x058F ),
            UnicodeBlock( i18n( "Hebrew" ), 0x0590, 0x05FF ),
            UnicodeBlock( i18n( "Arabic" ), 0x0600, 0x06FF ),
            UnicodeBlock( i18n( "Syriac" ), 0x0700, 0x074F ),
            UnicodeBlock( i18n( "Thaana" ), 0x0780, 0x07BF ),
            UnicodeBlock( i18n( "Devanagari" ), 0x0900, 0x097F ),
            UnicodeBlock( i18n( "Bengali" ), 0x0980, 0x09FF ),
            UnicodeBlock( i18n( "Gurmukhi" ), 0x0A00, 0x0A7F ),
            UnicodeBlock( i18n( "Gujarati" ), 0x0A80, 0x0AFF ),
            UnicodeBlock( i18n( "Oriya" ), 0x0B00, 0x0B7F ),
            UnicodeBlock( i18n( "Tamil" ), 0x0B80, 0x0BFF ),
            UnicodeBlock( i18n( "Telugu" ), 0x0C00, 0x0C7F ),
            UnicodeBlock( i18n( "Kannada" ), 0x0C80, 0x0CFF ),
            UnicodeBlock( i18n( "Malayalam" ), 0x0D00, 0x0D7F ),
            UnicodeBlock( i18n( "Sinhala" ), 0x0D80, 0x0DFF ),
            UnicodeBlock( i18n( "Thai" ), 0x0E00, 0x0E7F ),
            UnicodeBlock( i18n( "Lao" ), 0x0E80, 0x0EFF ),
            UnicodeBlock( i18n( "Tibetan" ), 0x0F00, 0x0FFF ),
            UnicodeBlock( i18n( "Myanmar" ), 0x1000, 0x109F ),
            UnicodeBlock( i18n( "Georgian" ), 0x10A0, 0x10FF ),
            UnicodeBlock( i18n( "Hangul Jamo" ), 0x1100, 0x11FF ),
            UnicodeBlock( i18n( "Ethiopic" ), 0x1200, 0x137F ),
            UnicodeBlock( i18n( "Cherokee" ), 0x13A0, 0x13FF ),
            UnicodeBlock( i18n( "Unified Canadian Aboriginal Syllabics" ), 0x1400, 0x167F ),
            UnicodeBlock( i18n( "Ogham" ), 0x1680, 0x169F ),
            UnicodeBlock( i18n( "Runic" ), 0x16A0, 0x16FF ),
            UnicodeBlock( i18n( "Tagalog" ), 0x1700, 0x171F ),
            UnicodeBlock( i18n( "Hanunoo" ), 0x1720, 0x173F ),
            UnicodeBlock( i18n( "Buhid" ), 0x1740, 0x175F ),
            UnicodeBlock( i18n( "Tagbanwa" ), 0x1760, 0x177F ),
            UnicodeBlock( i18n( "Khmer" ), 0x1780, 0x17FF ),
            UnicodeBlock( i18n( "Mongolian" ), 0x1800, 0x18AF ),
            UnicodeBlock( i18n( "Limbu" ), 0x1900, 0x194F ),
            UnicodeBlock( i18n( "Tai Letters" ), 0x1950, 0x197F ),
            UnicodeBlock( i18n( "Khmer Symbols" ), 0x19E0, 0x19FF ),
            UnicodeBlock( i18n( "Phonetic Extensions" ), 0x1D00, 0x1D7F ),
            UnicodeBlock( i18n( "Latin Extended Additional" ), 0x1E00, 0x1EFF ),
            UnicodeBlock( i18n( "Greek Extended-B" ), 0x1F00, 0x1FFF ),
            UnicodeBlock( i18n( "General Punctuation" ), 0x2000, 0x206F ),
            UnicodeBlock( i18n( "Superscripts and Subscripts" ), 0x2070, 0x209F ),
            UnicodeBlock( i18n( "Currency Symbols" ), 0x20A0, 0x20CF ),
            UnicodeBlock( i18n( "Combining Diacritical Marks for Symbols" ), 0x20D0, 0x20FF ),
            UnicodeBlock( i18n( "Letterlike Symbols" ), 0x2100, 0x214F ),
            UnicodeBlock( i18n( "Number Forms" ), 0x2150, 0x218F ),
            UnicodeBlock( i18n( "Arrows" ), 0x2190, 0x21FF ),
            UnicodeBlock( i18n( "Mathematical Operators" ), 0x2200, 0x22FF ),
            UnicodeBlock( i18n( "Miscellaneous Technical" ), 0x2300, 0x23FF ),
            UnicodeBlock( i18n( "Control Pictures" ), 0x2400, 0x243F ),
            UnicodeBlock( i18n( "Optical Character Recognition" ), 0x2440, 0x245F ),
            UnicodeBlock( i18n( "Enclosed Alphanumerics" ), 0x2460, 0x24FF ),
            UnicodeBlock( i18n( "Box Drawing" ), 0x2500, 0x257F ),
            UnicodeBlock( i18n( "Block Elements" ), 0x2580, 0x259F ),
            UnicodeBlock( i18n( "Geometric Shapes" ), 0x25A0, 0x25FF ),
            UnicodeBlock( i18n( "Miscellaneous Symbols" ), 0x2600, 0x26FF ),
            UnicodeBlock( i18n( "Dingbats" ), 0x2700, 0x27BF ),
            UnicodeBlock( i18n( "Miscellaneous Mathematical Symbols-A" ), 0x27C0, 0x27EF ),
            UnicodeBlock( i18n( "Supplemental Arrows-A" ), 0x27F0, 0x27FF ),
            UnicodeBlock( i18n( "Braille Patterns" ), 0x2800, 0x28FF ),
            UnicodeBlock( i18n( "Supplemental Arrows-B" ), 0x2900, 0x297F ),
            UnicodeBlock( i18n( "Miscellaneous Mathematical Symbols-B" ), 0x2980, 0x29FF ),
            UnicodeBlock( i18n( "Supplemental Mathematical Operators" ), 0x2A00, 0x2AFF ),
            UnicodeBlock( i18n( "Miscellaneous Symbols and Arrows-B" ), 0x2B00, 0x2BFF ),
            UnicodeBlock( i18n( "CJK Radicals Supplemental" ), 0x2E80, 0x2EFF ),
            UnicodeBlock( i18n( "Kangxi Radicals" ), 0x2F00, 0x2FDF ),
            UnicodeBlock( i18n( "Ideographic Description Characters" ), 0x2FF0, 0x2FFF ),
            UnicodeBlock( i18n( "CJK Symbols and Punctuation" ), 0x3000, 0x303F ),
            UnicodeBlock( i18n( "Hiragana" ), 0x3040, 0x309F ),
            UnicodeBlock( i18n( "Katakana" ), 0x30A0, 0x30FF ),
            UnicodeBlock( i18n( "Bopomofo" ), 0x3100, 0x312F ),
            UnicodeBlock( i18n( "Hangul Compatibility Jamo" ), 0x3130, 0x318F ),
            UnicodeBlock( i18n( "Kanbun" ), 0x3190, 0x319F ),
            UnicodeBlock( i18n( "Bopomofo Extended-B 1F00" ), 0x31A0, 0x31BF ),
            UnicodeBlock( i18n( "Katakana Phonetic Extensions" ), 0x31F0, 0x31FF ),
            UnicodeBlock( i18n( "Enclosed CJK Letters and Months" ), 0x3200, 0x32FF ),
            UnicodeBlock( i18n( "CJK Compatibility" ), 0x3300, 0x33FF ),
            UnicodeBlock( i18n( "CJK Unified Ideographs Extension and" ), 0x3400, 0x4DBF ),
            UnicodeBlock( i18n( "Yijing Hexagram Symbols-B" ), 0x4DC0, 0x4DFF ),
            UnicodeBlock( i18n( "CJK Unified Ideographs" ), 0x4E00, 0x9FFF ),
            UnicodeBlock( i18n( "Yi Syllables" ), 0xA000, 0xA48F ),
            UnicodeBlock( i18n( "Yi Radicals" ), 0xA490, 0xA4CF ),
            UnicodeBlock( i18n( "Hangul Syllables" ), 0xAC00, 0xD7AF ),
            UnicodeBlock( i18n( "High Surrogates" ), 0xD800, 0xDB7F ),
            UnicodeBlock( i18n( "High Private Use Surrogates" ), 0xDB80, 0xDBFF ),
            UnicodeBlock( i18n( "Low Surrogates" ), 0xDC00, 0xDFFF ),
            UnicodeBlock( i18n( "Private Use Area" ), 0xE000, 0xF8FF ),
            UnicodeBlock( i18n( "CJK Compatibility Ideographs" ), 0xF900, 0xFAFF ),
            UnicodeBlock( i18n( "Alphabetic Presentation Forms" ), 0xFB00, 0xFB4F ),
            UnicodeBlock( i18n( "Arabic Presentation Forms-A" ), 0xFB50, 0xFDFF ),
            UnicodeBlock( i18n( "Variation Selectors" ), 0xFE00, 0xFE0F ),
            UnicodeBlock( i18n( "Combining Half Marks" ), 0xFE20, 0xFE2F ),
            UnicodeBlock( i18n( "CJK Compatibility Forms-A" ), 0xFE30, 0xFE4F ),
            UnicodeBlock( i18n( "Small Form Variants" ), 0xFE50, 0xFE6F ),
            UnicodeBlock( i18n( "Arabic Presentation Forms-B" ), 0xFE70, 0xFEFF ),
            UnicodeBlock( i18n( "Halfwidth and Fullwidth Forms-B" ), 0xFF00, 0xFFEF ),
            UnicodeBlock( i18n( "Specials" ), 0xFFF0, 0xFFFF ),
            UnicodeBlock( i18n( "Linear B Syllabary" ), 0x10000, 0x1007F ),
            UnicodeBlock( i18n( "Linear B Ideograms" ), 0x10080, 0x100FF ),
            UnicodeBlock( i18n( "Aegean Numbers" ), 0x10100, 0x1013F ),
            UnicodeBlock( i18n( "Old Italic" ), 0x10300, 0x1032F ),
            UnicodeBlock( i18n( "Gothic" ), 0x10330, 0x1034F ),
            UnicodeBlock( i18n( "Ugaritic" ), 0x10380, 0x1039F ),
            UnicodeBlock( i18n( "Deseret" ), 0x10400, 0x1044F ),
            UnicodeBlock( i18n( "Shavian" ), 0x10450, 0x1047F ),
            UnicodeBlock( i18n( "Osmanya" ), 0x10480, 0x104AF ),
            UnicodeBlock( i18n( "Cypriot Syllabary" ), 0x10800, 0x1083F ),
            UnicodeBlock( i18n( "Byzantine Musical Symbols-B 4DC0" ), 0x1D000, 0x1D0FF ),
            UnicodeBlock( i18n( "Musical Symbols-B 4DC0 1D000" ), 0x1D100, 0x1D1FF ),
            UnicodeBlock( i18n( "Tai Xuan Jing Symbols-B 4DC0 1D000 1D100" ), 0x1D300, 0x1D35F ),
            UnicodeBlock( i18n( "Mathematical Alphanumeric Symbols-B 4DC0 1D000 1D100 1D300" ), 0x1D400, 0x1D7FF ),
            UnicodeBlock( i18n( "CJK Unified Ideographs Extension B 4DC0 1D000 1D100 1D300 1D400" ), 0x20000, 0x2A6DF ),
            UnicodeBlock( i18n( "CJK Compatibility Ideographs Supplemental 2E80" ), 0x2F800, 0x2FA1F ),
            UnicodeBlock( i18n( "Tags" ), 0xE0000, 0xE007F ),
            UnicodeBlock( i18n( "Variation Selectors Supplemental 2E80 2F800" ), 0xE0100, 0xE01EF ),
            UnicodeBlock( i18n( "Supplementary Private Use Area-A" ), 0xF0000, 0xFFFFF ),
            UnicodeBlock( i18n( "Supplementary Private Use Area-B" ), 0x100000, 0x10FFFF )
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
    unicodeBlockLabel->setText( i18n( "UnicodeBlock List" ) );
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
    for ( int i = ( UBLOCK_SIZE - 1 ); i >= 0; i-- )
    {
        QListViewItem *item = new QListViewItem( m_unicodeBlockListView, uBlocks[ i ].getName() );
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
    for ( ushort d = block->getStartHex(); d < block->getEndHex(); d++ )
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
