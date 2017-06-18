;;; iso-639-1.scm: definition of ISO 639-1 language codes
;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;; See following page for original code definitions
;;   http://www.loc.gov/standards/iso639-2/englangn.html

(define iso-639-1-alist
  `(("ab"    . ,(N_ "Abkhazian"))
    ("aa"    . ,(N_ "Afar"))
    ("af"    . ,(N_ "Afrikaans"))
    ("sq"    . ,(N_ "Albanian"))
    ("am"    . ,(N_ "Amharic"))
    ("ar"    . ,(N_ "Arabic"))
    ("hy"    . ,(N_ "Armenian"))
    ("as"    . ,(N_ "Assamese"))
    ("ay"    . ,(N_ "Aymara"))
    ("az"    . ,(N_ "Azerbaijani"))
    ("ba"    . ,(N_ "Bashkir"))
    ("eu"    . ,(N_ "Basque"))
    ("bn"    . ,(N_ "Bengali (Bangla)"))
    ("bn"    . ,(N_ "Bengali"))
    ("dz"    . ,(N_ "Bhutani"))
    ("bh"    . ,(N_ "Bihari"))
    ("bi"    . ,(N_ "Bislama")) 
    ("br"    . ,(N_ "Breton"))
    ("bg"    . ,(N_ "Bulgarian"))
    ("my"    . ,(N_ "Burmese"))
    ("be"    . ,(N_ "Byelorussian (Belarusian)"))
    ("be"    . ,(N_ "Byelorussian"))
    ("km"    . ,(N_ "Cambodian"))
    ("ca"    . ,(N_ "Catalan"))
    ("la"    . ,(N_ "Chewa"))
    ("zh_CN" . ,(N_ "Chinese (Simplified)"))
    ("zh_TW" . ,(N_ "Chinese (Traditional)"))
    ("zh_HK" . ,(N_ "Chinese (Traditional)"))
    ("zh"    . ,(N_ "Chinese"))
    ("co"    . ,(N_ "Corsican"))
    ("hr"    . ,(N_ "Croatian"))
    ("cs"    . ,(N_ "Czech"))
    ("da"    . ,(N_ "Danish"))
    ("dv"    . ,(N_ "Dhivehi"))
    ("nl"    . ,(N_ "Dutch"))
    ("en"    . ,(N_ "English"))
    ("eo"    . ,(N_ "Esperanto"))
    ("et"    . ,(N_ "Estonian"))
    ("fo"    . ,(N_ "Faeroese"))
    ("fa"    . ,(N_ "Farsi"))
    ("fj"    . ,(N_ "Fiji"))
    ("fi"    . ,(N_ "Finnish"))
    ("LA"    . ,(N_ "Flemish")) ;; defined as "nl" in the source
    ("fr"    . ,(N_ "French"))
    ("fy"    . ,(N_ "Frisian"))
    ("gl"    . ,(N_ "Galician"))
    ("gd"    . ,(N_ "Gaelic (Scottish)"))
    ("gv"    . ,(N_ "Gaelic (Manx)"))
    ("ka"    . ,(N_ "Georgian"))
    ("de"    . ,(N_ "German"))
    ("el"    . ,(N_ "Greek"))
    ("kl"    . ,(N_ "Greenlandic"))
    ("gn"    . ,(N_ "Guarani"))
    ("gu"    . ,(N_ "Gujarati"))
    ("ha"    . ,(N_ "Hausa"))
    ("he"    . ,(N_ "Hebrew"))
    ("hi"    . ,(N_ "Hindi"))
    ("hu"    . ,(N_ "Hungarian"))
    ("is"    . ,(N_ "Icelandic"))
    ("id"    . ,(N_ "Indonesian"))
    ("ia"    . ,(N_ "Interlingua"))
    ("ie"    . ,(N_ "Interlingue"))
    ("iu"    . ,(N_ "Inuktitut"))
    ("ik"    . ,(N_ "Inupiak"))
    ("ga"    . ,(N_ "Irish"))
    ("it"    . ,(N_ "Italian"))
    ("ja"    . ,(N_ "Japanese"))
    ("jv"    . ,(N_ "Javanese"))
    ("kn"    . ,(N_ "Kannada"))
    ("ks"    . ,(N_ "Kashmiri"))
    ("kk"    . ,(N_ "Kazakh"))
    ("rw"    . ,(N_ "Kinyarwanda (Ruanda)"))
    ("ky"    . ,(N_ "Kirghiz"))
    ("rn"    . ,(N_ "Kirundi (Rundi)"))
    ("LA"    . ,(N_ "Konkani")) ;; defined as "kok" in ISO 639-2
    ("ko"    . ,(N_ "Korean"))
    ("ku"    . ,(N_ "Kurdish"))
    ("lo"    . ,(N_ "Laothian"))
    ("la"    . ,(N_ "Latin"))
    ("lv"    . ,(N_ "Latvian (Lettish)"))
    ("ln"    . ,(N_ "Lingala"))
    ("lt"    . ,(N_ "Lithuanian"))
    ("mk"    . ,(N_ "Macedonian"))
    ("mg"    . ,(N_ "Malagasy"))
    ("ms"    . ,(N_ "Malay"))
    ("ml"    . ,(N_ "Malayalam"))
    ("mt"    . ,(N_ "Maltese"))
    ("mi"    . ,(N_ "Maori"))
    ("mr"    . ,(N_ "Marathi"))
    ("mo"    . ,(N_ "Moldavian"))
    ("mn"    . ,(N_ "Mongolian"))
    ("my"    . ,(N_ "Myanmar"))
    ("na"    . ,(N_ "Nauru"))
    ("ne"    . ,(N_ "Nepali"))
    ("no"    . ,(N_ "Norwegian"))
    ("oc"    . ,(N_ "Occitan"))
    ("or"    . ,(N_ "Oriya"))
    ("om"    . ,(N_ "Oromo (Afan, Galla)"))
    ("ps"    . ,(N_ "Pashto (Pushto)"))
    ("pl"    . ,(N_ "Polish"))
    ("pt"    . ,(N_ "Portuguese"))
    ("pa"    . ,(N_ "Punjabi"))
    ("qu"    . ,(N_ "Quechua"))
    ("ro"    . ,(N_ "Romanian"))
    ("ru"    . ,(N_ "Russian"))
    ("sm"    . ,(N_ "Samoan"))
    ("sg"    . ,(N_ "Sangro"))
    ("sa"    . ,(N_ "Sanskrit"))
    ("sr"    . ,(N_ "Serbian"))
    ("st"    . ,(N_ "Sesotho"))
    ("tn"    . ,(N_ "Setswana"))
    ("sn"    . ,(N_ "Shona"))
    ("sd"    . ,(N_ "Sindhi"))
    ("si"    . ,(N_ "Sinhalese"))
    ("ss"    . ,(N_ "Siswati"))
    ("sk"    . ,(N_ "Slovak"))
    ("sl"    . ,(N_ "Slovenian"))
    ("so"    . ,(N_ "Somali"))
    ("es"    . ,(N_ "Spanish"))
    ("su"    . ,(N_ "Sundanese"))
    ("sw"    . ,(N_ "Swahili (Kiswahili)"))
    ("sv"    . ,(N_ "Swedish"))
    ("LA"    . ,(N_ "Syriac")) ;; defined as "syr" in ISO 639-2
    ("tl"    . ,(N_ "Tagalog"))
    ("tg"    . ,(N_ "Tajik"))
    ("ta"    . ,(N_ "Tamil"))
    ("tt"    . ,(N_ "Tatar"))
    ("te"    . ,(N_ "Telugu"))
    ("th"    . ,(N_ "Thai"))
    ("bo"    . ,(N_ "Tibetan"))
    ("ti"    . ,(N_ "Tigrinya"))
    ("to"    . ,(N_ "Tonga"))
    ("ts"    . ,(N_ "Tsonga"))
    ("tr"    . ,(N_ "Turkish"))
    ("tk"    . ,(N_ "Turkmen"))
    ("tw"    . ,(N_ "Twi"))
    ("ug"    . ,(N_ "Uighur"))
    ("uk"    . ,(N_ "Ukrainian"))
    ("ur"    . ,(N_ "Urdu"))
    ("uz"    . ,(N_ "Uzbek"))
    ("vi"    . ,(N_ "Vietnamese"))
    ("cy"    . ,(N_ "Welsh"))
    ("wo"    . ,(N_ "Wolof"))
    ("xh"    . ,(N_ "Xhosa"))
    ("yi"    . ,(N_ "Yiddish"))
    ("yo"    . ,(N_ "Yoruba"))
    ("zu"    . ,(N_ "Zulu"))))
