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

// -*- C++ -*-
// XIM related constant numbers
#ifndef UIM_XIM_XIMPN_H
#define UIM_XIM_XIMPN_H

// MAJOR number or XIM request
//
#define XIM_CONNECT 1
#define XIM_CONNECT_REPLY 2
#define XIM_DISCONNECT 3
#define XIM_DISCONNECT_REPLY 4

#define XIM_AUTH_REQUIRED 10
#define XIM_AUTH_REPLY 11
#define XIM_AUTH_NEXT 12
#define XIM_AUTH_SETUP 13
#define XIM_AUTH_NG 14

#define XIM_ERROR 20

#define XIM_OPEN 30
#define XIM_OPEN_REPLY 31
#define XIM_CLOSE 32
#define XIM_CLOSE_REPLY 33
#define XIM_REGISTER_TRIGGERKEYS 34
#define XIM_TRIGGER_NOTIFY 35
#define XIM_TRIGGER_NOTIFY_REPLY 36
#define XIM_SET_EVENT_MASK 37
#define XIM_ENCODING_NEGOTIATION 38
#define XIM_ENCODING_NEGOTIATION_REPLY 39
#define XIM_QUERY_EXTENSION 40
#define XIM_QUERY_EXTENSION_REPLY 41
#define XIM_SET_IM_VALUES 42
#define XIM_SET_IM_VALUES_REPLY 43
#define XIM_GET_IM_VALUES 44
#define XIM_GET_IM_VALUES_REPLY 45

#define XIM_CREATE_IC 50
#define XIM_CREATE_IC_REPLY 51
#define XIM_DESTROY_IC 52
#define XIM_DESTROY_IC_REPLY 53
#define XIM_SET_IC_VALUES 54
#define XIM_SET_IC_VALUES_REPLY 55
#define XIM_GET_IC_VALUES 56
#define XIM_GET_IC_VALUES_REPLY 57
#define XIM_SET_IC_FOCUS 58
#define XIM_UNSET_IC_FOCUS 59
#define XIM_FORWARD_EVENT 60
#define XIM_SYNC 61
#define XIM_SYNC_REPLY 62
#define XIM_COMMIT 63
#define XIM_RESET_IC 64
#define XIM_RESET_IC_REPLY 65

#define XIM_GEOMETRY 70
#define XIM_STR_CONVERSION 71
#define XIM_STR_CONVERSION_REPLY 72
#define XIM_PREEDIT_START 73
#define XIM_PREEDIT_START_REPLY 74
#define XIM_PREEDIT_DRAW 75
#define XIM_PREEDIT_CARET 76
#define XIM_PREEDIT_CARET_REPLY 77
#define XIM_PREEDIT_DONE 78
#define XIM_STATUS_START 79
#define XIM_STATUS_DRAW 80
#define XIM_STATUS_DONE 81
#define XIM_PREEDITSTATE 82

//
//
#define TYPE_SEPARATOR 0
#define TYPE_BYTE 1
#define TYPE_WORD 2
#define TYPE_LONG 3
#define TYPE_CHAR 4
#define TYPE_WINDOW 5
#define TYPE_XIMSTYLE 10
#define TYPE_XRECTANGLE 11
#define TYPE_POINT 12
#define TYPE_XFONTSET 13
#define TYPE_XIMHOTKEYTRIGGERS 15
#define TYPE_XIMHOTKEYSTATE 16
#define TYPE_XIMSTRINGCONVERSION 17
#define TYPE_XIMPREEDITSTATE 18
#define TYPE_XIMRESETSTATE 19
#define TYPE_NESTEDLIST 0x7fff

// ICATTRIBUTE
#define ICA_InputStyle 0
#define ICA_ClientWindow 1
#define ICA_FocusWindow 2
#define ICA_PreeditAttribute 3
#define ICA_Foreground 4
#define ICA_Background 5
#define ICA_SpotLocation 6
#define ICA_FontSet 7
#define ICA_Area 8
#define ICA_LineSpace 9
//
#define ICA_StatusAttributes 10
#define ICA_AreaNeeded 11
#define ICA_ColorMap 12
#define ICA_StdColorMap 13
#define ICA_BackgroundPixmap 14
#define ICA_Cursor 15
#define ICA_FilterEvents 16

// Style (Input Style)
#define IS_INVALID 0
#define IS_OVER_THE_SPOT 1
#define IS_ROOT_WINDOW 2
#define IS_OFF_THE_SPOT 3
#define IS_ON_THE_SPOT 4

// Error of XIM
#define ERR_BadAlloc 1
#define ERR_Style 2
#define ERR_BadClientWindow 3
#define ERR_BadFocusWindow 4
#define ERR_BadArea 5
#define ERR_BadSpotLocation 6
#define ERR_BadColormap 7
#define ERR_BadAtom 8
#define ERR_BadPixel 9
#define ERR_BadPixmap 10
#define ERR_BadName 11
#define ERR_BadCursor 12
#define ERR_BadProtocol 13
#define ERR_BadForeground 14
#define ERR_BadBackground 15
#define ERR_LocaleNotSupported 16
#define ERR_BadSomething 999

// Feedback for OnTheSpot input
#define FB_None 0
#define FB_Reverse 1
#define FB_Underline 2
#define FB_Highlight 4

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
