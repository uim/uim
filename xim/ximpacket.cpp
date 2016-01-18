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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <list>

#include "xim.h"
#include "util.h"

//
// Routines for byte manipulating
//
int
rup4(int l)
{
    if ((l % 4) == 0)
	return l;

    return (l & 0xffffffc) + 4;
}

void
writeC8(C8 val, int /* byte_order */, unsigned char *buf)
{
    buf[0] = val;
}

void writeC16(C16 val, int byte_order, unsigned char *buf)
{
    if (byte_order == LSB_FIRST) {
	buf[0] = (unsigned char)(val & 255);
	buf[1] = (unsigned char)((val >> 8) & 255);
    } else {
	buf[1] = (unsigned char)(val & 255);
	buf[0] = (unsigned char)((val >> 8) & 255);
    }
}

void writeC32(unsigned int val, int byte_order, unsigned char *buf)
{
    if (byte_order == LSB_FIRST) {
	buf[0] = (unsigned char)(val & 255);
	buf[1] = (unsigned char)((val >> 8) & 255);
	buf[2] = (unsigned char)((val >> 16) & 255);
	buf[3] = (unsigned char)((val >> 24) & 255);
    } else {
	buf[3] = (unsigned char)(val & 255);
	buf[2] = (unsigned char)((val >> 8) & 255);
	buf[1] = (unsigned char)((val >> 16) & 255);
	buf[0] = (unsigned char)((val >> 24) & 255);
    }
}

C8 readC8(unsigned char *buf)
{
    return buf[0];
}

C16 readC16(unsigned char *buf, int byte_order)
{
    C16 v;
    if (byte_order == LSB_FIRST)
	v = (C16)(buf[0] + buf[1] * 256);
    else
	v = (C16)(buf[1] + buf[0] * 256);
    return v;
}

C32 readC32(unsigned char *buf, int byte_order)
{
    C32 v;
    if (byte_order == LSB_FIRST)
	v = buf[0] + buf[1] * 256 + (buf[2] << 16) +(buf[3] << 24);
    else
	v = buf[3] + buf[2] * 256 + (buf[1] << 16) +(buf[0] << 24);
    return v;
}

//
// TxPacket
//

class TxElement{
public:
    virtual ~TxElement() {};
    virtual int get_size() = 0;
    virtual int write_to_buf(unsigned char *buf, int byte_order) = 0;
};

class TxC8 : public TxElement {
public:
    TxC8(C8 v) {
	val = v;
    }
    virtual int get_size() {
	return 1;
    }
    virtual int write_to_buf(unsigned char *buf, int bo) {
	writeC8(val, bo, buf);
	return 1;
    }
private:
    C8 val;
};

class TxC16 : public TxElement {
public:
    TxC16(C16 v) {
	val = v;
    }
    virtual int get_size() {
	return 2;
    }
    virtual int write_to_buf(unsigned char *buf, int bo) {
	writeC16(val, bo, buf);
	return 2;
    }
private:
    C16 val;
};

class TxC32 : public TxElement {
public:
    TxC32(C32 v) {
	val = v;
    }
    virtual int get_size() {
	return 4;
    }
    virtual int write_to_buf(unsigned char *buf, int bo) {
	writeC32(val, bo, buf);
	return 4;
    }
private:
    C32 val;
};

class TxString : public TxElement {
public:
    TxString(char *s) {
	init(s, static_cast<int>(strlen(s)));
    }
    TxString(char *s, int len) {
	init(s, static_cast<int>(len));
    }
    virtual ~TxString() {
	free(m_str);
    }
    virtual int get_size() {
	return 2 + m_len + pad4(2 + m_len);
    }
    virtual int write_to_buf(unsigned char *buf, int bo) {
	writeC16((C16)m_len, bo, buf);
	memcpy(&buf[2], m_str, m_len);
	return get_size();
    }
private:
    void init(char *s, int len) {
	m_len = len;
	m_str = (char *)malloc(len + 1);
	strlcpy(m_str, s, len + 1);
    }
    int m_len;
    char *m_str;
};

class TxBytes : public TxElement {
public:
    TxBytes(const char *s, int len) {
	m_str = (char *)malloc(len);
	m_len = len;
	memcpy(m_str, s, len);
    };
    virtual ~TxBytes() {
	free(m_str);
    }
    virtual int get_size() {
	return m_len;
    }
    virtual int write_to_buf(unsigned char *buf, int /* bo */) {
	memcpy(buf, m_str, m_len);
	return get_size();
    }
private:
    int m_len;
    char *m_str;
};

class TxPacket_impl : public TxPacket {
public:
    TxPacket_impl(C8 major, C8 minor);
    virtual ~TxPacket_impl();

    virtual int get_length();
    virtual int write_to_buf(unsigned char *buf, int buflen, int byte_order);

    virtual void dump(int byte_order);
    virtual C8 get_major();

    virtual int pushC8(C8);
    virtual int pushC16(C16);
    virtual int pushC32(C32);
    virtual int pushSTRING(char *);
    virtual int pushBytes(const char *, int);

    virtual int pop_back();
private:
    void write_header(unsigned char *buf, int l, int byte_order);
    C8 m_major, m_minor;
    std::list <TxElement *> m_elms;
};

TxPacket_impl::TxPacket_impl(C8 major, C8 minor)
{
    m_major = major;
    m_minor = minor;
}

TxPacket_impl::~TxPacket_impl()
{
    std::list<TxElement *>::iterator i;
    for (i = m_elms.begin(); i != m_elms.end(); ++i) {
	delete *i;
    }
}

int TxPacket_impl::get_length()
{
    std::list<TxElement *>::iterator i;
    int l;
    l = 4;
    for (i = m_elms.begin(); i != m_elms.end(); ++i) {
	l +=  (*i)->get_size();
    }  
    return l;
}

int TxPacket_impl::write_to_buf(unsigned char *buf, int buflen, int byte_order)
{
    std::list<TxElement *>::iterator i;
    int l, m;
    l = 4;
    for (i = m_elms.begin(); i != m_elms.end(); ++i) {
	m = (*i)->get_size();
	if (l + m > buflen)
	    return 0;

	m = (*i)->write_to_buf(&buf[l], byte_order);
	l += m;
    }
    l = rup4(l);
    write_header(buf, l, byte_order);
    return l;
}

int TxPacket_impl::pushC8(C8 v)
{
    TxElement *e;
    e = new TxC8(v);
    m_elms.push_back(e);
    return e->get_size();
}

int TxPacket_impl::pushC16(C16 v)
{
    TxElement *e;
    e = new TxC16(v);
    m_elms.push_back(e);
    return e->get_size();
}

int TxPacket_impl::pushC32(C32 v)
{
    TxElement *e;
    e = new TxC32(v);
    m_elms.push_back(e);
    return e->get_size();
}

int TxPacket_impl::pushSTRING(char *s)
{
    TxElement *e;
    e = new TxString(s);
    m_elms.push_back(e);
    return e->get_size();
}

int TxPacket_impl::pushBytes(const char *b, int len)
{
    TxElement *e;
    e = new TxBytes(b, len);
    m_elms.push_back(e);
    return e->get_size();
}

int TxPacket_impl::pop_back()
{
    int len;
    TxElement *e;
    e = m_elms.back();
    len = e->get_size();
    delete e;
    m_elms.pop_back();
    return len;
}

void TxPacket_impl::write_header(unsigned char *buf, int l, int byte_order)
{
    buf[0] = m_major;
    buf[1] = m_minor;
    writeC16((C16)(l / 4 - 1), byte_order, &buf[2]);
}

void TxPacket_impl::dump(int byte_order)
{
    unsigned char *buf;
    int len;
    len = get_length();
    buf = (unsigned char *)malloc(len);
    write_to_buf(buf, len, byte_order);
    hex_dump(buf, len);
    free(buf);
}

C8 TxPacket_impl::get_major()
{
    return m_major;
}

TxPacket *createTxPacket(C8 major, C8 minor)
{
    return new TxPacket_impl(major, minor);
}

//
// Routines for RxPacket
//
class RxPacket_impl : public RxPacket {
public:
    RxPacket_impl(unsigned char *buf, int byte_order);
    RxPacket_impl(const RxPacket_impl& rhs);
    virtual ~RxPacket_impl();

    virtual void rewind();
    virtual C8 getC8();
    virtual C16 getC16();
    virtual C32 getC32();
    virtual int getStrLen();
    virtual void getStr(char *buf);
    virtual int getStr8Len();
    virtual void getStr8(char *);

    virtual int getMajor();

    virtual bool isOverRun() {
	if ((g_option_mask & OPT_TRACE) && mIsOverRun)
	    printf("RxPacket Overrun.\n");
	return mIsOverRun;
    };
    virtual void dump();
private:
    bool canRead(int);
    int mLen;
    unsigned char *mBuf;
    int mIndex;
    int mByteOrder;
    bool mIsOverRun;
};

RxPacket_impl::RxPacket_impl(unsigned char *b, int byte_order)
{
    mLen = getPacketLength(b, byte_order);
    mBuf = (unsigned char *)malloc(mLen);
    memcpy(mBuf, b, mLen);
    mByteOrder = byte_order;
    rewind();
}

RxPacket_impl::RxPacket_impl(const RxPacket_impl& rhs) : RxPacket(rhs)
{
    mLen = rhs.mLen;
    mBuf = (unsigned char *)malloc(mLen);
    memcpy(mBuf, rhs.mBuf, mLen);
    mIndex = rhs.mIndex;
    mByteOrder = rhs.mByteOrder;
    mIsOverRun = rhs.mIsOverRun;
}

RxPacket_impl::~RxPacket_impl()
{
    free(mBuf);
}

void RxPacket_impl::rewind()
{
    mIndex = 4;
    mIsOverRun = false;
}

C8 RxPacket_impl::getC8()
{
    C8 v;
    if (!canRead(1)) {
	mIsOverRun = true;
	return 0;
    }
    v = readC8(&mBuf[mIndex]);
    mIndex += 1;
    return v;
}

C16 RxPacket_impl::getC16()
{
    C16 v;
    if (!canRead(2)) {
	mIsOverRun = true;
	return 0;
    }
    v = readC16(&mBuf[mIndex], mByteOrder);
    mIndex += 2;
    return v;
}

C32 RxPacket_impl::getC32()
{
    C32 v;
    if (!canRead(4)) {
	mIsOverRun = true;
	return 0;
    }
    v = readC32(&mBuf[mIndex], mByteOrder);
    mIndex += 4;
    return v;
}

int RxPacket_impl::getStrLen()
{
    if (!canRead(2)) {
	mIsOverRun = true;
	return 0;
    }
    return readC16(&mBuf[mIndex], mByteOrder);
}

void RxPacket_impl::getStr(char *buf)
{
    int l;
    l = getStrLen();
    if (!canRead(l + 2)) {
	mIsOverRun = true;
	return;
    }
    memcpy(buf, &mBuf[mIndex + 2], l);
    mIndex += (2 + l + pad4(2 + l));
}

int RxPacket_impl::getStr8Len()
{
    if (!canRead(1)) {
	mIsOverRun = true;
	return 0;
    }
    return readC8(&mBuf[mIndex]);
}

void RxPacket_impl::getStr8(char *buf)
{
    int l;
    l = getStr8Len();
    if (!canRead(l + 1)) {
	mIsOverRun = true;
	return;
    }
    memcpy(buf, &mBuf[mIndex + 1], l);
    mIndex += (1 + l);
}

int RxPacket_impl::getMajor()
{
    return mBuf[0];
}

void RxPacket_impl::dump()
{
    hex_dump(mBuf, mLen);
}

bool RxPacket_impl::canRead(int s)
{
    if (mIndex +s > mLen)
	return false;

    return true;
}

// static methods
int RxPacket::getPacketLength(unsigned char *buf, int byte_order)
{
    if (byte_order == BYTEORDER_UNKNOWN)
	return 0;

    return readC16(&buf[2], byte_order) * 4 + 4;
}

RxPacket *createRxPacket(unsigned char *buf, int byte_order)
{
    return new RxPacket_impl(buf, byte_order);
}

RxPacket *copyRxPacket(RxPacket *p)
{
    return new RxPacket_impl(*(RxPacket_impl *)p);
}
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
