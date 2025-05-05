/*
*   Copyright (C) 1997  Michael Roth <mroth@wirlweb.de>
*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU Library General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU Library General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this program; if not, see <https://www.gnu.org/licenses/>.
*
*/

#ifndef __KSEPARATOR_H__
#define __KSEPARATOR_H__

#include <qframe.h>

/**
 * Standard horizontal or vertical separator.
 *
 * @author Michael Roth <mroth@wirlweb.de>
 * @version $Id: kseparator.h,v 1.14 2004/09/09 15:23:53 faure Exp $
*/
class KSeparator : public QFrame
{
    Q_OBJECT
    Q_PROPERTY( int orientation READ orientation WRITE setOrientation )
public:
    /**
     * Constructor.
     * @param parent parent object.
     * @param name name of the new object.
     * @param f extra QWidget flags.
     **/
    KSeparator( QWidget* parent = 0, const char* name = 0, WFlags f = 0 );

    /**
     * Constructor.
     * @param orientation Set the orientation of the separator.
     * Possible values are HLine or Horizontal and VLine or Vertical.
     * @param parent parent object.
     * @param name name of the new object.
     * @param f extra QWidget flags.
     **/
    KSeparator( int orientation, QWidget* parent = 0, const char* name = 0,
                WFlags f = 0 );

    /**
     * Returns the orientation of the separator.
     * @return int Possible values are VLine and HLine.
     **/
    int orientation() const;

    /**
     * Set the orientation of the separator to @p orient
     *
     * @param orient Possible values are VLine and HLine.
     */
    void setOrientation( int orient );

    /**
     * The recommended height (width) for a horizontal (vertical) separator.
     **/
    virtual QSize sizeHint() const;

protected:
    /**
     * @param p pointer to painter
     */
    virtual void drawFrame( QPainter *p );
protected:
    virtual void virtual_hook( int id, void* data );
private:
    class KSeparatorPrivate* d;
};


#endif // __KSEPARATOR_H__
