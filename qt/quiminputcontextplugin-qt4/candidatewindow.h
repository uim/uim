#ifndef _CANDIDATE_WINDOW_H_
#define _CANDIDATE_WINDOW_H_

class QUimInputContext;

#include <uim/uim.h>

#include <qvboxwidget.h>

class QLabel;
class QListWidget;
class QListWidgetItem;

class QUimInputContext;

class CandidateWindow : public QVBoxWidget
{
    Q_OBJECT

public:
    CandidateWindow( QWidget * parent = 0 );
    ~CandidateWindow();

    void activateCandwin( int dLimit );
    void deactivateCandwin();
    void clearCandidates();
    void popup();

    void setAlwaysLeftPosition( bool left ) { isAlwaysLeft = left; }
    bool isAlwaysLeftPosition() const { return isAlwaysLeft; }

    void setCandidates( int displayLimit, const QList<uim_candidate> &candidates );
    void setPage( int page );
    void shiftPage( bool forward );
    void layoutWindow( int x, int y, int w, int h );
    void setIndex( int totalindex );
    void setIndexInPage( int index );

    void setQUimInputContext( QUimInputContext* m_ic ) { ic = m_ic; }

protected slots:
    void slotCandidateSelected( QListWidgetItem * item );

protected:
    void updateLabel();

protected:
    QUimInputContext *ic;

    QListWidget *cList;
    QLabel *numLabel;

    QList<uim_candidate> stores;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;

    bool isAlwaysLeft;
};

#endif /* Not def: _CANDIDATE_WINDOW_H_ */
