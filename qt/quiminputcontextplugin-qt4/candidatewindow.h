#ifndef _CANDIDATE_WINDOW_H_
#define _CANDIDATE_WINDOW_H_

class QUimInputContext;

#include <uim/uim.h>

#include <qvbox.h>

class QLabel;

class QUimInputContext;

class CandidateWindow : public QVBox {
    Q_OBJECT

public:
    CandidateWindow(QWidget * parent = 0);
    ~CandidateWindow();

    void activateCandwin(int dLimit);
    void deactivateCandwin();
    void clearCandidates();
    void popup();

    void setCandidates(int displayLimit, const QList<uim_candidate> &candidates);
    void setPage(int page);
    void shiftPage(bool forward);
    void layoutWindow(int x, int y, int w, int h);
    void setIndex(int totalindex);
    void setIndexInPage(int index);

    void setQUimInputContext(QUimInputContext* m_ic){ ic = m_ic; }

protected:
    void updateLabel();

protected:
    QUimInputContext *ic;

    QLabel *numLabel;

    QList<uim_candidate> stores;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;
};

#endif /* Not def: _CANDIDATE_WINDOW_H_ */
