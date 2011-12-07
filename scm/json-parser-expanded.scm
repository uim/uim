;; automatically generated from "json.scm.in"
;; see copyright notice in COPYING file

(define expanded:json-parser
  ((lambda ()
     (let ((any.1539 'undefined)
           (comment.1540 'undefined)
           (comment-body.1541 'undefined)
           (table-entries.1542 'undefined)
           (table-entries-nonempty.1543 'undefined)
           (table-entry.1544 'undefined)
           (array-entries.1545 'undefined)
           (array-entries-nonempty.1546 'undefined)
           (jstring.1547 'undefined)
           (jnumber.1548 'undefined)
           (number->utf-8.1549 'undefined)
           (decode-unicode-string.1550 'undefined)
           (white.1551 'undefined)
           (skip-comment-char.1552 'undefined)
           (skip-to-newline.1553 'undefined)
           (token.1554 'undefined)
           (interpret-string-escape.1555 'undefined)
           (jstring-body.1556 'undefined)
           (jnumber-body.1557 'undefined))
       (begin
         (set! any.1539
           (lambda (results.1558)
             (results->result
               results.1558
               'any
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      white.1551
                      (lambda (dummy.1559)
                        (packrat-check-base
                          '#\{
                          (lambda (dummy.1560)
                            (packrat-check
                              table-entries.1542
                              (lambda (entries.1561)
                                (packrat-check
                                  white.1551
                                  (lambda (dummy.1562)
                                    (packrat-check-base
                                      '#\}
                                      (lambda (dummy.1563)
                                        (lambda (results.1564)
                                          (make-result
                                            (list->vector entries.1561)
                                            results.1564))))))))))))
                    (packrat-or
                      (packrat-check
                        white.1551
                        (lambda (dummy.1565)
                          (packrat-check-base
                            '#\[
                            (lambda (dummy.1566)
                              (packrat-check
                                array-entries.1545
                                (lambda (entries.1567)
                                  (packrat-check
                                    white.1551
                                    (lambda (dummy.1568)
                                      (packrat-check-base
                                        '#\]
                                        (lambda (dummy.1569)
                                          (lambda (results.1570)
                                            (make-result entries.1567 results.1570))))))))))))
                      (packrat-or
                        (packrat-check
                          jstring.1547
                          (lambda (s.1571)
                            (lambda (results.1572)
                              (make-result s.1571 results.1572))))
                        (packrat-or
                          (packrat-check
                            jnumber.1548
                            (lambda (n.1573)
                              (lambda (results.1574)
                                (make-result n.1573 results.1574))))
                          (packrat-or
                            (packrat-check
                              white.1551
                              (lambda (dummy.1575)
                                (packrat-check
                                  (token.1554 '"true")
                                  (lambda (dummy.1576)
                                    (lambda (results.1577)
                                      (make-result '#t results.1577))))))
                            (packrat-or
                              (packrat-check
                                white.1551
                                (lambda (dummy.1578)
                                  (packrat-check
                                    (token.1554 '"false")
                                    (lambda (dummy.1579)
                                      (lambda (results.1580)
                                        (make-result '#f results.1580))))))
                              (packrat-check
                                white.1551
                                (lambda (dummy.1581)
                                  (packrat-check
                                    (token.1554 '"null")
                                    (lambda (dummy.1582)
                                      (lambda (results.1583)
                                        (make-result (void) results.1583))))))))))))
                  results.1558)))))
         (set! comment.1540
           (lambda (results.1584)
             (results->result
               results.1584
               'comment
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      (token.1554 '"/*")
                      (lambda (dummy.1585)
                        (packrat-check
                          comment-body.1541
                          (lambda (b.1586)
                            (lambda (results.1587)
                              (make-result b.1586 results.1587))))))
                    (packrat-or
                      (packrat-check
                        (token.1554 '"//")
                        (lambda (dummy.1588)
                          (packrat-check
                            skip-to-newline.1553
                            (lambda (b.1589)
                              (lambda (results.1590)
                                (make-result b.1589 results.1590))))))
                      (lambda (results.1591)
                        (make-result 'whitespace results.1591))))
                  results.1584)))))
         (set! comment-body.1541
           (lambda (results.1592)
             (results->result
               results.1592
               'comment-body
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      (token.1554 '"*/")
                      (lambda (dummy.1593)
                        (packrat-check
                          white.1551
                          (lambda (w.1594)
                            (lambda (results.1595)
                              (make-result w.1594 results.1595))))))
                    (packrat-check
                      skip-comment-char.1552
                      (lambda (dummy.1596)
                        (lambda (results.1597)
                          (make-result 'skipped-comment-char results.1597)))))
                  results.1592)))))
         (set! table-entries.1542
           (lambda (results.1598)
             (results->result
               results.1598
               'table-entries
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      table-entries-nonempty.1543
                      (lambda (a.1599)
                        (lambda (results.1600)
                          (make-result a.1599 results.1600))))
                    (lambda (results.1601) (make-result '() results.1601)))
                  results.1598)))))
         (set! table-entries-nonempty.1543
           (lambda (results.1602)
             (results->result
               results.1602
               'table-entries-nonempty
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      table-entry.1544
                      (lambda (entry.1603)
                        (packrat-check
                          white.1551
                          (lambda (dummy.1604)
                            (packrat-check-base
                              '#\,
                              (lambda (dummy.1605)
                                (packrat-check
                                  table-entries-nonempty.1543
                                  (lambda (entries.1606)
                                    (lambda (results.1607)
                                      (make-result
                                        (cons entry.1603 entries.1606)
                                        results.1607))))))))))
                    (packrat-check
                      table-entry.1544
                      (lambda (entry.1608)
                        (lambda (results.1609)
                          (make-result (list entry.1608) results.1609)))))
                  results.1602)))))
         (set! table-entry.1544
           (lambda (results.1610)
             (results->result
               results.1610
               'table-entry
               (lambda ()
                 ((packrat-check
                    jstring.1547
                    (lambda (key.1611)
                      (packrat-check
                        white.1551
                        (lambda (dummy.1612)
                          (packrat-check-base
                            '#\:
                            (lambda (dummy.1613)
                              (packrat-check
                                any.1539
                                (lambda (val.1614)
                                  (lambda (results.1615)
                                    (make-result
                                      (cons key.1611 val.1614)
                                      results.1615))))))))))
                  results.1610)))))
         (set! array-entries.1545
           (lambda (results.1616)
             (results->result
               results.1616
               'array-entries
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      array-entries-nonempty.1546
                      (lambda (a.1617)
                        (lambda (results.1618)
                          (make-result a.1617 results.1618))))
                    (lambda (results.1619) (make-result '() results.1619)))
                  results.1616)))))
         (set! array-entries-nonempty.1546
           (lambda (results.1620)
             (results->result
               results.1620
               'array-entries-nonempty
               (lambda ()
                 ((packrat-or
                    (packrat-check
                      any.1539
                      (lambda (entry.1621)
                        (packrat-check
                          white.1551
                          (lambda (dummy.1622)
                            (packrat-check-base
                              '#\,
                              (lambda (dummy.1623)
                                (packrat-check
                                  array-entries-nonempty.1546
                                  (lambda (entries.1624)
                                    (lambda (results.1625)
                                      (make-result
                                        (cons entry.1621 entries.1624)
                                        results.1625))))))))))
                    (packrat-check
                      any.1539
                      (lambda (entry.1626)
                        (lambda (results.1627)
                          (make-result (list entry.1626) results.1627)))))
                  results.1620)))))
         (set! jstring.1547
           (lambda (results.1628)
             (results->result
               results.1628
               'jstring
               (lambda ()
                 ((packrat-check
                    white.1551
                    (lambda (dummy.1629)
                      (packrat-check-base
                        '#\"
                        (lambda (dummy.1630)
                          (packrat-check
                            jstring-body.1556
                            (lambda (body.1631)
                              (packrat-check-base
                                '#\"
                                (lambda (dummy.1632)
                                  (lambda (results.1633)
                                    (make-result body.1631 results.1633))))))))))
                  results.1628)))))
         (set! jnumber.1548
           (lambda (results.1634)
             (results->result
               results.1634
               'jnumber
               (lambda ()
                 ((packrat-check
                    white.1551
                    (lambda (dummy.1635)
                      (packrat-check
                        jnumber-body.1557
                        (lambda (body.1636)
                          (lambda (results.1637)
                            (make-result body.1636 results.1637))))))
                  results.1634)))))
         (set! number->utf-8.1549
           (lambda (c.1638)
             (list->string
               (map integer->char
                    (if (< c.1638 '128)
                      (list c.1638)
                      (if (< c.1638 '2048)
                        (list (logior (/ c.1638 '64) '192)
                              (logior (logand c.1638 '63) '128))
                        (if (< c.1638 '65536)
                          (list (logior (/ c.1638 '4096) '224)
                                (logior (logand (/ c.1638 '64) '63) '128)
                                (logior (logand c.1638 '63) '128))
                          (if (< c.1638 '2097152)
                            (list (logior (/ c.1638 '262144) '240)
                                  (logior (logand (/ c.1638 '4096) '63) '128)
                                  (logior (logand (/ c.1638 '64) '63) '128)
                                  (logior (logand c.1638 '63) '128))
                            (list '35)))))))))
         (set! decode-unicode-string.1550
           (lambda (s.1639)
             (let ((hex->number.1640 'undefined))
               (begin
                 (set! hex->number.1640
                   (lambda (c.1641)
                     (assq-cdr
                       c.1641
                       '((#\0 . 0)
                         (#\1 . 1)
                         (#\2 . 2)
                         (#\3 . 3)
                         (#\4 . 4)
                         (#\5 . 5)
                         (#\6 . 6)
                         (#\7 . 7)
                         (#\8 . 8)
                         (#\9 . 9)
                         (#\a . 10)
                         (#\b . 11)
                         (#\c . 12)
                         (#\d . 13)
                         (#\e . 14)
                         (#\f . 15)
                         (#\A . 10)
                         (#\B . 11)
                         (#\C . 12)
                         (#\D . 13)
                         (#\E . 14)
                         (#\F . 15)))))
                 ((lambda (l.1642)
                    (number->utf-8.1549
                      (+ (* '4096 (hex->number.1640 (list-ref l.1642 '0)))
                         (* '256 (hex->number.1640 (list-ref l.1642 '1)))
                         (* '16 (hex->number.1640 (list-ref l.1642 '2)))
                         (hex->number.1640 (list-ref l.1642 '3)))))
                  (string->list s.1639))))))
         (set! white.1551
           (lambda (results.1643)
             (if (char-whitespace? (parse-results-token-value results.1643))
               (white.1551 (parse-results-next results.1643))
               (comment.1540 results.1643))))
         (set! skip-comment-char.1552
           (lambda (results.1644)
             (comment-body.1541 (parse-results-next results.1644))))
         (set! skip-to-newline.1553
           (lambda (results.1645)
             (if (memv (parse-results-token-value results.1645)
                       '(#\newline #\return))
               (white.1551 results.1645)
               (skip-to-newline.1553 (parse-results-next results.1645)))))
         (set! token.1554
           (lambda (str.1646)
             (lambda (starting-results.1647)
               ((let ((loop.1648 'undefined))
                  (begin
                    (set! loop.1648
                      (lambda (pos.1649 results.1650)
                        (if (= pos.1649 (string-length str.1646))
                          (make-result str.1646 results.1650)
                          (if (char=?
                                (parse-results-token-value results.1650)
                                (string-ref str.1646 pos.1649))
                            (loop.1648
                              (+ pos.1649 '1)
                              (parse-results-next results.1650))
                            (make-expected-result
                              (parse-results-position starting-results.1647)
                              str.1646)))))
                    loop.1648))
                '0
                starting-results.1647))))
         (set! interpret-string-escape.1555
           (lambda (results.1651 k.1652)
             ((lambda (ch.1653 results.1654)
                (if (char=? ch.1653 '#\u)
                  ((let ((doloop.1655 'undefined))
                     (begin
                       (set! doloop.1655
                         (lambda (i.1656 str.1657 results.1658)
                           (if (= i.1656 '4)
                             (k.1652
                               (decode-unicode-string.1550 str.1657)
                               results.1658)
                             (begin
                               (set! str.1657
                                 (string-append
                                   str.1657
                                   (string (parse-results-token-value results.1658))))
                               (doloop.1655
                                 (+ i.1656 '1)
                                 str.1657
                                 (parse-results-next results.1658))))))
                       doloop.1655))
                   '0
                   '""
                   results.1654)
                  (k.1652
                    ((lambda (t.1659) (if t.1659 (cdr t.1659) ch.1653))
                     (assv ch.1653
                           '((#\b . #\backspace)
                             (#\n . #\newline)
                             (#\f . #\page)
                             (#\r . #\return)
                             (#\t . #\tab))))
                    results.1654)))
              (parse-results-token-value results.1651)
              (parse-results-next results.1651))))
         (set! jstring-body.1556
           (lambda (results.1660)
             ((let ((loop.1661 'undefined))
                (begin
                  (set! loop.1661
                    (lambda (acc.1662 results.1663)
                      ((lambda (ch.1664)
                         ((lambda (t.1665)
                            (if (memv t.1665 '(#\\))
                              (interpret-string-escape.1555
                                (parse-results-next results.1663)
                                (lambda (val.1666 results.1667)
                                  (let ((new-acc.1668 'undefined))
                                    (begin
                                      (set! new-acc.1668
                                        (if (char? val.1666)
                                          (cons val.1666 acc.1662)
                                          (append-reverse!
                                            (string->list val.1666)
                                            acc.1662)))
                                      (loop.1661 new-acc.1668 results.1667)))))
                              (if (memv t.1665 '(#\"))
                                (make-result
                                  (list->string (reverse acc.1662))
                                  results.1663)
                                (loop.1661
                                  (cons ch.1664 acc.1662)
                                  (parse-results-next results.1663)))))
                          ch.1664))
                       (parse-results-token-value results.1663))))
                  loop.1661))
              '()
              results.1660)))
         (set! jnumber-body.1557
           (lambda (starting-results.1669)
             ((let ((loop.1670 'undefined))
                (begin
                  (set! loop.1670
                    (lambda (acc.1671 results.1672)
                      ((lambda (ch.1673)
                         (if (memv ch.1673
                                   '(#\-
                                     #\+
                                     #\0
                                     #\1
                                     #\2
                                     #\3
                                     #\4
                                     #\5
                                     #\6
                                     #\7
                                     #\8
                                     #\9
                                     #\.
                                     #\e
                                     #\E))
                           (loop.1670
                             (cons ch.1673 acc.1671)
                             (parse-results-next results.1672))
                           ((lambda (n.1674)
                              (if n.1674
                                (make-result n.1674 results.1672)
                                (make-expected-result
                                  (parse-results-position starting-results.1669)
                                  'number)))
                            (string->number
                              (list->string (reverse acc.1671))))))
                       (parse-results-token-value results.1672))))
                  loop.1670))
              '()
              starting-results.1669)))
         any.1539)))))
