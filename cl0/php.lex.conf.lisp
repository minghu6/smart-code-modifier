((:extra-type-fun . (
                     (varp (*digits* *ascii-letters* #\_))
                     (identityp (*digits* *ascii-letters* (#\_ #\\ )))
                     (identity-head-p (*ascii-letters* (#\_ #\\ )))
                     (phpp (#\p #\h))
                     (delimiterp (#\, #\;))
                     (numberchar-sign-p (#\+ #\-))
                     (operatorp (#\+
                                 #\-
                                 #\*
                                 #\/
                                 #\%
                                 #\^
                                 #\|
                                 #\&
                                 #\~
                                 #\!
                                 #\?
                                 #\:
                                 #\=
                                 #\@
                                 #\>
                                 #\<
                                 #\.
                                 #\$))
                     ))
 ;; priority based order
 (:lex-dfa-map .
               ((blank-state .((slash . (comment-start-state t))
                               (ngbs . (blank-state nil))
                               (<     . (tag-start-state t))
                               (identity-head . (identity-name-state t))
                               (parenthesis . (parenthesis-state t))
                               (delimiter . (delimiter-state t))
                               (singlequote . (singlequote-string-state t))
                               (doublequote . (doublequote-string-state t))

                               ;; deal with different number iterals
                               (zerochar . (numberzero-head-state t))
                               (numberchar . (numberchar-state t))
                               (numberchar-sign . (numberchar-0-state t))

                               (operator . (operator-state t))

                               ))
                (identity-start-state . (
                                         (identity-head . (identity-name-state nil))
                                         ))
                ;; SingleQuote
                (singlequote-string-state . (
                                             (anychar-but-backslash-singlequote .
                                              (singlequote-string-state nil))
                                             (backslash . (singlequote-string-backslash-state nil))
                                             (singlequote . (singlequote-end-state nil))
                                             ))
                (singlequote-end-state . (
                                          (ngbs . (blank-state t))
                                          (delimiter . (delimiter-state t))
                                          (parenthesis . (parenthesis-state t))
                                          (colon . (delimiter-state t))
                                          ))
                (singlequote-string-backslash-state . ((anychar . (singlequote-string-state nil))))

                ;; Delimiter: #\, #\;
                (delimiter-state . (
                                    (ngbs . (blank-state t))
                                    (slash . (comment-start-state t))
                                    (delimiter . (delimiter-state t))
                                    (identity-head . (identity-name-state t))
                                    ))


                (parenthesis-state . ((parenthesis . (parenthesis-state t))
                                      (ngbs . (blank-state t))
                                      (delimiter . (delimiter-state t))
                                      (identity-head . (identity-name-state t))
                                      (singlequote . (singlequote-string-state t))
                                      (doublequote . (doublequote-string-state t))

                                      (zerochar . (numberzero-head-state t))
                                      (numberchar . (numberchar-state t))
                                      (numberchar-sign . (numberchar-0-state t))

                                      (slash . (comment-start-state t))
                                      (operator . (operator-state t))
                                      ))
                ;; DoubleQuote
                (doublequote-string-state . (
                                             (anychar-but-backslash-doublequote .
                                              (doublequote-string-state nil))
                                             (backslash . (doublequote-string-backslash-state nil))
                                             (doublequote . (doublequote-end-state nil))
                                             ))
                (doublequote-end-state . (
                                          (ngbs . (blank-state t))
                                          (delimiter . (delimiter-state t))
                                          (parenthesis . (parenthesis-state t))
                                          ))
                (doublequote-string-backslash-state . ((anychar . (doublequote-string-state nil))))


                (tag-start-state . ((question . (tag-start-naming-state nil))
                                    (operator . (operator-state nil))

                                    (zerochar . (numberzero-head-state t))
                                    (numberchar . (numberchar-state t))
                                    (numberchar-sign . (numberchar-0-state t))

                                    (ngbs . (blank-state t))
                                    (identity-head . (identity-name-state t))

                                    ))
                (tag-start-naming-state . ((php . (php-tagname-state nil))))
                (php-tagname-state . ((php . (php-tagname-state nil))
                                      (ngbs . (blank-state t))))

                (identity-name-state . ((identity . (identity-name-state nil))
                                        (ngbs . (blank-state t))
                                        (parenthesis . (blank-state t))
                                        (delimiter . (delimiter-state t))
                                        (operator . (operator-state t))
                                        ))
                (operator-state . ((operator . (operator-state nil))

                                   (zerochar . (numberzero-head-state t))
                                   (numberchar . (numberchar-state t))
                                   (numberchar-sign . (numberchar-0-state t))

                                   (ngbs . (blank-state t))
                                   (identity-head . (identity-name-state t))
                                   (delimiter . (delimiter-state t))
                                   (parenthesis . (parenthesis-state t))
                                   ))
                (numberchar-0-state . ((zerochar . (numberzero-head-state nil))
                                       (numberchar . (numberchar-state nil))
                                       (parenthesis . (blank-state t))
                                       (delimiter . (delimiter-state t))
                                       (ngbs . (blank-state t))
                                       (operator . (operator-state t))
                                       ))
                (numberzero-head-state . ((x . (hex-numberchar-state nil))
                                          (numberchar . (numberchar-state nil))
                                          (parenthesis . (blank-state t))
                                          (delimiter . (delimiter-state t))
                                          (ngbs . (blank-state t))
                                          (operator . (operator-state t))
                                          ))
                (hex-numberchar-state . ((hex-numberchar . (hex-numberchar-state nil))
                                         (parenthesis . (blank-state t))
                                         (delimiter . (delimiter-state t))
                                         (ngbs . (blank-state t))
                                         (operator . (operator-state t))
                                         ))
                (numberchar-state . ((numberchar . (numberchar-state nil))
                                     (parenthesis . (blank-state t))
                                     (delimiter . (delimiter-state t))
                                     (ngbs . (blank-state t))
                                     (operator . (operator-state t))
                                     ))

                (comment-start-state . ((slash . (line-comment-state nil))
                                        (asterisk . (block-comment-state nil))
                                        (ngbs . (blank-state t))
                                        (equalchar . (blank-state t))
                                        ))

                (block-comment-state . ((asterisk .(block-comment-end-state nil))
                                        (anychar-but-asterisk . (block-comment-state nil))
                                        ))
                (block-comment-end-state . ((anychar-but-asterisk-slash . (block-comment-state nil))
                                            (asterisk . (block-comment-end-state nil))
                                            (slash . (block-comment-end-phase2-state nil))
                                            ))
                (block-comment-end-phase2-state . ((ngbs . (blank-state t))
                                                   (slash . (comment-start-state t))
                                                   (<     . (tag-start-state t))
                                                   (identity-head . (identity-name-state t))
                                                   (parenthesis . (parenthesis-state t))
                                                   (delimiter . (delimiter-state t))
                                                   (singlequote . (singlequote-string-state t))
                                                   (doublequote . (doublequote-string-state t))

                                                   ;; deal with different number iterals
                                                   (zerochar . (numberzero-head-state t))
                                                   (numberchar . (numberchar-state t))
                                                   (numberchar-sign . (numberchar-0-state t))

                                                   (operator . (operator-state t))
                                                   ))
                (line-comment-state . ((anychar-but-newline . (line-comment-state nil))
                                       (newline . (blank-state t))
                                       ))
                )
               ))
