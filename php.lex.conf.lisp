((:extra-type-fun . ((var-head-p #\$)
                     (phpp (#\p #\h))
                     ))
 (:lex-dfa-map .
               ((blank-state .((slash . (comment-start-state t))
                               (whitespace . (blank-state nil))
                               (<     . (tag-start-state t))
                               (var-head . (identity-start-state t))
                               (identity-head . (identity-name-state t))
                               (parenthesis . (parenthesis-state t))
                               (semicolon . (delimiter-state t))
                               (singlequote . (singlequote-string-state t))
                               (doublequote . (doublequote-string-state t))
                               (numberchar-head . (numberchar-state t))
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
                                          (whitespace . (blank-state t))
                                          (comma . (delimiter-state t))
                                          (parenthesis . (parenthesis-state t))

                                          ))
                (singlequote-string-backslash-state . ((anychar . (singlequote-string-state nil))))

                ;; Delimiter: #\, #\;
                (delimiter-state . (
                                    (whitespace . (blank-state t))
                                    ))


                (parenthesis-state . (
                                      (whitespace . (blank-state t))
                                      (comma . (delimiter-state t))
                                      (semicolon . (delimiter-state t))
                                      (var-head . (identity-start-state t))
                                      (identity-head . (identity-name-state t))
                                      (singlequote . (singlequote-string-state t))
                                      (doublequote . (doublequote-string-state t))
                                      (numberchar-head . (numberchar-state t))
                                      ))
                ;; DoubleQuote
                (doublequote-string-state . (
                                             (anychar-but-backslash-doublequote .
                                                                                (doublequote-string-state nil))
                                             (backslash . (doublequote-string-backslash-state nil))
                                             (doublequote . (doublequote-end-state nil))
                                             ))
                (doublequote-end-state . (
                                          (whitespace . (blank-state t))
                                          (comma . (delimiter-state t))
                                          (parenthesis . (parenthesis-state t))
                                          ))
                (doublequote-string-backslash-state . ((anychar . (doublequote-string-state nil))))


                (tag-start-state . ((question . (tag-start-naming-state nil))
                                    ))
                (tag-start-naming-state . ((php . (php-tagname-state nil))))
                (php-tagname-state . ((php . (php-tagname-state nil))
                                      (whitespace . (blank-state t))))

                (identity-name-state . ((identity . (identity-name-state nil))
                                        (whitespace . (blank-state t))
                                        (parenthesis . (blank-state t))
                                        (semicolon . (blank-state t))
                                        (operator . (operator-state t))
                                        ))
                (operator-state . ((operator . (operator-state nil))
                                   (numberchar-head . (numberchar-state t))
                                   (whitespace . (blank-state t))
                                   (identity-head . (identity-name-state t))
                                   
                                   ))

                (numberchar-state . ((numberchar . (numberchar-state nil))
                                     (parenthesis . (blank-state t))
                                     (comma . (delimiter-state t))
                                     ))

                (comment-start-state . ((slash . (line-comment-state nil))
                                        (asterisk . (block-comment-state nil))))
                (block-comment-state . ((asterisk .(block-comment-end-state nil))
                                        (anychar-but-asterisk . (block-comment-state nil))
                                        ))
                (block-comment-end-state . ((anychar-but-asterisk-slash . (block-comment-state nil))
                                            (asterisk . (block-comment-end-state nil))
                                            (slash . (block-comment-end-phase2-state nil))
                                            ))
                (block-comment-end-phase2-state . (
                                                   (whitespace . (blank-state t))
                                                   ))
                (line-comment-state . (()))
                )
               ))
