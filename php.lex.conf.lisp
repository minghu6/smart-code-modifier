((:extra-type-fun . ((var-head-p #\$)
                     (phpp (#\p #\h #\p))
                     ))
 (:lex-dfa-map . ((blank-state .((slash . (comment-start-state t))
                                 (whitespace . (blank-state nil))
                                 (<     . (tag-start-state t))
                                 (identity . (identity-name-state t))
                                 ))
                  (tag-start-state . ((question . (tag-start-naming-state nil))
                                      ))
                  (tag-start-naming-state . ((php . (php-tagname-state nil))))
                  (php-tagname-state . ((php . (php-tagname-state nil))
                                        (whitespace . (blank-state t))))

                  (identity-name-state . ((identity . (identity-name-state nil))
                                          (whitespace . (blank-state t))))

                  (comment-start-state . ((slash . (line-comment-state t))
                                          (asterisk . (block-comment-state t))))
                  (block-comment-state . ((anychar-but-asterisk-slash .
                                                                      (block-comment-end-state nil))))
                  (block-comment-end-state . ((anychar-but-slash . (block-comment-state nil))))
                  (line-comment-state . (()))
                  )
               ))
