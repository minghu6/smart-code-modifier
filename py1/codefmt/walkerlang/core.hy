
;; trees: n branch tree (n >= 2)
;; trees: [trees1 trees2 trees3 ...]

;; do: exec core action method

;; findfirst
'(defmacro traceback [matcher tree]
    (if (do matcher tree)
        tree
        (collect [traceback trees])
    )
)

