(import [string [digits ascii_letters ascii_lowercase]])

(import [hy.models [HySymbol HyKeyword]])

(import [common [parse-error-info ParseError]])
;;; 正则去掉 <?php tag 获取内容

(setv keyword-dict ascii_lowercase)
(setv identifier-dict (+ "_" digits ascii_letters))
(setv identifier-start-dict (+ "_" digits))
(setv blank-dict (+ " " "\t" "\n"))
(setv delimiter-dict (+ ";" "[" "]" "(" ")" "{" "}"))
(setv operator-dict (+
                        "+"
                        "-"
                        "*"
                        "/"
                        "%"
                        "="
                        "&"
                        "|"
                        "^"
                        "~"
                        "<"
                        ">"))
(defn var-start? [c] (= c "$"))
(defn identifier? [c] (in c identifier-dict))
(defn identifier-start? [c] (in c identifier-start-dict))
; (defn keyword? [c] (in c keyword-dict))
(defn blank? [c] (in c blank-dict))
(defn asterisk? [c] (= c "*"))
(defn slash? [c] (= c "/"))
(defn backslash? [c] (= c "\\"))
(defn delimiter? [c] (in c delimiter-dict))
(defn operator? [c] (in c operator-dict))
(defn singlequote? [c] (= c "'"))
(defn doublequote? [c] (= c "\""))
(defn any-but-newline? [c] (!= c "\n"))
(defn newline? [c] (= c "\n"))
(defn any-but-asterisk-slash? [c] (not (in c "*" "/")))
(defn any-but-slash [c] (!= c "/"))



(defclass UnrecognizedTokenError [ParseError])



(defn regularization [c]
    (cond [(var-start? c) :var-start]
          [(identifier? c) :identifier]
          [(identifier-start? c) :identifier-start]
        ;   [(keyword? c) :keyword]
          [(asterisk? c) :asterisk]
          [(slash? c) :slash]
          [(back-slash? c) :back-slash]
          [(blank? c) :blank]
          [(delimiter? c) :delimiter]
          [(operator? c): :operator]
          [(operator-single-quote? c) :single-quote]
          [(operator-double-quote? c) :double-quote]
          [True (raise (UnrecognizedChar c))]))


(defmacro! state-reg-conds [state-output]
    `(cond ~@(lfor [key value] (.items state-output) `[(~(HySymbol (+ (name key) "?")) c) ~value])))


(defn map-state [lex-dfa-map]
    (lfor state-output (.values lex-dfa-map)
        (.update state-output
            {:regularization (fn [c]  (state-reg-conds {:identifier-start :blank}))})
))


(map-state (setx lex-dfa-map {
    :blank {
        :identifier-start :identifier
        :var-start :var
        :operator :blank  ; 对于状态转移函数来说，operator 和 blank 节点完全一样
        ; :keyword :keyword
        :slash :comment-start
        :singlequote :singlequote-string
        :doublequote :doublequote-string
        :delimiter :blank
    }

    :identifier {
        :identifier :identifier
        :blank :blank
        :slash :comment
        :delimiter :blank
        :operator :operator
    }

    :var {
        :var-start :var
        :identifier-start :identifier
        :blank :blank
    }

    :comment-start {
        :slash :doubleslash-comment
        :asterisk :asterisk-comment
        :any-but-slash-asterisk :blank
    }

    :doubleslash-comment {
        :any-but-newline :double-slash-comment
        :newline :blank
    }

    :asterisk-comment {
        :asterisk :asterisk-comment-end-start
    }

    :asterisk-comment-end-start {
        :slash :blank
        :any-but-slash :asterisk-comment
    }

    :singlequote-string {
        :any-but-backslash-singlequote :singlequote-string
        :backslash :singlequote-string-prevent-eval
        :singlequote :back
    }

    :singlequote-string-prevent-eval {
        :any :singlequote-string
    }

    :doublequote-string {
        :any-but-backslash-doublequote :doublequote-string
        :backslash :doublequote-string-prevent-eval
        :doublequote :blank
    }

    :doublequote-string-prevent-eval {
        :any :doublequote-string
    }
}))



;(gen-state-reg lex-dfa-map)

; (defn run [src]
;     (setv state (get lex-dfa-map :blank))
;     (setv lineno 1
;           colno  0)
;     (setv token '')
;     (for [char src]
;         (if (= char "\n") (setv lineno (inc lineno)
;                                 colno 0)
;             (setv colno (inc colno)))
;         #_(get state )
;         )
; )

(defmain [&rest _]
    (import [os [ path ]] [pprint [pprint]])
    (setv src (.read (open "./draft/phpmyadmin/examples/openid.php")))
    #_(print src)
    (setv f (get (get lex-dfa-map :blank) :regularization))
    (pprint (f "d"))
    (lfor [k v] (.items lex-dfa-map) (print k v))
    )