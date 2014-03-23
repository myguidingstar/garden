(ns garden.selectors
  "Macros and functions for working with CSS selectors."
  (:refer-clojure :exclude [+ - > map meta not #+clj time #+clj var
                            empty first])
  (:require [garden.protocols :as p]
            [clojure.string :as string])
  #+cljs
  (:require-macros [garden.selectors :refer [defselector
                                             defid
                                             defclass
                                             defpseudoclass
                                             defpseudoelement
                                             gen-type-selector-defs
                                             gen-pseudo-class-defs]])
  #+clj
  (:import clojure.lang.Keyword
           clojure.lang.Symbol
           clojure.lang.IFn))

(defn selector? [x]
  (satisfies? p/ICssSelector x))

;; TODO:
;; * Account for meta in macros

(extend-protocol p/ICssSelector
  #+clj String
  #+cljs string
  (selector [this] this)

  Keyword
  (selector [this] (name this))

  Symbol
  (selector [this] (name this)))

(deftype CssSelector [x]
  p/ICssSelector
  (selector [_]
    (p/selector x))

  IFn
  (invoke [_]
    (CssSelector. (str (p/selector x))))
  (invoke [_ a]
    (CssSelector. (str (p/selector x)
                       (p/selector a))))
  (invoke [_ a b]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b))))
  (invoke [_ a b c]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c))))
  (invoke [_ a b c d]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d))))
  (invoke [_ a b c d e]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e))))
  (invoke [_ a b c d e f]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f))))
  (invoke [_ a b c d e f g]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g))))
  (invoke [_ a b c d e f g h]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h))))
  (invoke [_ a b c d e f g h i]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i))))
  (invoke [_ a b c d e f g h i j]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j))))
  (invoke [_ a b c d e f g h i j k]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k))))
  (invoke [_ a b c d e f g h i j k l]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l))))
  (invoke [_ a b c d e f g h i j k l m]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m))))
  (invoke [_ a b c d e f g h i j k l m n]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n))))
  (invoke [_ a b c d e f g h i j k l m n o]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o))))
  (invoke [_ a b c d e f g h i j k l m n o p]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o)
                       (p/selector p))))
  (invoke [_ a b c d e f g h i j k l m n o p q]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o)
                       (p/selector p)
                       (p/selector q))))
  (invoke [_ a b c d e f g h i j k l m n o p q r]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o)
                       (p/selector p)
                       (p/selector q)
                       (p/selector r))))
  (invoke [_ a b c d e f g h i j k l m n o p q r s]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o)
                       (p/selector p)
                       (p/selector q)
                       (p/selector r)
                       (p/selector s))))
  (invoke [_ a b c d e f g h i j k l m n o p q r s t]
    (CssSelector. (str (p/selector x)
                       (p/selector a)
                       (p/selector b)
                       (p/selector c)
                       (p/selector d)
                       (p/selector e)
                       (p/selector f)
                       (p/selector g)
                       (p/selector h)
                       (p/selector i)
                       (p/selector j)
                       (p/selector k)
                       (p/selector l)
                       (p/selector m)
                       (p/selector n)
                       (p/selector o)
                       (p/selector p)
                       (p/selector q)
                       (p/selector r)
                       (p/selector s)
                       (p/selector t)))))

(defn selector [x]
  (CssSelector. x))

;;----------------------------------------------------------------------
;; Macros

(defmacro defselector
  "Define an instance of CssSelector named sym for creating a
  CSS type selector. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Example:

    (defselector a)
    ;; => #'user/a
    (a \":hover\")
    ;; => #<CssSelector garden.selectors.CssSelector@7c42c2a9>
    (p/selector a)
    ;; => \"a\"
    (p/selector (a \":hover\"))
    ;; => \"a:hover\"

    ;; Where p/selector is garden.protocols/selector
  "
  ([sym]
     `(def ~sym (selector ~(name sym))))
  ([sym strval]
     `(def ~sym (selector ~strval))))

(defmacro defclass [sym]
  `(defselector ~sym ~(str "." (name sym))))

(defmacro defid [sym]
  `(defselector ~sym ~(str "#" (name sym))))

(defmacro defpseudoclass
  "Define an instance of CssSelector named sym for creating a CSS
  pseudo class. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Optionally fn-tail may be passed to create a structual pseudo class.

  Example:

    (defselector a)
    ;; => #'user/a
    (defpseudoclass hover)
    ;; => #'user/hover
    (hover)
    ;; => #<CssSelector garden.selectors.CssSelector@2a0ca6e1>
    (p/selector (a hover))
    ;; => \"a:hover\"

  Example:

    (defpseudoclass not [x]
      (p/selector x))
    ;; => #'user/not
    (p/selector (a hover (not \"span\"))
    ;; => a:hover:not(span)

    ;; Where p/selector is garden.protocols/selector
  "
  [sym & fn-tail]
  (if (seq fn-tail)
    (let [fn1 `(fn ~fn-tail)]
      `(defn ~sym [& args#]
         (selector (str \: ~(name sym) "(" (p/selector (apply ~fn1 args#)) ")"))))
    `(defselector ~sym ~(str \: (name sym)))))

(defmacro defpseudoelement
  "Define an instance of CssSelector named sym for creating a CSS
  pseudo element. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Example:

    (defselector p)
    ;; => #'user/p
    (defpseudoelement first-letter)
    ;; => #'user/first-letter
    (first-letter)
    ;; => #<CssSelector garden.selectors.CssSelector@20aef718>
    (p/selector (p first-letter))
    ;; => \"p::first-letter\"

    ;; Where p/selector is garden.protocols/selector
  "
  [sym]
  `(defselector ~sym ~(str "::" (name sym))))

;;----------------------------------------------------------------------
;; Type selectors classes

(def ^:private html-tags
  '[a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    command
    datalist
    dd
    del
    details
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hgroup
    hr
    html
    i
    iframe
    img
    input
    ins
    kbd
    keygen
    label
    legend
    li
    link
    map
    mark
    math
    menu
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    option
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    svg
    table
    tbody
    td
    textarea
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr])

#+clj
(defn- gen-type-selector-def [tag]
  `(defselector ~tag))

(defmacro ^:private gen-type-selector-defs []
  `(do
     ~@(for [t html-tags]
         (gen-type-selector-def t))))

(gen-type-selector-defs)

;;----------------------------------------------------------------------
;; Pseudo classes

(def ^:private pseudo-classes
  '[active
    checked
    default
    disabled
    empty
    enabled
    first
    first-child
    first-of-type
    fullscreen
    focus
    hover
    indeterminate
    in-range
    invalid
    last-child
    last-of-type
    left
    link
    only-child
    only-of-type
    optional
    out-of-range
    read-only
    read-write
    required
    right
    root
    scope
    target
    valid
    visited])

#+clj
(defn- gen-pseudo-class-def [p]
  `(defpseudoclass ~p))

(defmacro ^:private gen-pseudo-class-defs []
  `(do
     ~@(for [p pseudo-classes]
         (gen-pseudo-class-def p))))

(gen-pseudo-class-defs)

;;----------------------------------------------------------------------
;; Structural pseudo classes

(defpseudoclass lang [language]
  (name language))

(defpseudoclass not [selector]
  (p/selector selector))

;; SEE: http://www.w3.org/TR/selectors/#nth-child-pseudo
(def nth-child-re
  #+clj
  #"\s*(?i:[-+]?\d+n\s*(?:[-+]\s*\d+)?|[-+]?\d+|odd|even)\s*"
  #+cljs
  (js/RegExp. "\\s*(?:[-+]?\\d+n\\s*(?:[-+]\\s*\\d+)?|[-+]?\\d+|odd|even)\\s*"
              "i"))

(defn nth-x
  "nth-child helper."
  [x]
  (assert (or (string? x) (keyword? x) (symbol? x))
          "Agument must be a string, keyword, or symbol")
  (let [s (name x)]
    (if-let [m (re-matches nth-child-re s)]
      m
      (throw
       (#+clj
        IllegalArgumentException.
        #+cljs
        js/Error.
        (str "Invalid value " (pr-str s)))))))

(defpseudoclass nth-child [x] (nth-x x))
(defpseudoclass nth-last-child [x] (nth-x x))
(defpseudoclass nth-of-type [x] (nth-x x))
(defpseudoclass nth-last-of-type [x] (nth-x x))

;;----------------------------------------------------------------------
;; Pseudo elements

(defpseudoelement after)
(defpseudoelement before)
(defpseudoelement first-letter)
(defpseudoelement first-line)

;;----------------------------------------------------------------------
;; Attribute selectors

;; SEE: http://www.w3.org/TR/selectors/#attribute-selectors

(defn attr
  ([attr-name]
     (selector (str \[ (name attr-name) \])))
  ([attr-name op attr-value]
     (let [v (name attr-value)
           ;; Wrap the value in quotes unless it's already
           ;; quoted to prevent emitting bad selectors. 
           v (if (re-matches #"\"(\\|[^\"])*\"|'(\\|[^\'])*'" v)
               v
               (pr-str v))]
       (selector (str \[ (name attr-name) (name op) v \])))))

(defn attr= [attr-name attr-value]
  (attr attr-name "=" attr-value))

(defn attr-contains [attr-name attr-value]
  (attr attr-name "~=" attr-value))

(defn attr-begins-with [attr-name attr-value]
  (attr attr-name "^=" attr-value))

;; TODO: This needs a better name.
(defn attr-begins-with* [attr-name attr-value]
  (attr attr-name "|=" attr-value))

(def attr-starts-with attr-begins-with)
(def attr-starts-with* attr-begins-with*)

(defn attr-ends-with [attr-name attr-value]
  (attr attr-name "$=" attr-value))

(defn attr-matches [attr-name attr-value]
  (attr attr-name "*=" attr-value))

;;----------------------------------------------------------------------
;; Selectors combinators

;; SEE: http://www.w3.org/TR/selectors/#combinators

(defn +
  "Adjacent sibling combinator."
  [a b]
  (str (p/selector a) " + " (p/selector b)))

(defn -
  "General sibling combinator."
  [a b]
  (str (p/selector a) " ~ " (p/selector b)))

(defn >
  "Child combinator."
  ([a]
     (p/selector a))
  ([a b]
     (str (p/selector a) " > " (p/selector b)))
  ([a b & more]
     (reduce > (> a b) more)))

;;----------------------------------------------------------------------
;; Special selectors

(defselector
  ^{:doc "Parent selector."}
  &)

;;----------------------------------------------------------------------
;; Specificity

;; SEE: http://www.w3.org/TR/selectors/#specificity

(defn- lex-specificity [s]
  (let [id-selector-re #"^\#[a-zA-Z][\w-]*"
        class-selector-re #"^\.[a-zA-Z][\w-]*"
        attribute-selector-re #"^\[[^\]]*\]"
        type-selector-re #"^[a-zA-Z][\w-]"
        pseudo-class-re #"^:[a-zA-Z][\w-]*(?:\([^\)]+\))?"
        pseudo-element-re #"^::[a-zA-Z][\w-]*"]
    (some
     (fn [[re k]]
       (if-let [m (re-find re s)]
         [m k]))
     [[id-selector-re :a]
      [class-selector-re :b]
      [attribute-selector-re :b]
      [pseudo-class-re :b]
      [type-selector-re :c]
      [pseudo-element-re :c]])))

(defn- specificity* [selector]
  (let [s (p/selector selector)
        score {:a 0 :b 0 :c 0}]
    (loop [s s, score score]
      (if (empty? s)
        score
        (if-let [[m k] (lex-specificity s)]
          ;; The negation pseudo class is a special case.
          (if-let [[_ inner] (re-find #"^:not\(([^\)]*)\)" m)]
            (recur (subs s (count m))
                   (merge-with clojure.core/+ score (specificity* inner)))
            (recur (subs s (count m)) (update-in score [k] inc)))
          (recur (subs s 1) score))))))

(defn specificity
  "Calculate a CSS3 selector's specificity.
  
  Example:

    (specificity \"#s12:not(FOO)\")
    ;; => 101
    (specificity (a hover))
    ;; => 10
  " 
  [selector]
  (let [{:keys [a b c]} (specificity* selector)
        sv (string/replace (str a b c) #"^0*" "")]
    (if (empty? sv)
      0
      #+clj (Integer. sv)
      #+cljs (js/parseInt sv))))
