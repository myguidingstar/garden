(ns garden.selectors
  "Macros and functions for working with CSS selectors."
  (:refer-clojure :exclude [map meta not #+clj time #+clj var
                            empty first])
  (:require [garden.protocols :as p])
  #+cljs
  (:require-macros [garden.selectors :refer :all])
  #+clj
  (:import clojure.lang.Keyword
           clojure.lang.Symbol
           clojure.lang.IFn))

;; TODO:
;; * Implement +, ~, and > combinators
;;   SEE: http://www.w3.org/TR/selectors/#combinators
;; * Generic defselector (for prefixes)
;; * Account for meta in macros
;; * Function for computing specificity
;;   SEE: http://www.w3.org/TR/selectors/#specificity

(extend-protocol p/ICssSelector
  #+clj String
  #+cljs string
  (selector [this] this)

  Keyword
  (selector [this] (name this))

  Symbol
  (selector [this] (name this)))

;;----------------------------------------------------------------------
;; Macro helpers

#+clj
(defn- do-selector-fn []
  (let [fn-sym (gensym "f")
        arg (gensym "x")
        invoke-specs (clojure.core/map
                      (fn [i syms]
                        (let [argslist (vec (take i syms))
                              body `(~fn-sym (str (p/selector ~arg)
                                                  ~@(clojure.core/map
                                                     (fn [sym]
                                                       `(p/selector ~sym))
                                                     (rest argslist))))]
                          `(~'invoke ~argslist ~body)))
                      (range 1 22)
                      (repeat '[_ a b c d e f g h i j k l m n o p q r s t]))
        impl `(reify
                p/ICssSelector
                (~'selector [~'_]
                  (p/selector ~arg))
                IFn
                ~@invoke-specs)]
    `(fn ~fn-sym [~arg] ~impl)))

;;----------------------------------------------------------------------
;; Macros

(defmacro deftypeselector
  "Define a reified instance named sym which satisfies
  clojure.lang.IFn and garden.protocols.ICssSelector for creating a
  CSS type selector. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Example:

    (deftypeselector a)
    ;; => #'user/a
    (a \":hover\")
    ;; => #<selectors$f16521$reify__16523 garden.selectors$f16521$reify__16523@63be555>
    (p/selector a)
    ;; => \"a\"
    (p/selector (a \":hover\"))
    ;; => \"a:hover\"

    ;; Where p/selector is garden.protocols/selector
  "
  [sym]
  `(def ~sym (~(do-selector-fn) ~(name sym))))


(defmacro defpseudoclass
  "Define a reified instance named sym which satisfies
  clojure.lang.IFn and garden.protocols.ICssSelector for creating a CSS
  pseudo class. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Example:

    (deftypeselector a)
    ;; => #'user/a
    (defpseudoclass hover)
    ;; => #'user/hover
    (hover)
    ;; => #<selectors$f16962$reify__16964 garden.selectors$f16962$reify__16964@520f79c5>
    (p/selector (a hover))
    ;; => \"a:hover\"

    ;; Where p/selector is garden.protocols/selector
  "
  [sym]
  `(def ~sym (~(selector-fn) ~(str \: (name sym)))))


(defmacro defpseudoelement
  "Define a reified instance named sym which satisfies
  clojure.lang.IFn and garden.protocols.ICssSelector for creating a CSS
  pseudo element. This instance doubles as both a function and a
  literal (when passed to garden.protocols/selector). When the
  function is called it will return a new instance that pocesses the
  same property. All arguments to the function must satisfy
  garden.protocols.ICssSelector.

  Example:

    (deftypeselector p)
    ;; => #'user/p
    (defpseudoelement first-letter)
    ;; => #'user/first-letter
    (first-letter)
    ;; => #<selectors$f22178$reify__22180 garden.selectors$f22178$reify__22180@1c82315b>
    (p/selector (p first-letter))
    ;; => \"p::first-letter\"

    ;; Where p/selector is garden.protocols/selector
  "
  [sym]
  `(def ~sym (~(selector-fn) ~(str "::" (name sym)))))


(defmacro defstructuralpseudoclass
  "Define a function named sym for creating a CSS Structural
  pseudo-classes selector. When called the function returns a reified
  instance which satisfies clojure.lang.IFn and
  garden.protocols.ICssSelector. The return value of fn-tail is used to
  format the argument portion of the selector and must satisfy
  garden.protocols.ICssSelector.

  Example:

    (defstructuralpseudoclass not [x]
      (p/selector x))
    ;; => #'user/not
    (p/selector (not \"a\"))
    ;; => #<selectors$not$f20254$reify__20257 garden.selectors$not$f20254$reify__20257@35f4fd63>
    (p/selector ((not \"a\") \"hover\"))
    ;; => \":not(a)hover\"

    ;; Where p/selector is garden.protocols/selector
  "
  [sym & fn-tail]
  (let [fmt (str \: (name sym) "(%s)")
        fn1 `(fn ~fn-tail)]
    `(defn ~sym [& args#]
       (~(selector-fn) (format ~fmt
                               (p/selector (apply ~fn1 args#)))))))

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
  `(deftypeselector ~tag))

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

(defstructuralpseudoclass lang [language]
  (name language))

(defstructuralpseudoclass not [selector]
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

(defstructuralpseudoclass nth-child [x] (nth-x x))
(defstructuralpseudoclass nth-last-child [x] (nth-x x))
(defstructuralpseudoclass nth-of-type [x] (nth-x x))
(defstructuralpseudoclass nth-last-of-type [x] (nth-x x))

;;----------------------------------------------------------------------
;; Pseudo elements

(defpseudoelement after)
(defpseudoelement before)
(defpseudoelement first-letter)
(defpseudoelement first-line)

;;----------------------------------------------------------------------
;; Special selectors

(deftypeselector
  ^{:doc "Parent selector."}
  &)
