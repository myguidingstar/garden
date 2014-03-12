(ns garden.selectors
  (:refer-clojure :exclude [map meta not #+clj time #+clj var
                            empty first])
  (:require [garden.protocols :as p])
  #+cljs
  (:require-macros [garden.selectors :refer :all])
  #+clj
  (:import clojure.lang.Keyword
           clojure.lang.Symbol
           clojure.lang.IFn))

(extend-protocol p/ISelector
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
(defn- invoke-specs
  "Helper for generating the clojure.lang.IFn spec for selector
  macros. Generated specs look like:

    (invoke [_ a ...]
      (str (p/selector x) (p/selector a) ...))"
  [x] 
  (let [map clojure.core/map
        syms (map (comp symbol str char) (range 0x61 0x75))
        argslists (map
                   (fn [i]
                     (vec (cons '_ (take i syms))))
                   (range 21)) ;; Only to fully satisfy IFn.  
        specs (map
               (fn [args]
                 (list 'invoke args
                       (concat `(str (p/selector ~x))
                               (map
                                (fn [a]
                                  `(p/selector ~a))
                                (rest args)))))
               argslists)]
    specs))

#+clj
(defn- ifn-impl [x]
  `(IFn
    ~@(invoke-specs x)))

#+clj
(defn- selector-impl [x]
  `(p/ISelector
    (~'selector [~'_]
      (p/selector ~x))))

;;----------------------------------------------------------------------
;; Macros

;; TODO: Provide more description in the docstring.
(defmacro deftypeselector
  "Define a reified instance named sym which satisfies
  clojure.lang.IFn and garden.protocols.ISelector.

  Example:

    (deftypeselector a)
    ;; => #'user/a

    (a \":hover\")
    ;; => \"a:hover\"

    (p/selector a)
    ;; => \"a\"
  "
  [sym]
  `(def ~sym
     (reify
       ~@(selector-impl `'~sym)
       ~@(ifn-impl `'~sym))))


;; TODO: Provide more description in the docstring.
(defmacro defpseudoclass
  "Define a reified instance named sym which satisfies
  clojure.lang.IFn and garden.protocols.ISelector.

  Example:

    (defpseudoclass hover)
    ;; => #'user/hover

    (hover \"[type=text]\")
    ;; => \":hover[type=text]\"

    (p/selector a)
    ;; => \":hover\"
  "
  [sym]
  (let [s (str \: (name sym))]
    `(def ~sym
       (reify
         ~@(selector-impl s)
         ~@(ifn-impl s)))))


(defmacro defstructuralpseudoclass
  "Define a function named sym for creating a CSS Structural
  pseudo-classes selector. When called the function returns a reified
  instance which satisfies clojure.lang.IFn and
  garden.protocols.ISelector. The return value of fn-tail is used to
  format the argument portion of the selector and must satisfy
  garden.protocols.ISelector.

  Example:

    (defstructuralpseudoclass not [x]
      (p/selector x))
    ;; => #'user/not

    (p/selector (not \"a\"))
    ;; => \":not(a)\"

    ((not \"a\") \"[src^=a]\")
    ;; => \":not(a)[src^=a]\"
  "
  [sym & fn-tail]
  (let [fmt (str \: (name sym) "(%s)")
        fn1 `(fn ~fn-tail)
        fn2 (let [args (gensym "args")]
              (list `fn `[& ~args]
                    (let [x `(format ~fmt (p/selector (apply ~fn1 ~args)))
                          s `(reify
                               ~@(selector-impl x)
                               ~@(ifn-impl x))]
                      s)))]
    `(def ~sym ~fn2)))

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
        Exception.
        #+cljs
        js/Error.
        (str "Invalid value " (pr-str s)))))))

(defstructuralpseudoclass nth-child [x] (nth-x x))
(defstructuralpseudoclass nth-last-child [x] (nth-x x))
(defstructuralpseudoclass nth-of-type [x] (nth-x x))
(defstructuralpseudoclass nth-last-of-type [x] (nth-x x))

;;----------------------------------------------------------------------
;; Special selectors

(deftypeselector
  ^{:doc "Parent selector."}
  &)
