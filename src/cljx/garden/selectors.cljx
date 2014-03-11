(ns garden.selectors
  (:refer-clojure :exclude [map meta var])
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

(defmacro defpseudoclass [sym]
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

    (p/selector (not \":hover\"))
    ;; => \":not(:hover)\"

    ((not \":hover\") \"[src^=a]\")
    ;; => \":not(:hover)[src^=a]\"
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
     ~@(map gen-type-selector-def html-tags)))

(gen-type-selector-defs)
