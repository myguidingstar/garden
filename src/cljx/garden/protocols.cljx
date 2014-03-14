(ns garden.protocols)

(defprotocol ICssSelector
  (selector [this] "Render this as a CSS selector"))
