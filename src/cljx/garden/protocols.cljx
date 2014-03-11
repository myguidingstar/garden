(ns garden.protocols)

(defprotocol ISelector
  (selector [this] "Render this as a CSS selector"))
