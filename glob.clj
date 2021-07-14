(ns glob (:require [me.raynes.fs :as fs]))

(run! (comp println str)
      (fs/glob (first *command-line-args*)))
