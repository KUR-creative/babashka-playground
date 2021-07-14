#!/usr/bin/env bb
(ns me.raynes.fs
  "File system utilities in Clojure"
  (:refer-clojure :exclude [name parents])
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import [java.io File FilenameFilter]
           [java.nio.file Files Path LinkOption CopyOption]
           [java.nio.file.attribute FileAttribute]))

;; Once you've started a JVM, that JVM's working directory is set in stone
;; and cannot be changed. This library will provide a way to simulate a
;; working directory change. `cwd` is considered to be the current working
;; directory for functions in this library. Unfortunately, this will only
;; apply to functions inside this library since we can't change the JVM's
;; actual working directory.
(def ^{:doc "Current working directory. This cannot be changed in the JVM.
             Changing this will only change the working directory for functions
             in this library."
       :dynamic true}
  *cwd* (.getCanonicalFile (io/file ".")))

(let [homedir (io/file (System/getProperty "user.home"))
      usersdir (.getParent homedir)])

;; Library functions will call this function on paths/files so that
;; we get the cwd effect on them.
(defn ^File file
  "If `path` is a period, replaces it with cwd and creates a new File object
   out of it and `paths`. Or, if the resulting File object does not constitute
   an absolute path, makes it absolutely by creating a new File object out of
   the `paths` and cwd."
  [path & paths]
  (when-let [path (apply
                   io/file (if (= path ".")
                             *cwd*
                             path)
                   paths)]
    (if (.isAbsolute ^File path)
      path
      (io/file *cwd* path))))

(extend-protocol io/Coercions
  Path
  (as-file [this] (.toFile this))
  (as-url [this] (.. this (toFile) (toURL))))

;; Rewrite directory? and delete-dir to include LinkOptions.
(def ^{:doc "The root of a unix system is `/`, `nil` on Windows"}
  unix-root (when (= File/separator "/") File/separator))

(defn split
  "Split `path` to components."
  [path]
  (let [pathstr (str path)
        jregx (str "\\Q" File/separator "\\E")]
    (cond (= pathstr unix-root) (list unix-root)
          (and unix-root (.startsWith pathstr unix-root))
          ;; unix absolute path
            (cons unix-root (seq (.split (subs pathstr 1) jregx)))
          :else (seq (.split pathstr jregx)))))

; Taken from https://github.com/jkk/clj-glob. (thanks Justin!)
(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
        (cond
         (nil? c) (re-pattern
                    ; We add ^ and $ since we check only for file names
                    (str "^" (if (= \. (first s)) "" "(?=[^\\.])") re "$"))
         (= c \\) (recur (nnext stream) (str re c c) curly-depth)
         (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                         curly-depth)
         (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
         (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
         (= c \{) (recur (next stream) (str re \() (inc curly-depth))
         (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                 curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                  curly-depth)
         :else (recur (next stream) (str re c) curly-depth)))))

(defn glob
  "Returns files matching glob pattern."
  ([pattern]
     (let [parts (split pattern)
           root (apply file (if (= (count parts) 1) ["."] (butlast parts)))]
       (glob root (last parts))))
  ([^File root pattern]
     (let [regex (glob->regex pattern)]
       (seq (.listFiles
             root
             (reify FilenameFilter
               (accept [_ _ filename]
                 (boolean (re-find regex filename)))))))))

(ns glob (:require [me.raynes.fs :as fs]))

(run! (comp println str)
      (fs/glob (first *command-line-args*)))
