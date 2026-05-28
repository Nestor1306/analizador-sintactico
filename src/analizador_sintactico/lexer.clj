(ns analizador-sintactico.lexer
    (:require [clojure.string :as str]))

;; ─────────────────────────────────────────────
;; Token types
;; ─────────────────────────────────────────────

(def token-types
  #{:keyword :identifier :number :operator :logical-op
    :assignment :punctuation :unknown})

;; ─────────────────────────────────────────────
;; Reserved words
;; ─────────────────────────────────────────────

(def reserved-words
  #{"var" "if" "else" "while" "function" "return"})

;; ─────────────────────────────────────────────
;; Token patterns  (order matters — most specific first)
;; ─────────────────────────────────────────────

(def token-patterns
  [;; Multi-char logical operators before single-char ones
   [:logical-op   #"^(?:==|!=|<=|>=)"]
   ;; Single-char logical operators
   [:logical-op   #"^[<>]"]
   ;; Assignment (single =, not ==)
   [:assignment   #"^="]
   ;; Algebraic operators
   [:operator     #"^[+\-*/]"]
   ;; Punctuation
   [:punctuation  #"^[(){};,]"]
   ;; Numbers — decimal before integer to avoid partial match
   [:number       #"^\d+\.\d+"]
   [:number       #"^\d+"]
   ;; Identifiers / keywords (start with letter, then letters+digits)
   [:identifier   #"^[a-zA-Z][a-zA-Z0-9]*"]])

;; ─────────────────────────────────────────────
;; Lex errors
;; ─────────────────────────────────────────────

(defn lex-error
      "Builds a lex-error map for an unrecognised character."
      [ch position line col]
      {:type  :lex-error
       :value (str ch)
       :msg   (str "Unexpected character '" ch "' at line " line ", col " col)
       :position position
       :line  line
       :col   col})

;; ─────────────────────────────────────────────
;; Single-token scanner
;; ─────────────────────────────────────────────

(defn- try-match
       "Tries every pattern against the remaining source string.
        Returns [token-type matched-string] or nil."
       [source]
       (some (fn [[ttype pattern]]
                 (when-let [m (re-find pattern source)]
                           [ttype m]))
             token-patterns))

(defn- classify-identifier
       "Upgrades :identifier to :keyword when the lexeme is a reserved word."
       [ttype lexeme]
       (if (and (= ttype :identifier) (reserved-words lexeme))
         :keyword
         ttype))

;; ─────────────────────────────────────────────
;; Tokenizer  →  public API
;; ─────────────────────────────────────────────

(defn tokenize
      "Scans `source` (a string) and returns a map:
         {:tokens   [{:type … :value … :line … :col …} …]
          :errors   [{:type :lex-error …} …]}

       Whitespace (including newlines) is skipped and used only for
       line/col tracking.  Unknown characters produce :lex-error entries
       but scanning continues (error recovery)."
      [source]
      (loop [remaining source
             position  0
             line      1
             col       1
             tokens    []
             errors    []]

            (if (empty? remaining)
              {:tokens tokens :errors errors}

              (let [ch (first remaining)]
                   (cond
                     ;; ── Newline ──────────────────────────────────────────────
                     (= ch \newline)
                     (recur (subs remaining 1) (inc position)
                            (inc line) 1 tokens errors)

                     ;; ── Other whitespace ─────────────────────────────────────
                     (Character/isWhitespace ch)
                     (recur (subs remaining 1) (inc position)
                            line (inc col) tokens errors)

                     ;; ── Known token ──────────────────────────────────────────
                     :else
                     (if-let [[raw-type lexeme] (try-match remaining)]
                             (let [ttype (classify-identifier raw-type lexeme)
                                   token {:type  ttype
                                          :value lexeme
                                          :line  line
                                          :col   col}]
                                  (recur (subs remaining (count lexeme))
                                         (+ position (count lexeme))
                                         line (+ col (count lexeme))
                                         (conj tokens token)
                                         errors))

                             ;; ── Unknown character — record error, skip 1 char ─────
                             (let [err (lex-error ch position line col)]
                                  (recur (subs remaining 1) (inc position)
                                         line (inc col) tokens
                                         (conj errors err)))))))))

;; ─────────────────────────────────────────────
;; Convenience helpers (used by the parser + HTML gen)
;; ─────────────────────────────────────────────

(defn lex-ok?
      "Returns true when tokenization produced zero errors."
      [lex-result]
      (empty? (:errors lex-result)))

(defn token-list
      "Extracts just the token vector from a tokenize result."
      [lex-result]
      (:tokens lex-result))

;; ─────────────────────────────────────────────
;; HTML helpers (used by the generator)
;; ─────────────────────────────────────────────

(defn escape-html
      "Escapes the five XML/HTML special characters."
      [text]
      (-> text
          (str/replace "&"  "&amp;")
          (str/replace "<"  "&lt;")
          (str/replace ">"  "&gt;")
          (str/replace "\"" "&quot;")
          (str/replace "'"  "&#39;")))

(defn token-css-class
      "Maps a token type keyword to a CSS class name string."
      [token-type]
      (case token-type
            :keyword      "keyword"
            :identifier   "identifier"
            :number       "number"
            :operator     "operator"
            :logical-op   "logical-op"
            :assignment   "assignment"
            :punctuation  "punctuation"
            :lex-error    "error"
            "plain"))
