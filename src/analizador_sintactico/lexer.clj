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
   [:number       #"^\d+\.\d+"] ;ORDEN IMPORTA
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
       (some (fn [[ttype pattern]] ;Solo descomprime el par sin la sintaxis larga
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
      "Scanea el input source, que es un string, y regresa un mapa de la manera:
        {:tokens   [{:type … :value … :line … :col …} …]
        :errors   [{:type :lex-error …} …]}"
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
                     ; Salto de linea
                     (= ch \newline)
                     (recur (subs remaining 1) (inc position) ;Quita el primer elemento del string que en este momento es /n
                            (inc line) 1 tokens errors)
                     ;Regresar a la primera columna y saltar de linea porque encontro un salto de linea

                     ; Espacio en blanco
                     (Character/isWhitespace ch)
                     (recur (subs remaining 1) (inc position)
                            line (inc col) tokens errors)

                     ; Token conocido
                     :else
                     (if-let [[raw-type lexeme] (try-match remaining)]
                             (let [type (classify-identifier raw-type lexeme)
                                   token {:type  type
                                          :value lexeme
                                          :line  line
                                          :col   col}]
                                  (recur (subs remaining (count lexeme)) ;Solo se le resta la cantidad de caracteres que tiene el resultado
                                         (+ position (count lexeme))
                                         line (+ col (count lexeme))
                                         (conj tokens token) ; Lo agrega a la lista de tokens
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
