(ns analizador-sintactico.lexer
    (:require [clojure.string :as str]))

;; ─────────────────────────────────────────────
;; Tipos de Tokens
;; ─────────────────────────────────────────────

(def token-types
  #{:keyword :identifier :number :operator :logical-op
    :assignment :punctuation :unknown})

;; ─────────────────────────────────────────────
;; Palabras reservadas
;; ─────────────────────────────────────────────

(def reserved-words
  #{"var" "if" "else" "while" "function" "return"})

;; ─────────────────────────────────────────────
;; Patrones de tokens (Regex)
;; ─────────────────────────────────────────────

(def token-patterns
  [;; Caracteres multiples antes de individuales
   [:logical-op   #"^(?:==|!=|<=|>=)"]
   ;; Operadores logicos de un caracter
   [:logical-op   #"^[<>]"]
   ;; Asignacion (un solo caracter sin '==')
   [:assignment   #"^="]
   ;; Operadores algebraicos
   [:operator     #"^[+\-*/]"]
   ;; Puntuacion
   [:punctuation  #"^[(){};,]"]
   ;; Numberos (decimal antes de entero)
   [:number       #"^\d+\.\d+"] ;ORDEN IMPORTA
   [:number       #"^\d+"]
   ;; Identificadores
   [:identifier   #"^[a-zA-Z][a-zA-Z0-9]*"]])

;; ─────────────────────────────────────────────
;; Errores lexicos
;; ─────────────────────────────────────────────

(defn lex-error
      "Construye un mapa cuando reconoce un caracter no definido"
      [ch position line col]
      {:type  :lex-error
       :value (str ch)
       :msg   (str "Unexpected character '" ch "' at line " line ", col " col)
       :position position
       :line  line
       :col   col})

;; ─────────────────────────────────────────────
;; Scanner de un solo token
;; ─────────────────────────────────────────────

(defn- try-match
       "Intenta todo patron con el strin restante hasta que encuentra algun.
        Regresa nil si no encuentra nada, una tupla si si lo hace"

       [source]
       (some (fn [[ttype pattern]] ;Solo descomprime el par sin la sintaxis larga
                 (when-let [m (re-find pattern source)]
                           [ttype m]))
             token-patterns))

(defn- classify-identifier
       "Cambia el tipo del token si se trata de una palabra reservada"
       [ttype lexeme]
       (if (and (= ttype :identifier) (reserved-words lexeme))
         :keyword
         ttype))

;; ─────────────────────────────────────────────
;; Tokenizer
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

                             ;; Caracter desconocido
                             (let [err (lex-error ch position line col)]
                                  (recur (subs remaining 1) (inc position)
                                         line (inc col) tokens
                                         (conj errors err)))))))))


(defn token-list
      "Extracts just the token vector from a tokenize result."
      [lex-result]
      (:tokens lex-result))
