(ns analizador-sintactico.parser
  (:require [analizador-sintactico.lexer :as lexer]))

;; Estado del parser
(def ^:dynamic *tokens* (atom []))
(def ^:dynamic *pos*    (atom 0))

;; ─────────────────────────────────────────────
;; Helpers
;; ─────────────────────────────────────────────

(defn- peek-token
  "Devuelve el token actual sin consumirlo. nil si no hay más."
  []
  (nth @*tokens* @*pos* nil))


(defn- advance
  "Devuelve el token actual y avanza el cursor en 1."
  []
  (let [last_tok (peek-token)]
    (swap! *pos* inc)
    last_tok))


(defn- at-end?
  "True si ya se acabaron los tokens."
  []
  (>= @*pos* (count @*tokens*)))

(defn- check
  "True si el token actual coincide con el tipo (y opcionalmente con un valor)."
  ([tipo]
   (when-let [tok (peek-token)]
     (= (:type tok) tipo)))
  ([tipo valor]
   (when-let [tok (peek-token)]
     (and (= (:type tok) tipo)
          (= (:value tok) valor)))))

(defn- match
  ([tipo] (when (check tipo) (advance) true))
  ([tipo valor] (when (check tipo valor) (advance) true)))


(defn- syntax-error! [msg]
  (let [tok (peek-token)]
    (throw (ex-info msg
                    {:type  :syntax-error
                     :msg   msg
                     :line  (or (:line tok) :eof)
                     :col   (or (:col tok)  :eof)
                     :found tok}))))

(defn- expect
  ([tipo msg]
   (if (check tipo)
     (advance)
     (syntax-error! (str "Se esperaba " msg))))
  ([tipo valor msg]
   (if (check tipo valor)
     (advance)
     (syntax-error! (str "Se esperaba " msg)))))

;; ─────────────────────────────────────────────
;; Reglas — todas se anuncian arriba por mutua recursión
;; ─────────────────────────────────────────────

(declare parse-program
         parse-statement-list
         parse-statement
         parse-var-decl
         parse-assignment
         parse-if
         parse-while
         parse-function
         parse-return
         parse-expression
         parse-comparison
         parse-addition
         parse-multiplication
         parse-primary)

;; ─────────────────────────────────────────────
;; Stubs temporales (los reemplazas uno por uno)
;; ─────────────────────────────────────────────

(defn- parse-var-decl       [] (throw (Exception. "TODO parse-var-decl")))
(defn- parse-assignment     [] (throw (Exception. "TODO parse-assignment")))
(defn- parse-if             [] (throw (Exception. "TODO parse-if")))
(defn- parse-while          [] (throw (Exception. "TODO parse-while")))
(defn- parse-function       [] (throw (Exception. "TODO parse-function")))
(defn- parse-return         [] (throw (Exception. "TODO parse-return")))
(defn- parse-expression     [] (throw (Exception. "TODO parse-expression")))
(defn- parse-comparison     [] (throw (Exception. "TODO parse-comparison")))
(defn- parse-addition       [] (throw (Exception. "TODO parse-addition")))
(defn- parse-multiplication [] (throw (Exception. "TODO parse-multiplication")))
(defn- parse-primary        [] (throw (Exception. "TODO parse-primary")))

;; ─────────────────────────────────────────────
;; S → L
;; (parsea la lista de estatutos y verifica que no sobren tokens)
;; ─────────────────────────────────────────────

(defn- parse-program
  []
  (parse-statement-list)
  (when-not (at-end?)
    (syntax-error! "tokens sobrantes")))

;; ─────────────────────────────────────────────
;; Helper FIRST(T): tokens que pueden empezar un estatuto.
;; ─────────────────────────────────────────────

(defn- statement-start?
  "True si el token actual es uno de: var, if, while, function, identifier."
  []
  (or (check :keyword "var")
      (check :keyword "if")
      (check :keyword "while")
      (check :keyword "function")
      (check :identifier)))


;; ─────────────────────────────────────────────
;; L → T L | T   (uno o más estatutos)
;; (parsea uno y luego repite mientras siga habiendo)
;; ─────────────────────────────────────────────

(defn- parse-statement-list
  []
  (parse-statement)
  (while (statement-start?)
    (parse-statement)))

;; ─────────────────────────────────────────────
;; T → V; | A; | I | W | F
;; (dispatcher: mira el primer token y elige qué regla aplicar)
;;
;; OJO:
;;   - V; y A;  →  pides el ';' aquí, después de parsear V o A.
;;   - I, W, F  →  no llevan ';', son bloques con {}.
;;   - Solo usa `check`, NO `match` (no consumas el token, eso lo hace
;;     la función que llamas adentro).
;; ─────────────────────────────────────────────

(defn- parse-statement
  []
  (cond
    (check :keyword "var")
    (do (parse-var-decl)
        (expect :punctuation ";" "Se esperaba una ;"))

    (check :keyword "if")
    (parse-if)

    (check :keyword "while")
    (parse-while)

    (check :keyword "function")
    (parse-function)

    (check :identifier)
    (do (parse-assignment)
        (expect :punctuation ";" "Se esperaba un ;"))

    :else
    (syntax-error! "se esperaba un estatuto")))


;; ─────────────────────────────────────────────
;; API pública
;; ─────────────────────────────────────────────

(defn parse
  "Recibe el vector de tokens del lexer y devuelve
     {:ok? true  :errors []}                       si todo va bien
     {:ok? false :errors [{...info del error...}]} si hay error sintáctico."
  [tokens]
  (reset! *tokens* (vec tokens))
  (reset! *pos*    0)
  (try
    (parse-program)
    {:ok? true :errors []}
    (catch clojure.lang.ExceptionInfo e
      {:ok? false :errors [(ex-data e)]})))