(ns analizador-sintactico.parser)

;; Estado del parser
;; Se decidio usar atoms por su ventaja con threads en Clojure
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
  "Recibe un tipo a esperar, opcionalmente un valor (para palabras reservadas)
  y si coincide avanza, tira error a lo contrario"

  ([tipo msg]
   (if (check tipo)
     (advance)
     (syntax-error! (str "Se esperaba " msg))))
  ([tipo valor msg]
   (if (check tipo valor)
     (advance)
     (syntax-error! (str "Se esperaba " msg)))))

;; ─────────────────────────────────────────────
;; Reglas
;; ─────────────────────────────────────────────

;; Esto hace que sea visible para todas las funciones
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
         statement-start?
         parse-primary)

(defn- parse-var-decl
  "V → v d = E"
  []
  (expect :keyword    "var" "palabra reservada 'var'")
  (expect :identifier        "nombre de la variable después de 'var'")
  (expect :assignment        "signo '=' en la declaración")
  (parse-expression))

(defn- parse-expression
  "E → B"
  []
  (parse-comparison))

(defn- parse-assignment []
  (expect :identifier "un identificador")
  (expect :assignment "un simbolo '=' en la asignacion")
  (parse-expression))

(defn- parse-if
  "I → i ( E ) { L } Q
   Q → e { L } | λ"
  []
  (expect :keyword     "if" "palabra reservada 'if'")
  (expect :punctuation "("  "'(' después de 'if'")
  (parse-expression)
  (expect :punctuation ")"  "')' al cerrar la condición")
  (expect :punctuation "{"  "'{' al iniciar el bloque if")
  (parse-statement-list)
  (expect :punctuation "}"  "'}' al cerrar el bloque if")

  ;; else opcional
  (when (match :keyword "else")
    (expect :punctuation "{" "'{' al iniciar el bloque else")
    (parse-statement-list)
    (expect :punctuation "}" "'}' al cerrar el bloque else")))

(defn- parse-while []
  (expect :keyword "while")
  (expect :punctuation "(" "( para abrir el condicional")
  (parse-expression)
  (expect :punctuation ")" ") para cerrar el condicional")
  (expect :punctuation "{")
  (parse-statement-list)
  (expect :punctuation "}"))



(defn- parse-return
  "Parsea el return SIN consumir el ; final.
   El ';' lo pone parse-function-statement."
  []
  (expect :keyword "return" "palabra reservada 'return'")
  (when-not (check :punctuation ";")
    (parse-expression)))



(defn- function-statement-start?
  "Igual que statement-start? pero también acepta 'return'."
  []
  (or (statement-start?)
      (check :keyword "return")))


(defn- parse-function-statement
  "Como parse-statement, pero agrega 'return'."
  []
  (cond
    (check :keyword "return")
    (do (parse-return)
        (expect :punctuation ";" "';' al final del return"))

    :else
    (parse-statement)))

(defn- parse-function-statement-list
  "G → C G | C : 1 o más C."
  []
  (parse-function-statement)
  (while (function-statement-start?)
    (parse-function-statement)))


(defn- parse-function
  []
  (expect :keyword     "function" "palabra reservada 'function'")
  (expect :identifier              "nombre de la función")
  (expect :punctuation "("         "'(' al iniciar parámetros")
  ;; P → R | λ : la lista de params es opcional
  (when (match :identifier)
    ;; ya consumimos el primer identifier; ahora 0+ comas + ident
    (while (match :punctuation ",")
      (expect :identifier "nombre de parámetro después de ','")))
  (expect :punctuation ")" "')' al cerrar parámetros")
  (expect :punctuation "{" "'{' al iniciar cuerpo de la función")
  (parse-function-statement-list)
  (expect :punctuation "}" "'}' al cerrar cuerpo de la función"))


(defn- parse-comparison
  "B → M Y
   Y → < M Y | > M Y | == M Y | != M Y | λ"
  []
  (parse-addition)
  (while (or (match :logical-op "<")
             (match :logical-op ">")
             (match :logical-op "==")
             (match :logical-op "!="))
    (parse-addition)))

(defn- parse-addition
  "M → N U
   U → + N U | - N U | λ"
  []
  (parse-multiplication)
  (while (or (match :operator "+")
             (match :operator "-"))
    (parse-multiplication)))

(defn- parse-multiplication
  "N → H D
   D → * H D | / H D | λ"
  []
  (parse-primary)
  (while (or (match :operator "*")
             (match :operator "/"))
    (parse-primary)))

(defn- parse-primary
  "H → ( E ) | n | d K
   K → ( J ) | λ
   J → E A2 | λ
   A2 → , E A2 | λ"
  []
  (cond
    (match :punctuation "(")
    (do (parse-expression)
        (expect :punctuation ")" "')' al cerrar la expresión"))

    (match :number)
    nil

    (match :identifier)
    (when (match :punctuation "(")
      (when-not (check :punctuation ")")
        (parse-expression)
        (while (match :punctuation ",")
          (parse-expression)))
      (expect :punctuation ")" "')' al cerrar la llamada"))

    :else
    (syntax-error! "expresión inválida (se esperaba número, identificador o '(' )")))


(defn- parse-program
  []
  (parse-statement-list)
  (when-not (at-end?)
    (syntax-error! "tokens sobrantes")))

(defn- statement-start?
  "True si el token actual es uno de: var, if, while, function, identifier."
  []
  (or (check :keyword "var")
      (check :keyword "if")
      (check :keyword "while")
      (check :keyword "function")
      (check :identifier)))


(defn- parse-statement-list
  []
  (parse-statement)
  (while (statement-start?)
    (parse-statement)))


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
;; Publico
;; ─────────────────────────────────────────────

(defn parse
  "Recibe el vector de tokens del lexer y devuelve
     {:ok? true  :errors []}                       si todo fue bien
     {:ok? false :errors [{...info del error...}]} si hay error sintáctico."
  [tokens]
  (reset! *tokens* (vec tokens))
  (reset! *pos*    0)
  (try
    (parse-program)
    {:ok? true :errors []}
    (catch clojure.lang.ExceptionInfo e
      {:ok? false :errors [(ex-data e)]})))