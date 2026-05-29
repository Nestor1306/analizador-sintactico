(ns analizador-sintactico.html-generator
    (:require [clojure.string :as str]))

"Este archivo se encarga principalmente de generar un archivo html
para visualizar los errores obtenidos, ninguno en el caso perfecto,
de un archivo como output"

(defn escape-html [text]
  "Remplaza caracteres especiales por su manera
  correcta de incluirlos en html"

      (-> text
          (str/replace "&" "&amp;")
          (str/replace "<" "&lt;")
          (str/replace ">" "&gt;")
          (str/replace "\"" "&quot;")))

(defn token-class [token-type]
      (case token-type
            :keyword     "keyword"
            :identifier  "identifier"
            :number      "number"
            :operator    "operator"
            :assignment  "assignment"
            :punctuation "punctuation"
            :logical-op  "logical-op"
            :lex-error   "error"
            "plain"))

(defn token->span [token]
      (str "<span class=\"" (token-class (:type token)) "\">"
           (escape-html (:value token))
           "</span>"))

(defn generate-css []
      "<style>
        body { background: #1e1e1e; color: #d4d4d4; font-family: monospace; padding: 2rem; }
        pre  { line-height: 1.6; font-size: 14px; }
        .keyword     { color: #569cd6; font-weight: bold; }
        .identifier  { color: #9cdcfe; }
        .number      { color: #b5cea8; }
        .operator    { color: #d4d4d4; }
        .logical-op  { color: #d4d4d4; }
        .assignment  { color: #d4d4d4; }
        .punctuation { color: #808080; }
        .error       { color: #f44747; text-decoration: line-through; }
        .error-banner {
          background: #3a0000; color: #f44747;
          border: 1px solid #f44747;
          padding: 0.8rem 1rem; border-radius: 4px;
          margin-bottom: 1rem; font-family: monospace;
        }
      </style>")

(defn tokens->html [tokens]
  "Por cada token obtenido de la lista despues del parser,
  se procesa y se añade a un string final que es el desplegado
  en la pagina de HTML"

      (loop [remaining tokens
             result    []
             line      1]
            (if (empty? remaining)
              (str/join "" result)
              (let [token    (first remaining)
                    tok-line (:line token)]
                   (if (> tok-line line)
                     (recur remaining
                            (conj result (str/join "" (repeat (- tok-line line) "\n")))
                            tok-line)
                     (recur (rest remaining)
                            (conj result (token->span token) " ")
                            line))))))



(defn generate [tokens errors filename]
  "Funcion que genera el archivo HTML completo y lo exporta
  con su extension en el root del txt. Este metodo se llama desde process file en main."
  (let [ok?       (empty? errors)
        err       (first errors) ;Solo el primer error pero puede estar bien un bucle para todos. Aunque... se suele mostrar el primero?
        error-msg (when-not ok?
                    (str "<div class=\"error-banner\">"
                         "&#9888; Error en línea "
                         (:line err) ", columna " (:col err) ": "
                         (escape-html (:msg err))
                         "</div>"))
        code-html (tokens->html tokens)]
    (str "<!DOCTYPE html>\n"
         "<html lang=\"es\">\n"
         "<head>\n"
         "  <meta charset=\"UTF-8\">\n"
         "  <title>" (escape-html filename) "</title>\n"
         (generate-css)
         "\n</head>\n"
         "<body>\n"
         "  <h2 style=\"color:#569cd6;\">" (escape-html filename) "</h2>\n"
         (or error-msg "")
         "  <pre>" code-html "</pre>\n"
         "</body>\n"
         "</html>")))