(ns analizador-sintactico.core
    (:require [analizador-sintactico.lexer :as lexer]
              [analizador-sintactico.parser :as p]
      [analizador-sintactico.html-generator :as html-gen]
      [clojure.java.io :as io])
    (:gen-class))

(defn process-file [input-path output-path]
  "Funcion auciliar del main que es llamada si es que
  recibe como minimo el nombre y direccion del archivo
  de texto"

  (let [source       (slurp input-path)
        filename     (.getName (io/file input-path))
        lex-result   (lexer/tokenize source)
        tokens       (lexer/token-list lex-result)
        lex-errors   (:errors lex-result)
        parse-result (p/parse tokens) ;El parser obtiene la lista de tokens obtenida del Lexer
        parse-errors (:errors parse-result)
        all-errors   (concat lex-errors parse-errors)
        html         (html-gen/generate tokens all-errors filename)]
    (spit output-path html)
    (println (str "✓ Generado: " output-path))
    {:lex lex-result :parse parse-result}))

(defn -main [& args]
      (when (empty? args)
            (println "Faltan parametros")
            (System/exit 1))
      (let [input  (first args)
            output (or (second args) (str input ".html"))]
           (process-file input output)))