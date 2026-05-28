(ns analizador-sintactico.core
    (:require [analizador-sintactico.lexer :as lexer]
      [analizador-sintactico.html-generator :as html-gen]
      [clojure.java.io :as io])
    (:gen-class))

(defn process-file [input-path output-path]
      (let [source     (slurp input-path)
            filename   (.getName (io/file input-path))
            resultado  (lexer/tokenize source)
            tokens     (lexer/token-list resultado)
            errors     (:errors resultado)
            html       (html-gen/generate tokens errors filename)]
           (spit output-path html)
           (println (str "✓ Generado: " output-path))
           resultado))

(defn -main [& args]
      (when (empty? args)
            (println "Uso: lein run <archivo-entrada> [archivo-salida]")
            (System/exit 1))
      (let [input  (first args)
            output (or (second args) (str input ".html"))]
           (process-file input output)))