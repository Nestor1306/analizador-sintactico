# analizador-sintactico

Un analizador sintactico hecho en Clojure

# Integrantes

-Emiliano Garcia
-Nestor Huidobro
-Alexis Alva

# Como correrlo

Estando en la carpeta del proyecto, ejecuta:

#!/bin/bash
lein run resources/sample/<nombre_del_texto.txt>

Esto creara un archivo HTML que despliega el texto resaltado por tokens y una alerta de eror en caso de presentar algun error.

Corre el siguiente comando para abrir el archivo HTML inmediatamente:
#!/bin/bash
open <archivo_HTML.html>
