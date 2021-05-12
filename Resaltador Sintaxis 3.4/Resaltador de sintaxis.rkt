#lang racket
;Leonardo Alvarado Menendez A01705998
;RESALTADOR DE SINTAXIS  DE C++ Y PYTHON
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Libreria que se utiliza para leer txt
(require 2htdp/batch-io)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con letras de "a" a "z"
; letras -> lst (string)
(define letras
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con los digitos
;numeros: -> lst (string)
(define numeros
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con operadores de cpp y python
; operadores: -> lst (string)
(define operadores
  '("[" "]" "{" "}" "(" ")" ";" "," ":" "&" "|" "=" "<" ">" "%" "*" "/" "+" "-")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con palabras reservadas de cpp
; reservados_cpp: -> lst (string)
(define reservados_cpp
 '("using" "namespace" "while" "do" "for" "if" "else" "switch" "case" "cout" "cin" "return" "true" "false" "endl")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con los tipos de datos
; tipos_datos: -> lst (string)
(define tipos_datos
  '("int" "long" "float" "double" "bool" "char" "string" "void")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con las librerias de cpp
; libreria_cpp: -> lst (string)
(define libreria_cpp
  '("#include")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Lista con palabras reservadas de python
; reservados_python: -> lst (string)
(define reservados_python
 '("def" "while" "if" "return" "for" "input" "true" "false" "else" "elif" "print" "in" "len")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;ista con las librerias de python
; librerias_python: -> lst (string)
(define librerias_python
  '("import" "from")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Direccion del archivo de prueba de cpp
;Nota:En esta secion se debe escribir la direccion en donde se encuentren los archivos que se quieran correr,
;esta direccion aplica para mi ejemplo, pero si se quiere correr, se devera colocar la direccion de donde se encuentra el archivo
; leer_archivo_c++: -> string
(define leer_archivo_c++
  "C:\\Users\\leo20\\Downloads\\input_cpp.txt"
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Direccion del archivo de prueba de python
;Nota:En esta secion se debe escribir la direccion en donde se encuentren los archivos que se quieran correr,
;esta direccion aplica para mi ejemplo, pero si se quiere correr, se devera colocar la direccion de donde se encuentra el archivo
; leer_archivo_python: -> string
(define leer_archivo_python
  "C:\\Users\\leo20\\Downloads\\input_python.txt"
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Direccion donde se guarda el archivo html
;Nota:En esta secion se debe escribir la direccion en donde sequiere guardar el html,
;esta direccion aplica para mi ejemplo, pero si se quiere guardar, se devera colocar la direccion de la pc
; escribir_archivo: -> string
(define escribir_archivo
  "C:\\Users\\leo20\\Downloads\\output.html"
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Se define una lista con el principio basico de un html
; txt: -> string
(define txt
  '("\n" "<body>" "\n"
"</head>" "\n"
"<title>RESALTADOR DE SINTAXIS</title>" "\n"
"<link rel='stylesheet' href='styles.css'>" "\n"
"<head>" "\n"
"<html>" "\n"
"<!DOCTYPE html>")
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que compara los caracteres de la lista enviada con los caracteres de las listas previamente definidas
(define buscar_en_lista
  (lambda (string list)
    (cond
     [(null? list) #f]
     [(string-ci=? string (car list)) #t]
     [else (buscar_en_lista string (cdr list))]
    )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega al texto final los caracteres necesarios para imprimir un html correcto al encintrar una libreria de cpp 
(define continuidad_libreria_cpp
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\n") (leer_c++ list (cons "</span>" txt))]
      [(string-ci=? (car list) "<") (continuidad_libreria_cpp (cdr list) (cons "&lt" txt))]
      [(string-ci=? (car list) ">") (continuidad_libreria_cpp (cdr list) (cons "&gt" txt))]
      [else (continuidad_libreria_cpp (cdr list) (cons (car list) txt))]
     )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que busca strings, los cuales sean palabras reservadas, librerias o tipos de dato en cpp
(define palabra_cpp
  (lambda (list txt palabra)
    (cond
      [(null? list) ((write escribir_archivo) (cons palabra txt))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra reservados_cpp)) (leer_c++ list (cons "</span>" (cons palabra (cons "<span class='reserved'>" txt))))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra tipos_datos)) (leer_c++ list (cons "</span>" (cons palabra (cons "<span class='data_type'>" txt))))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra libreria_cpp)) (continuidad_libreria_cpp list (cons palabra (cons "<span class='pragma'>" txt)))]
      [(string-ci=? (car list) " ") (leer_c++ list (cons palabra txt))]
      [(string-ci=? (car list) "\n") (leer_c++ list (cons palabra txt))]
      [(buscar_en_lista (car list) numeros) (leer_c++ list (cons palabra txt))]
      [(buscar_en_lista (car list) operadores) (leer_c++ list (cons palabra txt))]
      [else (palabra_cpp (cdr list) txt (~a palabra (car list)))]
   )
 )
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega a una lista todos los caracteres entre comillas
(define comentario_cpp
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\"") (leer_c++ (cdr list) (cons "</span>"(cons (car list) txt)))]
      [else (comentario_cpp (cdr list) (cons (car list) txt))]
     )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega a una lista todos los caracteres que se encuentran despues de "//" y termina al encontrar un salto de linea
(define comentario_slash_cpp
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\n") (leer_c++ (cdr list) (cons "</span>" (cons "<p>" (cons "\n" (cons "</p>" (cons (car list) txt))))))]
      [else (comentario_slash_cpp (cdr list) (cons (car list) txt))]
     )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega a una lista todos los caracteres que se encuentran entre "/**/"
(define comentario_multiple_cpp
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\n") (comentario_multiple_cpp (cdr list) (cons "<span class='comment'>"(cons "</span>" (cons "<p>" (cons "\n" (cons "</p>" txt))))))]
      [(and (string-ci=? (car list) "*") (string-ci=? (cadr list) "/") (leer_c++ (cddr list) (cons "</span>"(cons "*/" txt))))]
      [else (comentario_multiple_cpp (cdr list) (cons (car list) txt))]
     )
  )
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que que identifica que tipo de caracter resivimos para asi agregarlo de manera correcta al html 
(define leer_c++
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) (cons "\n"(cons "</html>" (cons "\n" (cons "</body>" (cons "\n" txt))))))]
      [(string-ci=? (car list) " ") (leer_c++ (cdr list) (cons "&nbsp;" txt))]
      [(string-ci=? (car list) "\n") (leer_c++ (cdr list) (cons "<p>" (cons "\n" (cons "</p>" txt))))]
      [(and (string-ci=? (car list) "/") (string-ci=? (cadr list) "/") (comentario_slash_cpp (cddr list) (cons "//" (cons "<span class='comment'>" txt))))]
      [(and (string-ci=? (car list) "/") (string-ci=? (cadr list) "*") (comentario_multiple_cpp (cddr list) (cons "/*" (cons "<span class='comment'>" txt))))]
      [(string-ci=? (car list) "\"") (comentario_cpp (cdr list) (cons (car list) (cons "<span class='text'>" txt)))]
      [(buscar_en_lista (car list) numeros) (leer_c++ (cdr list) (cons "</span>" (cons (car list) (cons "<span class='number'>" txt))))]
      [(buscar_en_lista (car list) operadores) (leer_c++ (cdr list) (cons "</span>" (cons (car list) (cons "<span class='reserved'>" txt))))]
      [else (palabra_cpp (cdr list) txt (~a "" (car list)))]
     )
   )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega a una lista los caracteres que son parte de un comentario de multiples lineas
(define triple_comentario_python
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\n") (triple_comentario_python (cdr list) (cons "<span class='comment'>"(cons "</span>" (cons "<p>" (cons "\n" (cons "</p>" txt))))))]
      [(and (string-ci=? (caddr list) "\"") (and (string-ci=? (cadr list) "\"") (string-ci=? (car list) "\""))) (leer_python (cdddr list) (cons "</span>" (cons (caddr list)(cons (cadr list) (cons (car list) txt)))))]
      [else (triple_comentario_python (cdr list) (cons (car list) txt))]
     )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que busca strings, los cuales sean palabras reservadas o librerias en python
(define palabra_python
  (lambda (list txt palabra)
    (cond
      [(null? list) ((write escribir_archivo) (cons palabra txt))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra tipos_datos)) (leer_python list (cons "</span>" (cons palabra (cons "<span class='data_type'>" txt))))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra librerias_python)) (leer_python list (cons "</span>" (cons palabra (cons "<span class='pragma'>" txt))))]
      [(and (not (buscar_en_lista (car list) letras))(buscar_en_lista palabra reservados_python)) (leer_python list (cons "</span>" (cons palabra (cons "<span class='reserved'>" txt))))]
      [(string-ci=? palabra "\"\"\"") (triple_comentario_python list (cons "\"\"\"" (cons "<span class='comment'>" txt)))]
      [(string-ci=? (car list) " ") (leer_python list (cons palabra txt))]
      [(string-ci=? (car list) "\n") (leer_python list (cons palabra txt))]
      [(buscar_en_lista (car list) numeros) (leer_python list (cons palabra txt))]
      [(buscar_en_lista (car list) operadores) (leer_python list (cons palabra txt))]
      [else (palabra_python (cdr list) txt (~a palabra (car list)))]
   )
 )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que agrega a una lista todos los caracteres despues de un # hasta el salto de linea
(define comentario_python
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) txt)]
      [(string-ci=? (car list) "\n") (leer_python list (cons "</span>" txt))]
      [else (comentario_python (cdr list) (cons (car list) txt))]
     )
  )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que que identifica que tipo de caracter resivimos para asi agregarlo de manera correcta al html 
(define leer_python
  (lambda (list txt)
    (cond
      [(null? list) ((write escribir_archivo) (cons "\n"(cons "</html>" (cons "\n" (cons "</body>" (cons "\n" txt))))))]
      [(string-ci=? (car list) "#") (comentario_python (cdr list) (cons (car list) (cons "<span class='comment'>" txt)))]
      [(string-ci=? (car list) " ") (leer_python (cdr list) (cons "&nbsp;" txt))]
      [(string-ci=? (car list) "\n") (leer_python (cdr list) (cons "<p>" (cons "\n" (cons "</p>" txt))))]
      [(buscar_en_lista (car list) numeros) (leer_python (cdr list) (cons "</span>" (cons (car list) (cons "<span class='number'>" txt))))]
      [(buscar_en_lista (car list) operadores) (leer_python (cdr list) (cons "</span>" (cons (car list) (cons "<span class='reserved'>" txt))))]
      [else (palabra_python (cdr list) txt (~a "" (car list)))]
     )
   )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion para identificar si recivimos un cpp o python
(define identificar_archivo
  (lambda (archivo tipo_archivo)
  (cond
    [(string-ci=? tipo_archivo "c++") (leer_c++ (list archivo) txt)]
    [(string-ci=? tipo_archivo "python") (leer_python (list archivo) txt)]
    [else ((write escribir_archivo) (cons "\n"(cons "</html>" (cons "\n" (cons "</body>" (cons "\n"(cons "entrada no valida" txt) ))))))]
  )
 )   
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion que recibe la ruta del archivo a leer y el tipo de documento que es en un string
(define input
  (lambda (archivo tipo_archivo)
    (identificar_archivo archivo tipo_archivo)
   )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion para leer el archivo caracter por caracter
(define list
  (lambda (archivo)
    (read-1strings archivo)
   )
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Funcion para escribir en un archivo de texto
(define write
  (lambda (destino)
    (lambda (txt)
    (write-file destino (string-join (reverse txt) ""))
  )
 )  
)
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;CASOS DE PRUEBA
;(input leer_archivo_c++ "c++")
;(input leer_archivo_python "python")