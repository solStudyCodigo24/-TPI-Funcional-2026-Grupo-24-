;; REQUERIMIENTO 1: Función de Transición de Colores.
;; ====================================================================
;; FUNCIÓN: transicion-semf
;; NATURALEZA: Pura (dado un timestap, siempre retorna el mismo color)
;; ESTRATEGIA: Selectiva (uso de cond para elegir una transición)
;; IMPACTO: No destructiva (solo calcula y devuelve resultados)
;; ===================================================================
(defun transicion-semf (color-actual cambiar-a)
	(cond 
		((and (eq color-actual 'en-rojo)(eq cambiar-a 'verde)) 
							    (list color-actual 'cambiar-a-verde))
	    ((and (eq color-actual 'en-verde)(eq cambiar-a 'amarillo)) 
							    (list color-actual 'cambiar-a-amarillo))
		((and (eq color-actual 'en-amarillo)(eq cambiar-a 'rojo)) 
							    (list color-actual 'cambiar-a-rojo))
	    (t (list color-actual 'accion-por-defecto))
	)
)

;REQUERIMIENTO 2: Temporizador Automático
;;====================================================================
;; FUNCIÓN: Sumar-ciclos
;; NATURALEZA: Pura.
;; ESTRATEGIA: Devuelve una suma de la cant. de segundos de duración del semf. 
;; en rojo 90 seg., amarillo 6 seg. , verde 120 seg. , las tres veces que aparecen
;; 3 es porque se agrega la intermitencia, del punto iterativo.
;; IMPACTO: No destructiva (Auxiliar para funcion timer y recomendacion-ciclo-ing)
;;====================================================================
(defun sumar-ciclos ()
  (+ 90 3 6 3 120 3))

;;====================================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selectiva (uso de cond y operador matemático rem para obtener el resto)
;; IMPACTO: No destructiva (solo calcula y devuelve resultados)
;;====================================================================
(defun timer (tiempo-unix)
  (cond
    ((< tiempo-unix 0)  "El tiempo Unix no puede ser negativo")
    ((< (rem tiempo-unix (sumar-ciclos)) 90)  'en-rojo)
    ((< (rem tiempo-unix (sumar-ciclos)) 96)  'en-amarillo)
    (t  'en-verde)))

;REQUERIMIENTO 3: Función de Auditoría
;; ====================================================================
;; FUNCION: auditoria
;; NATURALEZA: Impura (imprime en pantalla mediante format)
;; ESTRATEGIA: Secuencial e independiente, utiliza una condición IF (>= tiempo-Unix 0)
;; IMPACTO: No destructiva
;; ====================================================================
(defun auditoria (tiempo-Unix color-anterior color-nuevo)
  (if (>= tiempo-Unix 0)
      (format t "Tiempo ~d: la luz ha cambiado de ~a a ~a"
              tiempo-Unix color-anterior color-nuevo)
      nil))
;; ==============================================================
;; FUNCIÓN: de transición de estados
;; NATURALEZA: Pura.
;; ESTRATEGIA: pregunta ¿Cuál es el próximo estado del semáforo?, entonces recibe un estado y devuelve el siguiente.
;; IMPACTO: No destructiva
;; ============================================================

(defun siguiente-estado (estado)
  (cond
    ((equal estado 'rojo) 'rojo-intermitente)
    ((equal estado 'rojo-intermitente) 'verde)

    ((equal estado 'verde) 'verde-intermitente)
    ((equal estado 'verde-intermitente) 'amarillo)

    ((equal estado 'amarillo) 'amarillo-intermitente)
    ((equal estado 'amarillo-intermitente) 'rojo)

    (t 'estado-invalido)))
 ;; ==============================================================
;; FUNCIÓN:manejo de los tiempos
;; NATURALEZA: Pura.
;; ESTRATEGIA: su trabajo es decir¿Cuántos segundos dura este estado?,Recibe un estado y devuelve un número.
;; IMPACTO: No destructiva
;; ============================================================
(defun tiempo-estado (estado)
  (cond
    ((equal estado 'rojo) 30)
    ((equal estado 'verde) 25)
    ((equal estado 'amarillo) 5)

    ((equal estado 'rojo-intermitente) 3)
    ((equal estado 'verde-intermitente) 3)
    ((equal estado 'amarillo-intermitente) 3)

    (t 0)))
  
;; ==============================================================
;; FUNCIÓN:mostrar colores
;; NATURALEZA: Pura.
;; ESTRATEGIA:  mostrar información al usuario.Recibe un estado y devuelve un mensaje o texto para mostrar.
;; IMPACTO: No destructiva
;; ============================================================


 (defun mostrar-estado (estado)
  (cond
    ((equal estado 'rojo) "ROJO")
    ((equal estado 'rojo-intermitente) "ROJO-INTERMITENTE")

    ((equal estado 'verde) "VERDE")
    ((equal estado 'verde-intermitente) "VERDE-INTERMITENTE")

    ((equal estado 'amarillo) "AMARILLO")
    ((equal estado 'amarillo-intermitente) "AMARILLO-INTERMITENTE")

    (t "ESTADO INVALIDO")))
;;====================================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura( Escribe un archivo en el  disco)
;; ESTRATEGIA: Orden Superior (hace un mapeo mediante mapcar)
;; IMPACTO: No destructiva 
;; ====================================================================
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt" 
                          :direction :output 
                          :if-exists :supersede)
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================%%")
    (mapcar #'(lambda (registro)
                (format stream "A - Transición: ~A -> ~A%"
                        (first registro)
                        (second registro)
                        (third registro)))
            datos)
    (format stream "~%--- Fin del Informe ---")))

;;====================================================================
;; FUNCIÓN: tiempo-estado
;; NATURALEZA: Impura (Lee información desde un archivo externo JSON)
;; ESTRATEGIA: Lectura de configuración mediante archivo JSON y selección
;;             condicional del tiempo según el estado recibido.
;; IMPACTO: No destructivo (Solo consulta datos, no modifica archivos)
;;

(defun tiempo-estado (estado)
  (with-open-file (stream "config.json" :direction :input)
    (let* ((config (json:decode-json stream))
           (rojo (cdr (assoc "rojo" config :test #'string=)))
           (verde (cdr (assoc "verde" config :test #'string=)))
           (amarillo (cdr (assoc "amarillo" config :test #'string=)))
           (intermitente (cdr (assoc "intermitente" config :test #'string=))))
      (cond
        ((equal estado 'rojo) rojo)
        ((equal estado 'verde) verde)
        ((equal estado 'amarillo) amarillo)

        ((or (equal estado 'rojo-intermitente)
             (equal estado 'verde-intermitente)
             (equal estado 'amarillo-intermitente))
         intermitente)

        (t 0)))))


archivo config.json:

{
  "rojo": 90,
  "verde": 120,
  "amarillo": 6,
  "intermitente": 3
}


