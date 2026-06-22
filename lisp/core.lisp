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
