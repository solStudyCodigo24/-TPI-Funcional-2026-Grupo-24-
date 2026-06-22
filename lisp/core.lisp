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
