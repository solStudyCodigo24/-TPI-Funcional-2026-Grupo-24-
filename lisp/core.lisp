;; REQUERIMIENTO 1: Función de Transición de Colores.
;; ====================================================================
;; FUNCIÓN: transicion-semf
;; NATURALEZA: Pura (dado un timestap, siempre retorna el mismo color)
;; ESTRATEGIA: Selectiva (uso de cond para elegir una transición)
;; IMPACTO: No destructiva (solo calcula y devuelve resultados)
;; ===================================================================
(defun transicion-semf (color-actual cambiar-a)
	(cond 
		((and (eql color-actual 'en-rojo)(eq cambiar-a 'verde)) 
							    (list color-actual 'cambiar-a-verde))
	    ((and (eql color-actual 'en-verde)(eq cambiar-a 'amarillo)) 
							    (list color-actual 'cambiar-a-amarillo))
		((and (eql color-actual 'en-amarillo)(eq cambiar-a 'rojo)) 
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
  (+ 90 3 120 3 6 3))

;;====================================================================
;; FUNCIÓN: timer sin ingresar tiempo estado 
;; NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selectiva (uso de cond y operador matemático rem para obtener el resto)
;; IMPACTO: No destructiva (solo calcula y devuelve resultados)
;;====================================================================
;;(defun timer (tiempo-unix)
  ;;(cond
    ;;((< tiempo-unix 0)  "El tiempo Unix no puede ser negativo")
    ;;((< (rem tiempo-unix (sumar-ciclos)) 90)  'en-rojo)
    ;;((< (rem tiempo-unix (sumar-ciclos)) 210)  'en-verde)
    ;;(t  'en-amarillo)))

; ====================================================================
; FUNCIÓN: evaluar-tiempo-en-ciclo
; NATURALEZA: pura(recibe como parametro el resto de tiempo-unix y estado-actual)
; ESTRATEGIA: compara el tiempo de tiempo-estado, primero con los 90 seg. despues de ello lo resta, y ese resto va 
;comparando con el resto de las evaluaciones hasta llegar, al ciclo correcto.
; IMPACTO: No destructiva 
; ====================================================================

(defun evaluar-tiempo-en-ciclo (resto-tiempo estado-actual)
  (cond
    ((< resto-tiempo (tiempo-estado estado-actual)) 
     estado-actual)
    (t 
     (evaluar-tiempo-en-ciclo 
       (- resto-tiempo (tiempo-estado estado-actual)) 
       (siguiente-estado estado-actual)))))
; ====================================================================
; FUNCIÓN: timer
; NATURALEZA: Pura. Utiliza un cond para evaluar diferentes posiciones 
; e imprimir que color corresponde.
; ESTRATEGIA: Si el tiempo Unix es negativo, muestra un mensaje de error. 
; Si no , llama a la recursión pasándole como parámetro el resto  de tiempo-unix 
;             con sumar-ciclos, y arranca siempre desde el estado 'rojo.
; IMPACTO: No destructiva. No imprime restas intermedias; la consola 
;          mostrará el resultado únicamente cuando la función recursiva 
;          llegue a su caso base.
; ====================================================================

(defun timer (tiempo-unix)
  (cond
    ((< tiempo-unix 0) "El tiempo Unix no puede ser negativo")
    (t (evaluar-tiempo-en-ciclo (rem tiempo-unix (sumar-ciclos)) 'rojo))))

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
;;====================================================================
;; REQUERIMIENTO 4: Funciones de Duración y Evaluación del Ciclo
;; FUNCIÓN: calcular-duracion-total 
;; NATURALEZA: Pura.
;; ESTRATEGIA: Devuelve la suma de la cant. de segundos que tarda el semaforo en rojo, 
;; el semáforo en amarillo y el semáforo en verde.
;; IMPACTO: No destructiva. Función auxiliar para la función recomendación-ciclo-Ing. y calcular porcentaje hora.
;;====================================================================
(defun duracion-ciclo (semf-R semf-V semf-A)
  (+ semf-R semf-v semf-A))
;;====================================================================
;; FUNCIÓN: Evaluar-Ciclo (Req. 4.b)
;; NATURALEZA: Pura.
;; ESTRATEGIA: Ingresa como parámetro la cantidad de seg que dura la suma de un ciclo.
;; Devuelve un mensaje correspondiente a las distintas opciones recomendadas por ingenieros civiles.
;; IMPACTO: No destructiva
;;====================================================================
(defun evaluar-ciclo (nro-segundos)
  (cond
    ((< nro-segundos 35)
     "Ciclo por debajo del mínimo recomendado por 
	 Ingenieros Civiles.Existe riesgo de colision.")
    ((<= nro-segundos 150)
     "Ciclo dentro de los parametros recomendados 
	 en por la Ingenieros Civiles ")
    (t
     "Ciclo por encima del máximo recomendado por
	 Ingenieros Civiles.Existe riesgo de incrementar
	 incumplimiento en las normas de transito")))
;;====================================================================
;; FUNCIÓN: Recomendacion-ciclo-ing
;; NATURALEZA: Pura.
;; ESTRATEGIA: Devuelve el mensaje segun la suma de calcular los ciclos, por lo tanto, 
;; es una composición de la función Evaluar-ciclo y calcular-duracion-total.
;; IMPACTO: No destructiva.
;;====================================================================
(defun recomendacion-ciclo (semf-R semf-A semf-V)
  (Evaluar-ciclo (calcular-duracion-total semf-R semf-A semf-V)))
;;====================================================================
;; REQUERIMIENTO 5: Planificación Temporal
;; FUNCIÓN: Ciclos-por-tiempo
;; NATURALEZA: Pura.
;; ESTRATEGIA: Calcula la cant. de ciclos completados: pasa los minutos a segundos, 
;; luego divide con sumar-ciclos. Utiliza float para los decimales y truncate para obtener el entero.
;; IMPACTO: No destructiva
;; ====================================================================
(defun ciclos-por-tiempo (cant-min)
  (truncate (float (/ (* cant-min 60) (sumar-ciclos)))))

;; REQUERIMIENTO 6: Calcular Porcentaje.
;;====================================================================
;; FUNCIÓN: calcularPorcentajesHora
;; NATURALEZA: Pura
;; ESTRATEGIA: Retorna una lista con el color del semaforo correspondiente y el cálculo de su porcentaje.
;; Realiza una composición funcional reutilizando calcular-ciclos para un código limpio.
;; IMPACTO: No destructiva
;; ====================================================================
(defun calcular-Porcentaje-Hora (semf-R semf-A semf-V)
  (list
    'ROJO     (float (* (/ (* semf-R (truncate (/ 3600 (calcular-duracion-total semf-R semf-A semf-V)))) 3600) 100))
    'VERDE    (float (* (/ (* semf-V (truncate (/ 3600 (calcular-duracion-total semf-R semf-A semf-V)))) 3600) 100))
    'AMARILLO    (float (* (/ (* semf-A (truncate (/ 3600 (calcular-duracion-total semf-R semf-A semf-V)))) 3600) 100))))




;; ============================================================== 
;; FUNCIÓN: de transición de estados 
;; NATURALEZA: Pura. 
;; ESTRATEGIA: pregunta ¿Cuál es el próximo estado del semáforo?, entonces recibe un estado y devuelve el siguiente. 
;; IMPACTO: No destructiva 
;; ============================================================ 
 
(defun siguiente-estado (estado) 
  (cond 
	((eql estado 'rojo) 'rojo-intermitente) 
	((eql estado 'rojo-intermitente) 'verde) 
	((eql estado 'verde) 'verde-intermitente) 
	((eql estado 'verde-intermitente) 'amarillo) 
	((eql estado 'amarillo) 'amarillo-intermitente) 
	((equl estado 'amarillo-intermitente) 'rojo) 
	(t 'estado-invalido))) 

;; ==============================================================
;; FUNCIÓN: tiempo-estado
;; NATURALEZA: Pura.
;; ESTRATEGIA: Recibe un estado del semáforo y devuelve
;; la cantidad de segundos que debe permanecer activo ese estado.
;; Los estados intermitentes duran 3 segundos cada uno.
;; IMPACTO: No destructiva
;; ==============================================================
(defun tiempo-estado (estado)
  (cond
    ((eql estado 'rojo) 90)
    ((eql estado 'rojo-intermitente) 3)
    ((eql estado 'verde) 120)
    ((eql estado 'verde-intermitente) 3)
    ((eql estado 'amarillo) 6)
    ((eql estado 'amarillo-intermitente) 3)
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

;; ====================================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura. Escribe un archivo de texto directamente en el disco.
;; ESTRATEGIA: Usa mapcar junto a una función lambda que recibe el registro, 
;; para recorrer la lista de datos y darles formato línea por línea.
;; IMPACTO: No destructiva. No modifica la lista original de datos; solo lee 
;; la información y la guarda en el archivo.
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
