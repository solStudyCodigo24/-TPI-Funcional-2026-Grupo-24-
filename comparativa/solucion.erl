comparativa/
% agrego requerimiento 1 de lisp pasado a erlang, usando el metodo pattern matching.
transicion_semf(en_rojo, verde) ->
                [en_rojo, cambiar_a_verde];
            transicion_semf(en_verde, amarillo) ->
                [en_verde, cambiar_a_amarillo];
            transicion_semf(en_amarillo, rojo) ->
                [en_amarillo, cambiar_a_rojo];
            transicion_semf(ColorActual, _) ->
                [ColorActual, transicion_invalida].

timer(TiempoUnix) when TiempoUnix < 0 -> 
    "ERROR: LOS SEGUNDOS NO PUEDEN SER NEGATIVOS";
timer(TiempoUnix) ->
    if
        (TiempoUnix rem 225) < 90  -> en_rojo;
        (TiempoUnix rem 225) < 93  -> rojo_intermitente;
        (TiempoUnix rem 225) < 213 -> en_verde;
        (TiempoUnix rem 225) < 216 -> verde_intermitente;
        (TiempoUnix rem 225) < 222 -> en_amarillo;
        true                       -> amarillo_intermitente
    end.
