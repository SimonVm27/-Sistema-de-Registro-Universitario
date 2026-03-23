
:- dynamic estudiantes/4.
:- initialization(main).

main:-
cargar,
menu.

menu:-
writeln('-----Menu-----'),
writeln('1. Registro'),
writeln('2. Buscar Estudiante'),
writeln('3. Calcular Tiempo En La Universidad '),
writeln('4. Listado de estudiantes '),
writeln('5. Registrar Salida'),
writeln('6. Salir'),
read_line_to_string(user_input, O),
(number_string(Opcion,O) ->
    ejecutar(Opcion)
    ;
    writeln('Opcion no valida'),
    menu
).

ejecutar(1):- !,registro, guardar, menu.
ejecutar(2):- !,busqueda, menu.
ejecutar(3):- !,calcular_tiempo, menu.
ejecutar(4):- !,listar, menu.
ejecutar(5):- !,registro_salida, guardar, menu.
ejecutar(6):- !,guardar, writeln('Programa Finalizado'), halt.
ejecutar(_):- !,
writeln('Opcion no valida'),
menu.


%-----------Registro----------
registro:- 
writeln('Ingrese el nombre del estudiante:'),
read_line_to_string(user_input,Nombre),
writeln('Ingrese el id del estudiante: '),
read_line_to_string(user_input,Ide),
((Nombre=="" ; Ide== "")->
    writeln('Nombre o Id no valido')
    ;
    tomar_hora(Entrada),

    ( estudiantes(Ide,_,_,sin_salida) ->
        writeln('Ya esta dentro')
    ; estudiantes(Ide,_,_,_) ->
        writeln('Ese ID ya existe en el sistema')
    ;
        ( assertz(estudiantes(Ide,Nombre,Entrada,sin_salida)),
        writeln('Registrado con exito') )
    )).



tomar_hora(Hora):-
get_time(Segundos),                               % toma los segundos pasados desde 1 enero 1970
stamp_date_time(Segundos,TiempoLocal,local),                     % toma la zona horaria completa año,mes,dia,hora,etc
date_time_value(hour,TiempoLocal,H),                       % guarda en H solo las horas de la zona horaria
date_time_value(minute,TiempoLocal,M),                      % guarda en M solo los minutos de la zona horaria 

(H<10 ->                                          % Si las horas son menores que 10 le agrega un 0 delante ejem: 4-> 04
format(atom(Hor), '0~w',[H]);
format(atom(Hor), '~w',[H])),
                                                 
(M<10 ->                                         % Si los minutos son menores que 10 le agrega un 0 delante ejem: 8 -> 08
format(atom(Min), '0~w',[M]);
format(atom(Min), '~w',[M])),

format(atom(Hora), '~w:~w', [Hor,Min]).


%--------------------- buscar-----------
busqueda:-
writeln('Ingrese el ide del estudiante que desea buscar'),
read_line_to_string(user_input,Ide),
(estudiantes(Ide,Nombre,Entrada,Salida) ->

    (format('Nombre: ~w~n', [Nombre]),
    format('Hora de entrada: ~w~n', [Entrada]),
    (Salida == 'sin_salida' -> 
        writeln('El estudiante aun esta en la universidad')
        ; format('Hora de salida: ~w~n', [Salida])));
        
         writeln('Actualmente no hay un estudiante con ese Id en la universidad')
).




%----------------------listar---------
listar:-
nl,
findall( est(Ide,Nombre,Entrada,Salida), estudiantes(Ide,Nombre,Entrada,Salida), Listado),            % crea lista con todos los estudiantes
(Listado == [] ->
    writeln('Actualmente no hay estudiantes registrados')
    ;
    datos(Listado)
).



datos([]).
datos([est(Ide,Nombre,Entrada,Salida)|Resto]):-
format('Nombre: ~w~n', [Nombre]),
format('Ide: ~w~n', [Ide]),
format('Hora de entrada: ~w~n', [Entrada]),
(Salida == 'sin_salida' ->
    writeln('El estudiante esta actualmente en la universidad')
    ;
    format('Hora de salida: ~w~n', [Salida])),
writeln('-------------------------------------'),
datos(Resto).



%--------------registrar salida-----------------
registro_salida:-
writeln('Ingrese el Id del estudiante que va a salir:'),
read_line_to_string(user_input,Ide),
(estudiantes(Ide,Nombre,Entrada,Salida) -> 
    (Salida \= 'sin_salida' ->
        writeln('El estudiante ya tiene una salida registrada')
        ;
        (tomar_hora(Hora),         
        retract(estudiantes(Ide,Nombre,Entrada,Salida)),                              % Elimina el estudiante con el Id que metio el usuario
        assertz(estudiantes(Ide,Nombre,Entrada,Hora)),                           % registra un nuevo estudiante con los datos del que fue borrado pero cambiando su hora de salida
        writeln('Salida registrada con exito'))
    )
    ;
    writeln('Actualmente no hay un estudiante con ese Id dentro de la universidad')
).



%----------------calcular tiempo---------------
calcular_tiempo:-
writeln('Ingrese el Id del estudiante:'),
read_line_to_string(user_input,Ide),
(estudiantes(Ide,Nombre,Entrada,Salida)->
    (Salida \= 'sin_salida' ->
        (format('Nombre: ~w~n', [Nombre]),
        minutos(Salida,MinS),                                       % Deja en MinS la hora de salida en minutos
        minutos(Entrada,MinE),                                      %  Deja en MinE la hora de entrada en minutos
        Minutos is MinS - MinE,                                      % Guarda en minutos los minutos que el estudiante estuvo dentro de la universidad
        formato(Minutos,Tiempo),                                    % Guarda en Tiempo el tiempo que el estudiante estuvo en la universidad en formato HH:MM
        format('Tiempo que estuvo el estudiante en la universidad (HH:MM): ~w~n' , [Tiempo]))
        ;
        writeln('El estudiante aun esta en la universidad')
    )
    ;
    writeln('Actualmente no se ha registrado entrada de ningun estudiante con ese Id')
).




minutos(Tiempo, TotalMin):-
atom_string(Tiempo,Hora),
split_string(Hora, ':','',[H,M]),                            % separa la hora en formato HH:MM a horas y minutos 
number_string(Hor,H),
number_string(Min,M),
TotalMin is (Hor *60) + Min.


formato(Minutos,Formato):-
H  is Minutos // 60,
( H < 10 ->
    format(atom(Hor),'0~w', [H])
    ;
    format(atom(Hor), '~w', [H])),
M is Minutos mod 60,
( M < 10 -> 
    format(atom(Min), '0~w', [M])
    ;
    format(atom(Min), '~w', [M]) ),
format(atom(Formato), '~w:~w', [Hor, Min]).

guardar:-
open('University.txt', write, University),
forall(estudiantes(Ide,Nombre,Entrada,Salida),
    format(University, "estudiantes(\"~w\",\"~w\",'~w','~w').~n", [Ide,Nombre,Entrada,Salida])),
close(University).
    

cargar:-
retractall(estudiantes(_,_,_,_)),
(exists_file('University.txt') ->
    true
    ;
    open('University.txt',write,U),
    close(U)    
),
open('University.txt',read, University),
repeat,
read(University, E),
(E == end_of_file ->                % si llega al final del archivo cierra y termina el ciclo, sino ingresa al estudiante 
    close(University), !
    ;
    assertz(E),                      
    fail
).

    
