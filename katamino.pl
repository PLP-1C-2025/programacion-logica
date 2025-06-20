:- use_module(piezas).


%sublista(+Descartar, +Tomar, +L, -R).
sublista(Descartar, Tomar, L, R) :-  verificarSubLista(_, ElemsPosibles, Descartar, L), verificarSubLista(R, _, Tomar, ElemsPosibles).

verificarSubLista(L1, L2, N, L) :- append(L1, L2, L), length(L1, N).

/*
Es reversible: sigue logrando unificar correctamente si se cambia cuál de las variables entre Descartar y R está instanciada.
Para construir la sublista, usamos únicamente los predicados length/2 y append/3 dentro de verificarSubLista. Estos tienen 2 y 3 argumentos respectivamente, y ambos admiten que uno (o incluso dos, en el caso de append/3) permanezcan sin instanciar.
Además, pueden utilizarse sin requerir un orden específico: cualquiera de sus argumentos puede dejarse sin instanciar, siempre que haya suficiente información en los otros.
Por esto mismo, sublista/4 funciona en ambos sentidos para los parámetros Descartar y R.
Como dato adicional, el parámetro Tomar también puede no estar instanciado sin afectar la reversibilidad entre Descartar y R.
*/

% tablero(+K, -T)
tablero(K, T) :- generar_filas(5, K, T).

%generar_filas(+Filas, +Columnas, -Tablero)
generar_filas(0, _, []).
generar_filas(N, K, [Fila|Resto]) :-
    N > 0,
    length(Fila, K),         % Fila con K variables distintas
    N1 is N - 1,
    generar_filas(N1, K, Resto).
 

%tamano(+M, -F, -C)
tamano([Fila|Resto], F, C) :- length(Fila,C), length([Fila|Resto],F).


%coordenadas(+T, -IJ)
coordenadas(Matriz, (I, J)) :- tamano(Matriz, F, C), between(1, F, I), between(1, C, J).


combinar(0, _, []).
combinar(K, [X|XS], [X|YS]) :- K > 0, NEWK is K-1, combinar(NEWK, XS, YS). % En este si
combinar(K, [_|XS], YS) :- K > 0, combinar(K, XS, YS). % Este es el caso en el que no agarro nada

%kPiezas(+K, -PS)
kPiezas(K,PS) :- nombrePiezas(L), combinar(K, L, PS).

%seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I,J), ST) :- NEWI is I - 1, NEWJ is J - 1,
                                             sublista(NEWI, ALTO, T, R), columnasValidas(NEWJ, ANCHO, R, ST).

columnasValidas(_, _, [], []).
columnasValidas(J, ANCHO, [F|R], SOLUCION) :- sublista(J, ANCHO, F, FRES), 
                                               append([FRES], RF, SOLUCION), 
                                               columnasValidas(J,ANCHO,R,RF).

%ubicarPieza(+Tablero, +Identificador)
ubicarPieza(Tablero, Identificador) :- pieza(Identificador, E), tamano(E, F, C), coordenadas(Tablero, Coord), seccionTablero(Tablero, F, C, Coord, E).

%poda(+P, +T)
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

%ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [I|Identificadores]) :- poda(Poda, Tablero), ubicarPieza(Tablero, I), ubicarPiezas(Tablero, Poda, Identificadores).

%llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, Tablero) :- tablero(Columnas, Tablero), kPiezas(Columnas, PiezasPosibles), ubicarPiezas(Tablero, Poda, PiezasPosibles).

%cantSoluciones(+Poda, +Columnas, -N)
cantSoluciones(Poda, Columnas, N) :- findall(T, llenarTablero(Poda, Columnas, T), TS), length(TS, N).

% MAQUINA VIRTUAL
% time(cantSoluciones(sinPoda, 3, N)).
% 53,325,481 inferences, 3.217 CPU in 3.221 seconds (100% CPU, 16575187 Lips)
% N = 28.

% time(cantSoluciones(sinPoda, 4, N)).
% 2,350,843,172 inferences, 145.134 CPU in 145.124 seconds (100% CPU, 16197786 Lips)
% N = 200.

%todosGruposLibresModulo5(+T)
todosGruposLibresModulo5(T) :- recuperarLibre(T, ST), agrupar(ST, G), forall(member(Lista,G), moduloCinco(Lista)). % A REVISAR

%moduloCinco(+Lista)
moduloCinco(Lista) :- length(Lista, Ls), mod(Ls, 5) =:= 0.

% MAQUINA VIRTUAL
% ?- time(cantSoluciones(podaMod5, 3, N)).
% 30,298,534 inferences, 1.839 CPU in 1.826 seconds (101% CPU, 16479418 Lips)  -- 101 CPU!?!?!?!?!?
% N = 28.

% ?- time(cantSoluciones(podaMod5, 4, N)).
% 696,978,975 inferences, 44.081 CPU in 44.207 seconds (100% CPU, 15811206 Lips)
% N = 200.

%recuperarLibre(+T,-ST)
recuperarLibre(T,ST) :- findall((X,Y),cordLibre(T,(X,Y)),ST).

%cordLibre(+T,?(I,J))
cordLibre(T, Coord) :- coordenadas(T, Coord), seccionTablero(T, 1, 1, Coord, [[Valor]]), var(Valor).

tests :- forall(between(1,6, N), test(N)).

test(1) :-  sublista(0, 0, [1], R1), R1 = [], 
            sublista(0, 0, [1,2,3], R2), R2 = [], 
            sublista(0, 0, [], R3), R3 = [], 
            sublista(1, 1, [1,2], R4), R4 = [2],
            sublista(0, 2, [1,2], R5), R5 = [1,2],
            sublista(2, 0, [1,2], R6), R6 = [],
            sublista(2, 2, [1,2,3,4], R7), R7 = [3,4]. 



test(2) :-  (tablero(1, T1), T1 = [[_],[_],[_],[_],[_]]),
            (tablero(2, T2), T2 = [[_,_],[_,_],[_,_],[_,_],[_,_]]),
            (tablero(3, T3), T3 = [[_,_,_],[_,_,_],[_,_,_],[_,_,_],[_,_,_]]).


test(3) :-  (tablero(3, T1), tamano(T1, F1, C1), F1 = 5, C1 = 3),
            (pieza(e, E), tamano(E, F2, C2), F2 = 2, C2 = 3),
            (pieza(a, A1), tamano(A1, F31, C31), F31 = 4, C31 = 2),
            (pieza(a, A2), tamano(A2, F32, C32), F32 = 2, C32 = 4),
            (tablero(2, T2), tamano(T2, F4, C4), F4 = 5, C4 = 2),
            (tablero(1, T3), tamano(T3, F5, C5), F5 = 5, C5 = 1).


test(4) :-  tablero(3, T), (coordenadas(T, Coord1), Coord1 = (1,1)), 
            coordenadas(T, (1,1)), 
            coordenadas(T, (2,2)), 
            coordenadas(T, (5,2)),
            coordenadas(T, (4,3)). 


test(5) :-  kPiezas(2, [a,b]),
            kPiezas(1, [b]),
            kPiezas(2, [c,d]),
            kPiezas(5, [a,b,c,d,e]),
            kPiezas(1, [e]),
            (kPiezas(1, Piezas), Piezas = [a]).


test(6) :-
    tablero(3, T),
    seccionTablero(T, 3, 2, (1,2), ST),
    ST = [[_C12,_C13],
          [_C22,_C23],
          [_C32,_C33]].