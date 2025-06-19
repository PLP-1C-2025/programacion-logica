:- use_module(piezas).


%insertar(?x,+L,?Lx).
insertar(X,L,Lx) :- append(P,S,L), append(P,[X|S],Lx).

%sublista(+Descartar, +Tomar, +L, -R).
sublista(Descartar, Tomar, L, R) :- 
 append(ElemsDescartar, ElemsPosibles, L), 
 length(ElemsDescartar, Descartar), 
 append(R, _, ElemsPosibles), 
 length(R, Tomar).


%sublista(+Descartar, +Tomar, +L, -R).
sublista1(Descartar, Tomar, L, R) :-  verificarSubLista(_, ElemsPosibles, Descartar, L), verificarSubLista(R, _, Tomar, ElemsPosibles).

verificarSubLista(L1, L2, N, L) :- append(L1, L2, L), length(L1, N).

/*
Es reversible: sigue logrando unificar correctamente si se cambia cuál de las variables entre Descartar y R está instanciada.
Para construir la sublista, usamos únicamente los predicados length/2 y append/3. Estos tienen 2 y 3 argumentos respectivamente, y ambos admiten que uno (o incluso dos, en el caso de append/3) permanezcan sin instanciar.
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
tamano([E|M], F, C) :- length(E,C), length([E|M],F).

%coordenadas(+T, -IJ)
%coordenadas([],IJ).
% (1,1) (1,2) (1,3) (1,4) 
% (2,1) (2,2) (2,3) (2,4)
% (3,1) (3,2) (3,3) (3,4)
% (4,1) (4,2) (4,3) (4,4)
% (5,1) (5,2) (5,3) (5,4)
/*
coordenadas([[T|F]|TS], (I,J)) :- length([T|F],I), 
                                  length([[T|F]|TS],J),
                                  between(1, NEWI, I),
                                  between(1, NEWJ, J).
*/
%coordenadas(+Tablero, -Coordenada)
coordenadas([[T|F]|TS], (I, J)) :-length([[T|F]|TS], NEWI),
                                  length([T|F], NEWJ),
                                  between(1, NEWI, I),
                                  between(1, NEWJ, J).



/*Predicados: =, sort, msort, length, nth1, nth0, member, append, last,
 between, is_list, list_to_set, is_set, union, intersection, subset, subtract, select, delete, reverse, atom, number, numlist, sum_list, flatten

kPiezas(K, PS) :- .
%quiero todos los subconjuntos del conjunto de longitud k, todas las permutaciones distintas de longitud k
%kpiezas(+K, -PS)
piezas(K,PS) :- 
 */

/*
nombrePiezas(L), length(PS,K), append([A],L1,L),
                not(member(A, L1)), member(A,PS).

piezas(K,PS) :- append (Algo,_,ListaK), length (ListaK, K), [a, b, c, d, e, f, g, h, i, j, k, l] 
length(PS,K),
*/

combinar(0, _, []).
combinar(K, [X|XS], [X|YS]) :- K > 0, NEWK is K-1, combinar(NEWK, XS, YS). % En este si
combinar(K, [_|XS], YS) :- K > 0, combinar(K, XS, YS). % Este es el caso en el que no agarro nada

kPiezas(K,PS) :- nombrePiezas(L), combinar(K, L, PS).

%seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)

seccionTablero(T, ALTO, ANCHO, (I,J), ST) :- NEWI is I - 1, NEWJ is J - 1,
                                             sublista(NEWI, ALTO, T, R), columnasValidas(NEWJ, ANCHO, R, SOLUCION), ST = SOLUCION.

columnasValidas(_, _, [], []).
columnasValidas(J, ANCHO, [F|R], SOLUCION) :- sublista(J, ANCHO, F, FRES), 
                                               append([FRES], RF, SOLUCION), 
                                               columnasValidas(J,ANCHO,R,RF).

%ubicarPieza(+Tablero, +Identificador)
ubicarPieza(Tablero, Identificador) :- pieza(Identificador, E), tamano(E, F, C),coordenadas(Tablero,(X,Y)), seccionTablero(Tablero, F, C, (X,Y), E).

%poda(+P, +T)
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

%ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [I|Identificadores]) :- poda(Poda, Tablero), ubicarPieza(Tablero,I), ubicarPiezas(Tablero, Poda, Identificadores).

%llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, Tablero) :- tablero(Columnas, Tablero), kPiezas(Columnas, PiezasPosibles), ubicarPiezas(Tablero, Poda, PiezasPosibles).

cantSoluciones(Poda, Columnas, N) :-
findall(T, llenarTablero(Poda, Columnas, T), TS),
length(TS, N).

% MAQUINA VIRTUAL
% time(cantSoluciones(sinPoda, 3, N)).
% 53,325,481 inferences, 3.217 CPU in 3.221 seconds (100% CPU, 16575187 Lips)
% N = 28.

% time(cantSoluciones(sinPoda, 4, N)).
% 2,350,843,172 inferences, 145.134 CPU in 145.124 seconds (100% CPU, 16197786 Lips)
% N = 200.

%todosGruposLibresModulo5(+T)
%todosGruposLibresModulo5(T) :- recuperarLibre(T, ST), agrupar(ST, G), not((member(X,G), length(X,L), mod(L, 5) \= 0)).
todosGruposLibresModulo5(T) :- recuperarLibre(T, ST), agrupar(ST, G), forall(member(X,G), moduloCinco(X)). % A REVISAR

moduloCinco(X) :- length(X,L), mod(L, 5) =:= 0.

% MAQUINA VIRTUAL
% ?- time(cantSoluciones(podaMod5, 3, N)).
% 30,298,534 inferences, 1.839 CPU in 1.826 seconds (101% CPU, 16479418 Lips)  -- 101 CPU!?!?!?!?!?!?!?!?!?!
% N = 28.

% ?- time(cantSoluciones(podaMod5, 4, N)).
% 696,978,975 inferences, 44.081 CPU in 44.207 seconds (100% CPU, 15811206 Lips)
% N = 200.

recuperarLibre(T,ST) :- findall((X,Y),cordLibre(T,(X,Y)),ST).

%cordLibre(+T,+(I,J))
cordLibre(T,(I,J)) :- coordenadas(T, (I,J)), seccionTablero(T,1,1,(I,J),[[Valor]]), var(Valor).