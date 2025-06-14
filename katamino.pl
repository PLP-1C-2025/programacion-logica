:- use_module(piezas).


%insertar(?x,+L,?Lx).
insertar(X,L,Lx) :- append(P,S,L), append(P,[X|S],Lx).

%sublista(+Descartar, +Tomar, +L, -R).
%sublista(_,_,[],[]).
sublista(Desc2,Tomar2,L2,R2) :- 
 length(L2,LenL2),
 append(Noquiero,Ele2,L2), 
 length(Noquiero, Desc2), 
 LenEspEle2 is (LenL2 - Desc2), 
 length(Ele2, LenEspEle2), 
 append(R2,_,Ele2), 
 length(R2,Tomar2).

%Ej:sublista(2,3,[a,b,c,d,e,f],R).

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

coordenadas([[T|F]|TS], (I,J)) :- length([T|F],I), 
                                  length([[T|F]|TS],J),
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