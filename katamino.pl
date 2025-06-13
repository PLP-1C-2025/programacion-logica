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

% generar_filas(+Filas, +Columnas, -Tablero)
generar_filas(0, _, []).
generar_filas(N, K, [Fila|Resto]) :-
    N > 0,
    length(Fila, K), 
    N1 is N - 1,
    generar_filas(N1, K, Resto).


%tamano(+M, -F, -C)
tamano([E|M], F, C) :- length(E,F), length([E|M],C).

%coordenadas(+T, -IJ)
%coordenadas([],IJ).
coordenadas([TableroHead|TableroTail],(P1,P2)) :- length(TableroHead,LenColum),length([TableroHead|TableroTail],LenFila), 
P1 =< LenColum,P1 >= 1,
P2 =< LenFila, P2 >= 1,
P1 = 

% (1,1) (1,2) (1,3) (1,4) 
% (2,1) (2,2) (2,3) (2,4)
% (3,1) (3,2) (3,3) (3,4)
% (4,1) (4,2) (4,3) (4,4)
% (5,1) (5,2) (5,3) (5,4)

coordenadas([[T|F]|TS], (I,J)) :- length([T|F],I), 
                                  length([[T|F]|TS],J),
                                  between(1, I, NEWI),
                                  between(1, J, NEWJ).
                                  
                                  

/*Predicados: =, sort, msort, length, nth1, nth0, member, append, last,
 between, is_list, list_to_set, is_set, union, intersection, subset, subtract, select, delete, reverse, atom, number, numlist, sum_list, flatten
 */








