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

%tablero(+K, -T)
tablero(Tfila,Matriz) :- 
 append([],_,MatrizFila),
 length(MatrizFila,Tfila), repetir(MatrizFila,5,Matriz).

%repetir(+Elem,+Cantidad,-ListaCopia) %concatena
repetir(_,0,[]).
repetir(Elem,Cantidad,[Elem|ListaCopia]) :- Resta is Cantidad-1, repetir(Elem,Resta,ListaCopia).

%tamano(+M, -F, -C)
tamano([E|M], F, C) :- length(E,F), length([E|M],C).

%coordenadas(+T, -IJ)
%coordenadas([],IJ).
coordenadas([TableroHead|TableroTail],(P1,P2)) :-length(TableroHead,LenColum),length([TableroHead|TableroTail],LenFila), 
P1 =< LenColum,P1 >= 1,
P2 =< LenFila, P2 >= 1,
P1 = 