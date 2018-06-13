%:- encoding(utf8).
:- module(ID3v2,[]).

entrenarArbol(ListaAtributos,ListaEjemplos) :-
	inicializarAtributos(ListaAtributos,_Posicion),
	analizarEjemplos(ListaEjemplos).


inicializarAtributos([],0).

inicializarAtributos([HAtr|TAtr],Posicion) :-
	inicializarAtributos(TAtr,Pos),
	Pos is Posicion + 1,
	inicializarAtributo(HAtr,Posicion).

inicializarAtributo(Atributo,Posicion) :-
	asserta(posicion(Atributo,Posicion)).

analizarEjemplos(Ejemplos,Sumas):-
	transpose(Ejemplos,TEjemplos),
	sumarListas(TEjemplos,Sumas).

sumarListas([],[]).

sumarListas([H|T],[T1|T2]) :-
	sumatoria(H,0,T1),
	sumarListas(T,T2).

	
analizarEjemplo([HEj|_],HEj).
	
sumatoria([],T,T).
sumatoria([N|Ns],P,T) :- P1 is P + N, !, sumatoria(Ns,P1,T).

%From SWI-Prolog
transpose(Ls, Ts) :-
        must_be(list(list), Ls),
        lists_transpose(Ls, Ts).

lists_transpose([], []).
lists_transpose([L|Ls], Ts) :-
        maplist(same_length(L), Ls),
        foldl(transpose_, L, Ts, [L|Ls], _).

transpose_(_, Fs, Lists0, Lists) :-
        maplist(list_first_rest, Lists0, Fs, Lists).

list_first_rest([L|Ls], L, Ls).