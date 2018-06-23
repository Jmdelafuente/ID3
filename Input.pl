:- module(Input,_,[]).
:- use_module(library(write)).
:- use_module(library(file_utils)).
%:- use_module(library(iso_byte_char)).
:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(hiordlib)).


main(Archivo,Atrib,Prueba,Entrenamiento) :-
    	file_terms(Archivo,R),
	acomodar_str(R,Res),
	separar_atributos(Res,1,Atrib,Ejemp),
	separar_ejemplos(Ejemp,Prueba,Entrenamiento).
	
separar_atributos([[attribute|T1]|T],Pos,[[Pos|T1]|Res],Ejemplos):-
	Contador is Pos + 1,
	separar_atributos(T,Contador,Res,Ejemplos).

separar_atributos(Ejemplos,_Contador,[],Ejemplos).


contiene(Lista,Cadena,Lista):-
	contains1(Lista,Cadena).

contiene(_,_,_).

separar_ejemplos([],[],[]).

separar_ejemplos([H|[]],[],[H]).

separar_ejemplos([H,H2|T],[H|Prueba],[H2|Entrenamiento]):-
	separar_ejemplos(T,Prueba,Entrenamiento).


	
acomodar_str([],[]).


acomodar_str([H|T],[R2|R]):-
	acomodar_str(T,R),
	sequence_to_list(H,L),
	dar_formato(L,R2).

dar_formato([],[]).

dar_formato(['(-1'|T],['-1'|Res]):-
	dar_formato(T,Res).

dar_formato(['-1)'|T],['-1'|Res]):-
	dar_formato(T,Res).

dar_formato(['(0'|T],['0'|Res]):-
	dar_formato(T,Res).

dar_formato(['0)'|T],['0'|Res]):-
	dar_formato(T,Res).

dar_formato(['(1'|T],['1'|Res]):-
	dar_formato(T,Res).

dar_formato(['1)'|T],['1'|Res]):-
	dar_formato(T,Res).

dar_formato([H|T],[H|Res]):-
	dar_formato(T,Res).