:- module(Input,_,[]).
:- use_module(library(write)).
:- use_module(library(file_utils)).
:- use_module(library(iso_byte_char)).
:- use_module(library(read)).
:- use_module(library(lists)).


main(Archivo,Res) :-
    	file_terms(Archivo,R),
	acomodar_str(R,Res).
    

acomodar_str([],[]).


acomodar_str([H|T],[R2|R]):-
	acomodar_str(T,R),
	sequence_to_list(H,L),
	dar_formato(L,R2).
%	append(R2,R,Resultado).

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