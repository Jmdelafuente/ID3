%:- encoding(utf8).
:- module(ID3v2,_,_).
:- use_module(library(hiordlib)).
:- use_module(library(llists)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(sort)).
%:- use_module(input).

%main(Archivo,Arbol):-
	%analizar_archivo(Archivo,Atrib,_Prueba,Entrenamiento),
	%entrenarArbol(Atrib,Entrenamiento,Arbol).

entrenarArbol(ListaAtributos,ListaEjemplos,Arbol) :-
	analizarEjemplos(ListaEjemplos,Sumas),
	calcular_entropia(Sumas,LEntropias),
	list_butlast(ListaAtributos,LAtr),
	acomodar(LAtr,Sumas,LEntropias,ListaID3),
	sort(ListaID3,LO),
	reverse(LO,LOrdenada),
	algo([],LOrdenada,Arbol).

analizarEjemplos(Ejemplos,EjemplosAnalizados):-
	transpose(Ejemplos,TEjemplosConResultados),
	list_butlast(TEjemplosConResultados,TEjemplos),
	sumarListas(TEjemplos,Sumas),
	last(TEjemplosConResultados,Resultados),
	sumar_positivos(TEjemplos,Sumas,Resultados,EjemplosAnalizados).

acomodar([],[],[],[]).

acomodar([[_Pos|Nombre]|LAtr],[HSuma|LSuma],[HEntropia|LEntropia],[[HEntropia,Nombre,HSuma]|Res]):-
	acomodar(LAtr,LSuma,LEntropia,Res).

sumarListas([],[]).

sumarListas([H|T],[T1|T2]) :-
	sumatoria(H,T1),
	sumarListas(T,T2).


%Dada la lista de sumas [Elemento,Cantidad_Apariciones,Cantidad_True] retorna una lista con la entropia de cada atributo.
calcular_entropia(Sumas,LEntropias):-
	map(Sumas,entropy,LEntropias).

%Cuenta la cantidad de apariciones de cada elemento en una lista de listas y retorna una lista de listas de la forma: [Elemento, Cantidad_de_apariciones].
sumatoria([],[]).

sumatoria([H|T],Res) :-
	contar_miembros([H|T],H,R,NuevaLista),
	sumatoria(NuevaLista,ListaOcurrencias),
	append([R],ListaOcurrencias,Res).

%Cuenta la cantidad de apariciones de cada elemento en la lista retornando una lista compuesta por [Elemento,Cantidad_de_apariciones,0] por cada elemento distinto de la lista.
contar_miembros(Lista,Miembro,[Miembro,R1,0],Resultado):-
	list_member_occ(Lista,Miembro,R1),
	delete(Lista,Miembro,Resultado).

list_member_occ([], _, 0).       % list is empty, 0 occurrences
list_member_occ([X|Xs], X, N) :- % list has the element at the head
    list_member_occ(Xs, X, N0),  % count number of elements in the tail
    N is N0 + 1.                 % the number of occurrences is the
                                 % next natural number
list_member_occ([Y|Xs], X, N) :-
    X \= Y,                     % head and the element are different
    list_member_occ(Xs, X, N).  % occurrences in the tail of the list
                                % is the total number


%Filtra los resultados que son True de la lista de Resultados y llama a aumentar el valor de los elementos que correspondan en la Lista de Sumas

%Lista Transpuesta, Lista de Sumas de apariciones, Lista de Resultados sacada de la Transpuesta, RESULTADO: nueva lista de sumas.
sumar_positivos(_ListaT,ListaS,[],ListaS).

sumar_positivos(ListaT,ListaS,[0|T],NuevaL):-
	map(ListaT,removehead,NListaT),
	sumar_positivos(NListaT,ListaS,T,NuevaL).

sumar_positivos(ListaT,ListaS,[1|T],NuevaL):-
	map(ListaT,nth2(1),Primeros),
	aumentar_lista(Primeros,ListaS,NuevaListaS),
	map(ListaT,removehead,NListaT),
	sumar_positivos(NListaT,NuevaListaS,T,NuevaL).
%	append(NuevaListaS,Res,NuevaL).
	

%Lista de Elementos que tienen resultado true, Lista de Lista con las sumas de las apariciones de cada elemento ordenadas por posicion (cada lista representa la n-esima posicion de aparicion en los ejemplos), Resultado: nueva lista de listas con las sumas de apariciones y la cantidad de veces que dio true.
aumentar_lista([],_TS,[]).

aumentar_lista([HE|TE],[HS|TS],NuevaListaS):-
	first(HS,HE,R),
	aumentar(R,NH),
	delete(HS,R,HS2),
	append([NH],HS2,R2),
	aumentar_lista(TE,TS,NuevaLista),
	append([R2],NuevaLista,NuevaListaS).

%Predicado auxiliar que aumenta en uno el valor de la ultima posicion de la lista.
aumentar(Lista,NuevaLista) :-
	last(Lista,VH),
	list_butlast(Lista,R),
	VN is VH + 1,
	insert_last(R,VN,NuevaLista).

%Entropia general del Dominio.
%entropyD(TotalV,TotalV,Total,Entropia):-
%	Entropia is (-1*(log(1)/log(2))).
%
%
%entropyD(TotalV,0,_Total,Entropia):-
%	Entropia is (-1*(log(1)/log(2))).
%
%entropyD(TotalV,Positivos,Total,Entropia):-
%	A is ((abs(TotalV-Positivos))/Total),
%	B is Positivos/Total,
%	A \=0, B \=0,
%	Entropia is (- A * (log(A)/log(2))) - (B*(log(B)/log(2))).
%
%entropyD(_TotalV,_Positivos,_Total,0).

entropyD(0,_Negativos,_Total,Entropia):-
	Entropia is ((1)*(log(1)/log(2))).


entropyD(_Positivos,0,_Total,Entropia):-
	Entropia is ((1)*(log(1)/log(2))).

entropyD(Positivos,Negativos,Total,Entropia):-
	B is (Positivos/Total),
	A is (Negativos/Total),
%	print('a:'),
%	print(A),
%	print('b:'),
%	print(B),
	A \=0, B \=0,
	Entropia is ((A) * (log(A)/log(2))) + (B*(log(B)/log(2))).

%entropyD(_Positivos,_Negativos,_Total,0).



entropyV(TotalV,Positivos,Total,Entropia):-
	A is (TotalV/Total),
	Negativos is abs(TotalV - Positivos),
	entropyD(Positivos,Negativos,TotalV,Log),
	Entropia is ((A) * Log).


%Entropia de un Atributo.
entropy(SumaAtributo,Entropia):-
	map(SumaAtributo,nth2(2),ListaT),                             %Buscamos las veces que aparecio cada valor para el Attr.
	foldl(ListaT,0,suma,Total),                                   %Sumamos las apariciones de cada valor para calcular Total.
	map(SumaAtributo,removehead,ListaP),
	foldl(ListaP,0,entropy_sum(Total),EntropiaT),                  %Calculamos la sumatoria de las entropias de cada valor para el atributo.
	Entropia is - EntropiaT.

%Sumatoria de la entropia para dominios de valores amplios. Util para FOLDL.
entropy_sum([TotalV,Positivo],Total,0,Entropia):-
	entropyV(TotalV,Positivo,Total,Entropia).

entropy_sum([TotalV,Positivo],Total,Entropia1,Entropia):-
	entropyV(TotalV,Positivo,Total,Entropia2),
	suma(Entropia1,Entropia2,Entropia).
%	Entropia is -EntropiaT.
%Re-escritura de nth para cambiar el orden de los parametros.
nth2(Lista,Pos,Res) :-
	nth(Pos,Lista,Res).

%Retorna el elemento que se encuentre en esa posicion y la lista donde se encuentra. Util para MAP.
nth3(Lista,Pos,Elemento,Lista):-
	nth(Pos,Lista,Elemento).

nth3(_,_,_,[]).

%Elimina el primer elemento de la lista.
removehead([_|Tail], Tail).

%Dada una Lista de Listas LL y un Elemento, retorna la lista que contiene a Elemento en la primer posicion, si se comenta la ultima linea, retorna todas las listas que lo contengan en la primer posicion.
first(LL,Elemento,Lista):-
	map(LL,filter(Elemento),R),
	map(R,nth3(1,Elemento),R2),
	delete(R2,[],R3),
	nth(1,R3,Lista).

%Retorna Lista si Elemento se encuentra en Lista, [] en otro caso.
filter(Lista,Elemento,Lista):-
	contains1(Lista,Elemento).

filter(_,_,[]).

%Retorna la lista pero sin el ultimo elemento.
list_butlast([X|Xs], Ys) :-                      % use auxiliary predicate ...
	list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-  
	list_butlast_prev(Xs, Ys, X1).           % lag behind by one

%Contador de apariciones de elementos que permite consultas non-ground.
count([],_X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C) :-
    sort(List,List1),
    member(X,List1),
    count(List,X,C).

%Re-escritura de Logaritmo para mayor legibilidad.
%log(Numero,Base,Resultado):-
%	Resultado is log(Numero)/log(Base).

%Re-escritura de la Suma para mayor legibilidad.
suma(X,Y,Res):-
	Res is X + Y.



%algoritmo id3

%algo([_Nombre,CantidadApariciones,FueYes],[],Res):-
%	FueNo is CantidadApariciones - FueYes,
%	((FueNo > FueYes , Res = no) ;(Res = yes)).
	
%algo([_Nombre,FueYes,FueYes],_,yes).
%algo([_Nombre,_CantidadApariciones,0],_,no).


algo(_T,[H|LEntropy],[Nombre|Res]):-
	H=[_Entropy,Nombre,TipoVal],
	nodo(Nombre,TipoVal,[Nombre|Hijos]),
	map(Hijos,algo(LEntropy),Res).

nodo(Nombre,TipoVal,[Nombre|HijosGenerados]):-
	map(TipoVal,nodoValor,HijosGenerados).

nodoValor([Nombre,CantidadApariciones,FueYes],[Nombre,[CantidadApariciones,FueYes]]).