% id3(+Ejemplos,+Objetivo,+Atributos): aprender a clasificar Ejemplos
% segun las distintas Clases de Objetivo en base a los Valores de
% Atributos; lo aprendido se refleja en reglas:
% Categoria <= Condicion $ Casos
% donde:
% - Categoria es Objetivo = Clase.
% - Condicion es una conjuncion de condiciones Atributo = Valor, o true.
% - Casos es el numero de ejemplos que coinciden exactamente con la
% regla.

id3(Ejs,Obj,Ats) :-
  nl, nl, writeln('COMIENZO DE LA CLASIFICACION ID3.'), nl, nl,
  asserta(margen(2)),
  (odbc_current_connection(C,Fuente),!),
  odbc_get_connection(C,database_name(BD)),
  write('FUENTE DE DATOS => '), writeln(Fuente),
  write('  BASE DE DATOS => '), writeln(BD),
  write('EJEMPLOS        => '), writeln(Ejs),
  write('OBJETIVO        => '), writeln(Obj),
  write('ATRIBUTOS       => '), nl,
  noclaves(Ejs,Obj,Ats,NCs),
  nl, writeln('RAIZ:'), nl,
  id3(Ejs,true,Obj,NCs),
  retract(margen(_)),
  nl, writeln('FIN DE LA CLASIFICACION ID3.').

% id3(+Ejemplos,+Condicion,+Objetivo,+Atributos): El algoritmo de
% aprendizaje, tiene tres casos a considerar:
% - Si no hay mas atributos a partir de los cuales clasificar.
% - Si todos los Ejemplos pertenecen a una sola Clase.
% - Llamada recursiva, una vez obtenido el Atributo "Mejor
% Clasificador".
% Es una implementacion de id3, pero en lugar de construir un arbol de
% decision, al llegar a cada hoja se guardan las reglas obtenidas en el
% camino desde la raiz.

id3(Ejs,Cond,Obj,[]) :-
  !,
  valores(Ejs,Cond,Obj,Cls),
  reglas(Ejs,Cond,Obj,Cls).
id3(Ejs,Cond,Obj,_) :-
  valores(Ejs,Cond,Obj,[Cl]),
  !,
  reglas(Ejs,Cond,Obj,[Cl]).
id3(Ejs,Cond,Obj,Ats) :-
  mejor(Ejs,Cond,Obj,Ats,Mej,Vals),
  delete(Ats,Mej,RestoAts),
  id3s(Ejs,Cond,Obj,RestoAts,Mej,Vals).

% id3s(+Ejemplos,+Condicion,+Objetivo,+Atributos,+Mejor,+Dominio):
% Llamadas recursivas a id3 agregando los distintos pares Mejor = Valor
% a Condicion.

id3s(Ejs,Cond,Obj,Ats,Mej,[Val|Vals]) :-
  fortalecer(Cond,Mej = Val,NuevaCond),
  margen(M),
  tab(M), writeln('NODO:'),
  tab(M), writeln(NuevaCond), nl,
  M1 is M+2, asserta(margen(M1)),
  id3(Ejs,NuevaCond,Obj,Ats),
  retract(margen(_)),
  !,
  id3s(Ejs,Cond,Obj,Ats,Mej,Vals).
id3s(_,_,_,_,_,[]).



entrenarArbol(ListaAtributos,ListaEjemplos) :-
	inicializarContadores(ListaAtributos,ListaEjemplos),
	inicializarAtributos(ListaAtributos,_Posicion),
	analizarEjemplos(ListaEjemplos).

analizarEjemplos([]).

analizarEjemplos([H|T]) :-
	analizarEjemplo(H,_Pos),
	analizarEjemplos(T).

analizarEjemplo([],0).

analizarEjemplo([H|T],Posicion):-
	analizarEjemplo(T,Posicion),
	Pos is Posicion + 1,
	sumaCantidad(Pos,H,_Suma).

cantidadesAttr([[HAtr|TAtr],]).

inicializarContadores([HAtr|[]],[HPosibleValores|[]]) :-
	inicializarContador(HAtr,HPosibleValores).

inicializarContadores([HAtr|[]],[HPosibleValores|TPosibleValores]) :-
	inicializarContadores([HAtr|[]],TPosibleValores),
	inicializarContador(HAtr,HPosibleValores).

inicializarContadores([HAtr|TAtr],[HPosibleValores|TPosibleValores]) :-
	inicializarContador(HAtr).

inicializarContador(Atributo,PosibleValor) :-
	asserta(acumulador(Atributo,PosibleValor,0)).

inicializarAtributos([],0).

inicializarAtributos([HAtr|TAtr],Posicion) :-
	inicializarAtributos(TAtr,Pos),
	Pos is Posicion + 1,
	inicializarAtributo(HAtr,Posicion).

inicializarAtributo(Atributo,Posicion) :-
	asserta(posicion(Atributo,Posicion)).

sumaCantidad(Atributo,PosibleValor,Suma) :-
	acumulador(Atributo,PosibleValor,Acumulador),
	Suma is Acumulador + 1,
	retract(acumulador(Atributo,PosibleValor,Acumulador)),
	asserta(acumulador(Atributo,PosibleValor,Suma)).

	
sumatoria([],T,T).
sumatoria([N|Ns],P,T) :- P1 is P + N, !, sumatoria(Ns,P1,T).
