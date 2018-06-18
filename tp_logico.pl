%televidente(Persona)
televidente(juan).
televidente(nico).
televidente(maiu).
televidente(gaston).
televidente(aye).

%1 Punto A: Quién mira qué
%mira(Persona,Serie).
mira(juan,himym).
mira(juan,futurama).
mira(juan,got).
mira(nico, starWars).
mira(nico, got).
mira(maiu,starWars).
mira(maiu,got).
mira(gaston, hoc).
%Alf no mira ninguna serie no se define que ya solo nos importa los hechos verdaderos.

%esPopular(Serie).
esPopular(got).
esPopular(hoc).
esPopular(starWars).

%quiereVer(Persona, Serie).
quiereVer(juan, hoc).
quiereVer(aye, got).
quiereVer(gaston, himym).

%episodiosSerie(Serie, Temporada, Capitulos).
episodiosSerie(got, 3, 12).
episodiosSerie(got, 2, 10).
episodiosSerie(himym, 1, 23).
episodiosSerie(drHouse, 8, 16).

%2 Anexo: Lo que pasó, pasó

%paso(Serie, Temporada, Episodio, LoQuePaso)
paso(futurama, 2, 3, muerte(seymourDiera)).
paso(starWars, 10, 9, muerte(emperor)).
paso(starWars, 1, 2, relacion(parentesco, anakin, rey)).
paso(starWars, 3, 2, relacion(parentesco, vader, luke)).
paso(himym, 1, 1, relacion(amorosa, ted, robin)).
paso(himym, 4, 3, relacion(amorosa, swarley, robin)).
paso(got, 4, 5, relacion(amistad, tyrion, dragon)).

%leDijo(Persona1,Persona2,Serie,LoQuePaso).
leDijo(gaston, maiu, got, relacion(amistad, tyrion, dragon)).
leDijo(nico, maiu, starWars, relacion(parentesco, vader, luke)).
leDijo(nico, juan, got, muerte(tyrion)). 
leDijo(aye, juan, got, relacion(amistad, tyrion, john)).
leDijo(aye, maiu, got, relacion(amistad, tyrion, john)).
leDijo(aye, gaston, got, relacion(amistad, tyrion, dragon)).

%3 Punto B: Es spoiler
esSpoiler(Serie, Spoiler):- paso(Serie,_,_, Spoiler).
% a esSpoiler se le puede pregunatar cuales son las series a las cueles se las expoileo, cual es el espoiler de una serie.

%4 Punto C: Te pedí que no me lo dijeras
leSpoileo(PersonaA,PersonaB,Serie):- miraOquiereVer(PersonaB,Serie),leDijo(PersonaA, PersonaB, Serie, Spoiler), esSpoiler(Serie,Spoiler).

miraOquiereVer(Persona,Serie):-mira(Persona, Serie).
miraOquiereVer(Persona,Serie):-quiereVer(Persona, Serie).

% a leSpoileo le puedo preguntar quien espoileo, a quien se le espoileo, que serie fue espoileada, quien espoileo que serie y a quien le espoilearon que serie.

%5 Punto D: Responsable
televidenteResponsable(Persona):- televidente(Persona), not(leSpoileo(Persona,_,_)).
/*Para hacerlo inversible y consultar por "Quienes" son los televidentes responsable hay que unificar la variable
persona antes del "not", lo hice creando un hecho adicional llamado "televidente" que recibe como parámetro una persona.
Para no pasarle por ejemplo la función "mira" y que la variable "Persona" unifique con esa variable*/

%6 Punto E: Viene Zafando

vieneZafando(Persona,Serie):- miraOquiereVer(Persona,Serie), not(leSpoileo(_,Persona,Serie)),esPopular(Serie),paso(serie,_, _,_).

pasoAlgoFuerte(Serie):- paso(Serie,_,_,muerte(_)).
pasoAlgoFuerte(Serie):- paso(Serie,_,_,relacion(parentesco,_,_)).
pasoAlgoFuerte(Serie):- paso(Serie,_,_,relacion(amorosa,_,_)).

:- begin_tests(hombre).

test(laMuerteDelEmperador_StartWars_esSpoiler) :-
	esSpoiler(starWars,muerte(emperador).

:- end_tests(hombre).
