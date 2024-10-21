/**********************************************************/
/* */
/* Departamento de Contaduría y Administración */
/* Inteligencia Artificial */
/* Universidad Autónoma de Chiapas */
/* */
/* LOGICA DE PRIMER ORDEN */
/* Prolog */
/* */
/* A210525_Mónica Isabel López Flores.*/
/* */
/* S.O. : MS-DOS (Windows) */
/* Interprete : SWI-Prolog */
/* Fichero : EJ09.PL */
/* */
/**********************************************************/
/* ÁRBOL GENEALÓGICO */
/*---- PROGRAMA PRINCIPAL ----*/
/*------ Hechos ------*/
/* padres(H,P,M,A) <- H tiene como padre a P y como madre
 a M, y nació el año A */

padres('Don Dionisio',p1,m1,1923).
padres('Doña Blanca',p2,m2,1924).

padres('Don Juan',p3,m3,1922).
padres('Doña Ivonne',p4,m4,1922).

padres('Don Ramiro',p5,m5,1922).
padres('Doña Cristina',p6,m6,1927).

padres('Don Francisco',p7,m7,1921).
padres('Doña Carmen',p8,m8,1930).

padres('Antonio','Don Dionisio','Doña Blanca',1952).
padres('Romelia','Don Juan','Doña Ivonne',1950).

padres('Manuel','Don Ramiro','Doña Cristina',1942).
padres('Lubia','Don Francisco','Doña Carmen',1945).

padres('Liliana','Antonio','Romelia',1967).
padres('Margarita','Antonio','Romelia',1974).
padres('Romeo','Antonio','Romelia',1972).
padres('Sergio','Manuel','Lubia',1969). 
padres('Horacio','Manuel','Lubia',1970). 
padres('Charito','Manuel','Lubia',1971). 
padres('Patricia','Manuel','Lubia',1974).
padres('Veronica','Manuel','Rosa',1972).
padres('Ana','Romeo','Veronica',1998). 
padres('Cristi','Romeo','Veronica',2000).  
padres('Monica','Romeo','Veronica',2003). 

/* casados(H,M) <- El hombre H está casado con la mujer M */
casados('Ramiro','Cristina').
casados('Francisco','Carmen').
casados('Alejandro','Blanca').
casados('Juan','Ivonne').
casados('Manuel','Lubia').
casados('Antonio','Romelia').
casados('Romeo','Veronica').

/* hombre(P) <- la persona P es del género masculino */
hombre('Ramiro').
hombre('Francisco').
hombre('Dionisio').
hombre('Juan').
hombre('Manuel').
hombre('Antonio').
hombre('Romeo').
hombre('Sergio').
hombre('Horacio').


/* mujer(P) <- la persona P es del género femenino */
mujer('Cristina').
mujer('Carmen').
mujer('Blanca').
mujer('Ivonne').
mujer('Lubia').
mujer('Romelia').
mujer('Liliana').
mujer('Margarita').
mujer('Charito').
mujer('Patricia').
mujer('Veronica').
mujer('Ana'). 
mujer('Cristi').
mujer('Monica').

/*------ Reglas ------*/

/* edad(P,E) <- la persona P tiene E años */
edad(P,E) :- padres(P,_,_,A), E is 1998-A.

/* mayor(P1,P2) <- la persona P1 es mayor que P2 */
mayor(P1,P2) :- padres(P1,_,_,A1), padres(P2,_,_,A2), A1 < A2.
mayor(P1,P2) :- padres(P1,_,_,A1), padres(P2,_,_,A2), A1 = A2, P1 \= P2.

/* niño(P1) <- P1 es un niño (menos de 14 años) */
ninyo(P) :- edad(P,E), E=<14.

/* joven(P1) <- P1 es una persona joven (entre 14 y 25 años) */
joven(P) :- edad(P,E), 14<E,E=<25. 

 /* adulto(P1) <- P1 es un adulto (entre 25 y 50 años) */
adulto(P) :- edad(P,E), 25<E,E=<50.

/* viejo(P1) <- P1 es una persona vieja (más de 50 años) */
viejo(P) :- edad(P,E), E>50.

/* hermanos(H1,H2) <- H1 es hermano/a de H2 */
hermanos(H1,H2) :- padres(H1,P,M,_), padres(H2,P,M,_), H1\=H2.

/* tio(T,S) <- T es el tio de S */
tio(T,S) :- hombre(T), padres(S,P,_,_), hermanos(T,P).

tio(T,S) :- hombre(T), padres(S,_,M,_), hermanos(T,M).
tio(T,S) :- hombre(T), padres(S,P,_,_), hermanos(T1,P), casados(T,T1).
tio(T,S) :- hombre(T), padres(S,_,M,_), hermanos(T1,M), casados(T,T1).

/* tia(T,S) <- T es la tia de S */
tia(T,S) :- mujer(T), padres(S,P,_,_), hermanos(T,P).
tia(T,S) :- mujer(T), padres(S,_,M,_), hermanos(T,M).
tia(T,S) :- mujer(T), padres(S,P,_,_), hermanos(T1,P), casados(T1,T).
tia(T,S) :- mujer(T), padres(S,_,M,_), hermanos(T1,M), casados(T1,T).

/* primos(P1,P2) <- P1 es primo/a de P2 */
primos(P1,P2) :- padres(P1,PA1,MA1,_),
 padres(P2,PA2,MA2,_),
 (hermanos(PA1,PA2);
 hermanos(PA1,MA2);
 hermanos(MA1,PA2);
 hermanos(MA1,MA2)).

/* abuelo(A,N) <- A es el abuelo de N */
abuelo(A,N) :- padres(N,P,M,_), (padres(P,A,_,_);
 padres(M,A,_,_)).

/* abuela(A,N) <- A es la abuela de N */
abuela(A,N) :- padres(N,P,M,_), (padres(P,_,A,_);
 padres(M,_,A,_)).

/* antepasado(A,P) <- A es antepasado de P */
antepasado(A,P) :- padres(P,A,_,_).
antepasado(A,P) :- padres(P,_,A,_). 
antepasado(A,P) :- padres(P,PA,_,_), antepasado(A,PA).
antepasado(A,P) :- padres(P,_,MA,_), antepasado(A,MA).
