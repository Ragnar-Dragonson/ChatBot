/*Vocabulary*/
adj(old).
adj(good).
adj(long).
adj(sprightly).
adj(social).
adj(young).
adj(racing).
adj(avid).
adj(teenage).
det(an).
det(a).
noun(boy).
noun(father).
noun(grandfather).
noun(person).
noun(student).
noun(petrolhead).
noun(book).
noun(car).
noun(horses).
noun(walk).
noun(chat).
noun(guitar).
verb(loves).
verb(likes).
prep(a).


/*Input Verb, Noun recommendation based on input verb*/

recommend(likes,book,'He joins the book club').
recommend(loves,horses,'They join a riding club').
recommend(loves,walk,'He joins a rambling club').
recommend(likes,chat,'They join a social club').
recommend(likes,guitar,'They should join a band').
recommend(loves,car,'They should go to the races').

/*Agent*/

agent:-
      perceive(Percepts),
      action(Percepts).

/*The agent perceives its enviroment using read(X)*/

perceive(Percepts):-
      nl,
      write('Who should be recommended a present? '),
      read(Percepts).

go(List,Output):-
      sentence(List,Output),
      prty(Output).

/*Pretty Printed Parse Tree*/

prty(A):-
      A=sentence(X,Y),
      nl,
      write('Sentence'),
      nl,

      tab(10),
      write('Noun phrase'),
      nl,
      prty(X),
      tab(10),
      write('Verb phrase'),
      nl,
      prty(Y).

prty(np(A)):-
       A=np(X,Y),
       nl,
       tab(20),
       write(X),
       nl,
       prty(Y).

prty(vp(A)):-
    A=vp(X,Y),
    tab(20),
    write(X),
    nl,
    prty(Y).


prty(A):-
      A=np2(X,Y),
      tab(30),
      write(X),
      nl,
      prty(Y).

prty(A):-
       A=pp(X,Y),
       tab(30),

       write(X),
       nl,
       prty(Y).

prty(A):-
       A=vp(X,Y),
       tab(40),
       write(X),
       nl,
       prty(Y).

prty(A):-
      A=np2(X),
      tab(40),
      write(X),
      nl.


prty(A):-
       A=np(X,Y),
       tab(30),
       write(X),
       nl,
       prty(Y).

/*
Sentence Parser
*/

sentence(Sentence,sentence(np(Noun_Phrase),vp(Verb_Phrase))):-
      np(Sentence,Noun_Phrase,Rem),
      vp(Rem,Verb_Phrase).

/*Noun phrase to Det Noun phrase 2 */

np([X|T],np(det(X),NP2),Rem):-
	det(X),
	np2(T,NP2,Rem).

/*Noun Phrase to Noun Phrase 2*/

np(Sentence,Parse,Rem):-
np2(Sentence,Parse,Rem).

/*Noun Phrase to Prep Phrase*/

np(Sentence,np(NP,PP),Rem):-
	np(Sentence,NP,Rem1),
	pp(Rem1,PP,Rem).

/*Noun Phrase 2 to Noun*/

np2([H|T],np2(noun(H)),T):-
      noun(H).

/*Noun Phrase 2 to Adjective and Noun Phrase 2*/

np2([H|T],np2(adj(H),Rest),Rem):-
      adj(H),
      np2(T,Rest,Rem).

/*Prep Phrase to Prep Noun Phrase*/

pp([H|T],pp(prep(H),Parse),Rem):-
      prep(H),
      np(T,Parse,Rem).


/*Verb Phrase to Verb */

vp([H|[]],verb(H)):-
          verb(H).

/*Verb Phrase to Verb and Prep Phrase*/

vp([H|Rest],vp(verb(H),Restparsed)):-
        verb(H),
        pp(Rest,Restparsed,_).

/*Verb Phrase to Verb and Noun Phrase*/

vp([H|Rest],vp(verb(H),Restparsed)):-
      verb(H),
      np(Rest,Restparsed,_).

/*
Recommendation action.

*/

action(Response):-
      go(Response,Sentence),
      keyword(Sentence,Out),
      write('Sentence'),
      nl,tab(7),write('|'),
      nl,tab(7),write('|-'),write(Response),
      nl,tab(7),write('|'),
      nl,tab(7),write('|-'),write('Produces the recommendation that'),
      nl,tab(7),write('|'),

      nl,tab(7),write('|-'),write(Out).

/*
Key Word is used to retrieve the sentence and split it into noun phrase
and verb phrase.
Key is the Verb_Phrase, Noun(N) and Verb(V).
Recommend is the Verb, Noun and output(O).
*/

keyword(sentence(np(_),vp(Verb_Phrase)), Output):-
      key(Verb_Phrase,_,_,Output).

/*
Key includes verb phrase(A),verb(V),noun(N) and output(O).
*/

key(A,V,N,O):-
      A= vp(X,Y),
      X = verb(V),
      key(Y,V,N,O).

key(A,V,N,O):-
      A = np2(_,Y),
      key(Y,V,N,O).

key(A,V,N,O):-
      A = pp(_,Y),
      key(Y,V,N,O).

key(A,V,N,O):-
      A = vp(_,Y),
      key(Y,V,N,O).

key(A,V,N,O):-
      A = np(_,Y),
      key(Y,V,N,O).

key(A,V,N,O) :-
      A = np2(noun(N)),
      recommend(V,N,O).

key(_,V,N,O):-
      recommend(V,N,O).



