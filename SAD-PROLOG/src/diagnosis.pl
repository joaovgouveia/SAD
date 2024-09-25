:- module(diagnosis, [
    diagnosis_menu/0
]).

:- use_module("../utils/utils").

diagnosis_menu :-
    write("menu de diagnosticos\n").

filter([], _, []).
filter([H|T], DadosDoencas, [Doenca|Doencas]) :-
    member(Doc, DadosDoencas),
    get_dict(doenca, Doc, Doenca),
    get_dict(sintomas_associados, Doc, SintomasLista),
    member(H, SintomasLista),
    filter(T, DadosDoencas, Doencas).
filter([_|T], DadosDoencas, Doencas) :-
    filter(T, DadosDoencas, Doencas). 

diagnosis(Symptons) :- 
    read_json("../db/diseases.JSON", DadosDoencas),
    filter(Symptons, DadosDoencas, Doencas).