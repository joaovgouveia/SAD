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

intersection([], _, []).
intersection(L, L, L).
intersection([H|T], L, [H|R]) :-
    member(H, L),
    intersection(T, L, R).
intersection([_|T], L, R) :-
    intersection(T, L, R).

diagnosis(Symptons, Doencas) :- 
    read_json("../db/diseases.JSON", DadosDoencas),
    filter(Symptons, DadosDoencas, Doencas),
    write(Doencas).

teste :-
    read_line_to_string(user_input, A),
    read_line_to_string(user_input, B),
    read_line_to_string(user_input, C),
    diagnosis([A, B, C], Doenca).
