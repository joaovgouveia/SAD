:- module(diagnosis, [
    diagnosis_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

diagnosis_menu :-
    write("menu de diagnosticos\n").

filter_diseases(Symptons, Diseases, R) :-
    findall(Disease,
            (member(Disease, Diseases),
            get_dict(sintomas_associados, Disease, SymptonsList),
            intersection(Symptons, SymptonsList, R),
            length(R, N),
            N > 0), 
        FilteredDiseases),
        remove_duplicates(FilteredDiseases, R),
        print_all(R).

intersection([], _, []).
intersection(L, L, L).
intersection([H|T], L, [H|R]) :-
    member(H, L),
    intersection(T, L, R).
intersection([_|T], L, R) :-
    intersection(T, L, R).

calculate_probability(Disease, Symptons, Prob) :-
    print_bold("\ncalculating..."),
    get_dict(sintomas_associados, Disease, DiseaseSymptons),
    intersection(DiseaseSymptons, Symptons, Intersection),
    length(Intersection, IntersectionSize),
    length(DiseaseSymptons, SymptonsSize),
    write("\nis:"),
    print_bold(IntersectionSize),
    write("\nss:"),
    print_warning(SymptonsSize),
    X is (IntersectionSize / SymptonsSize) * 100,
    round(X, Prob),write("\nProb: "),print_highlighted(Prob),write("\n"),!.

most_probable([D], _, D):- get_dict(doenca, D, Name),write(Name). 
most_probable([D1,D2|T], Symptons, MostProbable) :-
    D1 \= D2,
    get_dict(doenca, D1, Name1),
    get_dict(doenca, D2, Name2),
    print_warning(Name1),write(" "),print_warning(Name2),
    calculate_probability(D1, Symptons, Prob1),
    calculate_probability(D2, Symptons, Prob2),
    print_bold("evaluating...\n"),
    (Prob1 >= Prob2 -> 
        write("1 -> \n"), most_probable([D1|T], Symptons, MostProbable);
        write("2 -> \n"), most_probable([D2|T], Symptons, MostProbable)).

diagnosis(Symptons) :- 
    read_json("../db/diseases.JSON", Diseases),
    filter_diseases(Symptons, Diseases, FilteredDiseases),
    most_probable(FilteredDiseases, Symptons, Disease),
    get_dict(doenca, Disease, Name),
    get_dict(possivel_causa, Disease, Cause),
    get_dict(medicamentos, Disease, Meds),
    calculate_probability(Disease, Symptons, Prob),
    string_concat("Diagnóstico: ", Name, Message),
    string_concat("Certeza: ", Prob, DataProb),
    string_concat("Possível Causa: ", Cause, DataCause),
    atomics_to_string(Meds, ' ', FormattedMeds),
    string_concat("Medicamentos indicados: ", FormattedMeds, DataMeds),
    print_success(Message),
    write("\n"),
    print_highlighted(DataProb),
    write("\n"),
    print_bold(DataCause),
    write("\n"),
    print_bold(DataMeds),
    write("\n"),
    !.

print_all([]):- write("\n").
print_all([D|T]):-
    get_dict(doenca, D, Name),
    print_success(Name),
    write(", "),
    print_all(T).
    
teste :-
    read_line_to_string(user_input, A),
    read_line_to_string(user_input, B),
    read_line_to_string(user_input, C),
    diagnosis([A, B, C]).