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
        remove_duplicates(FilteredDiseases, R).

calculate_probability(Disease, Symptons, Prob) :-
    get_dict(sintomas_associados, Disease, DiseaseSymptons),
    intersection(DiseaseSymptons, Symptons, Intersection),
    length(Intersection, IntersectionSize),
    length(DiseaseSymptons, SymptonsSize),
    X is (IntersectionSize / SymptonsSize) * 100,
    round(X, Prob),!.

most_probable([D], _, D). 
most_probable([D1,D2|T], Symptons, MostProbable) :-
    D1 \= D2,
    calculate_probability(D1, Symptons, Prob1),
    calculate_probability(D2, Symptons, Prob2),
    (Prob1 >= Prob2 -> 
        most_probable([D1|T], Symptons, MostProbable);
        most_probable([D2|T], Symptons, MostProbable)).

diagnosis(Symptons) :- 
    read_json("../db/diseases.JSON", Diseases),
    filter_diseases(Symptons, Diseases, FilteredDiseases),
    length(FilteredDiseases, L),
    (L > 0 -> true; print_warning("Nao foi possivel diagnosticar.\n")),
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
    write("\n"),!.
    
teste :-
    read_line_to_string(user_input, A),
    read_line_to_string(user_input, B),
    read_line_to_string(user_input, C),
    diagnosis([A, B, C]).