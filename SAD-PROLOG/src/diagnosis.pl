:- module(diagnosis, [
    diagnosis_menu/0
]).

:- use_module("../utils/utils").

diagnosis_menu :-
    write("menu de diagnosticos\n").

filter_diseases(Symptons, Diseases, FilteredDiseases) :-
    findall(Disease,
            (member(Disease, Diseases),
            get_dict(sintomas_associados, Disease, SymptonsList),
            intersection(Symptons, SymptonsList, R),
            length(R, N),
            N > 0), 
        FilteredDiseases).

intersection([], _, []).
intersection(L, L, L).
intersection([H|T], L, [H|R]) :-
    member(H, L),
    intersection(T, L, R).
intersection([_|T], L, R) :-
    intersection(T, L, R).

calculate_probability(Disease, Symptons, Prob) :-
    get_dict(sintomas_associados, Disease, DiseaseSymptons),
    intersection(DiseaseSymptons, Symptons, Intersection),
    length(Intersection, IntersectionSize),
    length(DiseaseSymptons, SymptonsSize),
    X is (SymptonsSize / IntersectionSize) * 100,
    round(X, Prob).

most_probable([H|[]], _, H).
most_probable([H1,H2|T], Symptons, MostProbable) :-
    calculate_probability(H1, Symptons, Prob1),
    calculate_probability(H2, Symptons, Prob2),
    (Prob1 >= Prob2 -> 
        most_probable([H1|T], MostProbable),
        most_probable([H2|T], MostProbable)).

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
    string_concat("Medicamentos indicados: ", Meds, DataMeds),
    print_success(Message),
    write(DataProb),
    write(DataCause),
    write(DataMeds).
    
teste :-
    read_line_to_string(user_input, A),
    read_line_to_string(user_input, B),
    read_line_to_string(user_input, C),
    diagnosis([A, B, C]).