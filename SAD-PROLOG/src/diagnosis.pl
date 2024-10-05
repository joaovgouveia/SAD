:- module(diagnosis, [
    diagnosis_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

diagnosis_menu :-
    write("menu de diagnosticos\n").

filter_diseases(Symptoms, Diseases, R) :-
    findall(Disease,
            (member(Disease, Diseases),
            get_dict(sintomas_associados, Disease, SymptomsList),
            intersection(Symptoms, SymptomsList, R),
            length(R, N),
            N > 0), 
        FilteredDiseases),
        remove_duplicates(FilteredDiseases, R).

calculate_probability(Disease, Symptoms, Prob) :-
    get_dict(sintomas_associados, Disease, DiseaseSymptoms),
    intersection(DiseaseSymptoms, Symptoms, Intersection),
    length(Intersection, IntersectionSize),
    length(DiseaseSymptoms, SymptomsSize),
    X is (IntersectionSize / SymptomsSize) * 100,
    round(X, Prob),!.

most_probable([D], _, D). 
most_probable([D1,D2|T], Symptoms, MostProbable) :-
    D1 \= D2,
    calculate_probability(D1, Symptoms, Prob1),
    calculate_probability(D2, Symptoms, Prob2),
    (Prob1 >= Prob2 -> 
        most_probable([D1|T], Symptoms, MostProbable);
        most_probable([D2|T], Symptoms, MostProbable)).

% Função de diagnóstico
diagnosis(Symptoms) :- 
    read_json("../db/diseases.JSON", Diseases),
    filter_diseases(Symptoms, Diseases, FilteredDiseases),
    length(FilteredDiseases, L),
    (L > 0 -> true; print_warning("Não foi possivel diagnosticar.\n")),
    most_probable(FilteredDiseases, Symptoms, Disease),
    get_dict(doenca, Disease, Name),
    get_dict(possivel_causa, Disease, Cause),
    get_dict(medicamentos, Disease, Meds),
    calculate_probability(Disease, Symptoms, Prob),
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
