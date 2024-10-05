:- module(diagnosis, [
    diagnosis_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% DIAGNOSIS RUN
run_diagnosis("1") :- menu_diagnosis.
run_diagnosis("logout") :- exit_system.
run_diagnosis("back") :- start_menu.
run_diagnosis(_):- print_warning("Função não existe\n"), sleep(2), diagnosis_menu.

diagnosis_menu :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                       ╔╦╗╦╔═╗╔═╗╔╗╔╔═╗╔═╗╦╔═╗\n"),
    print_bold_highlighted_blue("                                        ║║║╠═╣║ ╦║║║║ ║╚═╗║╚═╗\n"),
    print_bold_highlighted_blue("                                       ═╩╝╩╩ ╩╚═╝╝╚╝╚═╝╚═╝╩╚═╝\n"), 
    print_bold(                 "                                                  (1)\n"),
    print_highlighted_yellow(   "                                           GERA DIAGNÓSTICO\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diagnosis(Option).


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

menu_diagnosis:-
    print_bold_highlighted_blue("NÚMERO DE SINTOMAS:\n "),
    read_line_to_string(user_input, N),
    atom_string(A, N),
    atom_number(A, Numero),
    size_symptoms(Size),
    ((Numero =< Size, Numero >= 1) -> generate_symptoms_list(Numero, Sintomas),
                                      diagnosis(Sintomas),
                                      write("\nPressione [enter] para voltar para o menu "),
                                      read_line_to_string(user_input, _),
                                      write("\nOpção:\n> "),
                                      read_line_to_string(user_input, Option),   
                                      run_diagnosis(Option)
    ;
    print_error("NÚMERO INDEVIDO DE SINTOMAS INFORMADOS\n"), sleep(2), diagnosis_menu).

generate_symptoms_list(0, []).
generate_symptoms_list(N, [S|Rest]) :-
    print_bold_highlighted_blue("SINTOMA: "),
    read_line_to_string(user_input, S),
    NewN is N - 1,
    generate_symptoms_list(NewN, Rest).

size_symptoms(Size) :-
    read_json("../db/symptoms.JSON", DadosSintomas),
    length(DadosSintomas, Size).

% Função de diagnóstico
diagnosis(Symptoms) :- 
    read_json("../db/diseases.JSON", Diseases),
    filter_diseases(Symptoms, Diseases, FilteredDiseases),
    length(FilteredDiseases, L),
    (L > 0 -> true; print_warning("Não foi possível diagnosticar.\n")),
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
    print_bold_highlighted_blue(DataProb),
    print_bold_highlighted_blue("%\n"),
    print_bold(DataCause),
    write("\n"),
    print_bold(DataMeds),
    write("\n"),!.
