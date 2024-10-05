:- module(symptoms, [
    symptoms_menu/0
]).

symptoms_menu :-
    write("menu de sintomas\n").

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

eh_sintoma(SymptomName, Symptoms) :-
    member(Symptom, Symptoms),
    get_dict(sintoma, Symptom, SymptomName).

get_symptoms_per_system(Symptoms, System, SymptomsArea) :-
    findall(Symptom, (member(Symptom, Symptoms), get_dict(sistemas, Symptom, Sistemas), member(System, Sistemas)), SymptomsArea).
    
format_symptoms([], Formatted, Formatted).
format_symptoms([Symptom|Rest], FormattedTemp, FormattedResult) :-
    format_symptom(Symptom, FormattedSymptom),
    string_concat(FormattedTemp, FormattedSymptom, NewFormattedSymptom),
    format_symptoms(Rest, NewFormattedSymptom, FormattedResult).

format_symptom(Symptom, FormattedSymptom) :-
    get_dict(sintoma, Symptom, Sintoma),
    get_dict(sistemas, Symptom, Sistemas),
    format(string(FormattedSymptom), "\nSintoma: ~w\nSistemas: ~w\n",
           [Sintoma, Sistemas]).

% Funcao principal de visualizacao do sintoma
view_symptom(Sintoma):-
    read_json("../db/symptoms.JSON", Symptoms),
    (eh_sintoma(Sintoma, Symptoms) -> true ; print_error("SINTOMA NÃO CADASTRADO NO SISTEMA\n")),
    member(Symptom, Symptoms),
    get_dict(sintoma, Symptom, Sintoma),
    format_symptom(Symptom, FormattedSymptom),
    print_bold(FormattedSymptom),
    write("\n"),
    !.

% Funcao principal de listagem dos sintomas
list_symptoms :-
    read_json("../db/symptoms.JSON", Symptoms),
    format_symptoms(Symptoms, "", Result),
    print_bold(Result),
    !.

% Funcao principal de listagem dos sintomas por sistema
list_symptoms_per_system(System) :-
    read_json("../db/symptoms.JSON", Symptoms),
    get_symptoms_per_system(Symptoms, System, SymptomsSystem),
    length(SymptomsSystem, R),
    (R >= 1 -> true ; print_error("SISTEMA NÃO LISTADO OU SEM DOENÇAS DO SISTEMA ESCOLHIDO, SISTEMA EM LETRAS MAIÚSCULAS\n")),
    format_symptoms(SymptomsSystem, "", Result),
    print_bold(Result),
    !.