:- module(symptoms, [
    symptoms_menu/0
]).


:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% SYMPTOMS RUN
run_symptoms("1") :- menu_view_symptom.
run_symptoms("2") :- menu_list_symptoms.
run_symptoms("3") :- menu_list_symptoms_per_system.
run_symptoms("logout") :- exit_system.
run_symptoms("back") :- start_menu.
run_symptoms(_):- print_warning("Função não existe\n"), sleep(2), symptoms_menu.


symptoms_menu :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
    print_bold_highlighted_blue("                         ╔═╗╦ ╦╔╦╗╔═╗╔╦╗╔═╗╔╦╗╔═╗\n"),
    print_bold_highlighted_blue("                         ╚═╗╚╦╝║║║╠═╝ ║ ║ ║║║║╚═╗\n"),
    print_bold_highlighted_blue("                         ╚═╝ ╩ ╩ ╩╩   ╩ ╚═╝╩ ╩╚═╝\n"), 
    print_bold(                 "             (1)                  (2)                      (3)\n"),
    print_highlighted_yellow(   "         VER SINTOMA        LISTAR SINTOMAS     LISTA SINTOMAS POR SISTEMA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms(Option).


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


menu_view_symptom:-
    print_bold_highlighted_blue("SINTOMA:\n "),
    read_line_to_string(user_input, Sintoma),
    view_symptom(Sintoma),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms(Option).

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


menu_list_symptoms:-
    list_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms(Option).

% Funcao principal de listagem dos sintomas
list_symptoms :-
    read_json("../db/symptoms.JSON", Symptoms),
    format_symptoms(Symptoms, "", Result),
    print_bold(Result),
    !.

menu_list_symptoms_per_system:-
    print_bold_highlighted_blue("SISTEMA:\n "),
    read_line_to_string(user_input, Sistema),
    list_symptoms_per_system(Sistema),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms(Option).

% Funcao principal de listagem dos sintomas por sistema
list_symptoms_per_system(System) :-
    read_json("../db/symptoms.JSON", Symptoms),
    get_symptoms_per_system(Symptoms, System, SymptomsSystem),
    length(SymptomsSystem, R),
    (R >= 1 -> true ; print_error("SISTEMA NÃO LISTADO OU SEM DOENÇAS DO SISTEMA ESCOLHIDO, SISTEMA EM LETRAS MAIÚSCULAS\n")),
    format_symptoms(SymptomsSystem, "", Result),
    print_bold(Result),
    !.