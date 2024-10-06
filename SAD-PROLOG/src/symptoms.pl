:- module(symptoms, [
    symptoms_menu_adm/0,
    symptoms_menu_med/0,
    symptoms_menu_sec/0
]).


:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% SYMPTOMS RUN ADM
run_symptoms_adm("1") :- menu_view_symptom_adm.
run_symptoms_adm("2") :- menu_list_symptoms_adm.
run_symptoms_adm("3") :- menu_list_symptoms_per_system_adm.
run_symptoms_adm("logout") :- exit_system.
run_symptoms_adm("back") :- start_menu_adm.
run_symptoms_adm(_):- print_warning("Função não existe\n"), sleep(2), symptoms_menu_adm.

% SYMPTOMS RUN MED
run_symptoms_med("1") :- menu_list_symptoms_per_system_med.
run_symptoms_med("logout") :- exit_system.
run_symptoms_med("back") :- start_menu_med.
run_symptoms_med(_):- print_warning("Função não existe\n"), sleep(2), symptoms_menu_med.

% SYMPTOMS RUN SEC
run_symptoms_sec("1") :- menu_view_symptom_sec.
run_symptoms_sec("2") :- menu_list_symptoms_sec.
run_symptoms_sec("3") :- menu_list_symptoms_per_system_sec.
run_symptoms_sec("logout") :- exit_system.
run_symptoms_sec("back") :- start_menu_sec.
run_symptoms_sec(_):- print_warning("Função não existe\n"), sleep(2), symptoms_menu_sec.
% =============================================================================================================================================


symptoms_menu_adm :-
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
    run_symptoms_adm(Option).

% =============================================================================================================================================

symptoms_menu_med :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
    print_bold_highlighted_blue("                         ╔═╗╦ ╦╔╦╗╔═╗╔╦╗╔═╗╔╦╗╔═╗\n"),
    print_bold_highlighted_blue("                         ╚═╗╚╦╝║║║╠═╝ ║ ║ ║║║║╚═╗\n"),
    print_bold_highlighted_blue("                         ╚═╝ ╩ ╩ ╩╩   ╩ ╚═╝╩ ╩╚═╝\n"), 
    print_bold(                 "                                    (1)\n"),
    print_highlighted_yellow(   "                        LISTA SINTOMAS POR SISTEMA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_med(Option).

% =============================================================================================================================================


symptoms_menu_sec :-
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
    run_symptoms_sec(Option).

% =============================================================================================================================================

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

menu_view_symptom_adm:-
    print_bold_highlighted_blue("SINTOMA:\n "),
    read_line_to_string(user_input, Sintoma),
    view_symptom(Sintoma),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_adm(Option).

menu_view_symptom_sec:-
    print_bold_highlighted_blue("SINTOMA:\n "),
    read_line_to_string(user_input, Sintoma),
    view_symptom(Sintoma),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_sec(Option).

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


menu_list_symptoms_adm:-
    list_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_adm(Option).

menu_list_symptoms_sec:-
    list_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_sec(Option).

% Funcao principal de listagem dos sintomas
list_symptoms :-
    read_json("../db/symptoms.JSON", Symptoms),
    format_symptoms(Symptoms, "", Result),
    print_bold(Result),
    !.

menu_list_symptoms_per_system_adm:-
    print_bold_highlighted_blue("SISTEMA:\n "),
    read_line_to_string(user_input, Sistema),
    list_symptoms_per_system(Sistema),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_adm(Option).

menu_list_symptoms_per_system_med:-
    print_bold_highlighted_blue("SISTEMA:\n "),
    read_line_to_string(user_input, Sistema),
    list_symptoms_per_system(Sistema),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_med(Option).

menu_list_symptoms_per_system_sec:-
    print_bold_highlighted_blue("SISTEMA:\n "),
    read_line_to_string(user_input, Sistema),
    list_symptoms_per_system(Sistema),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_symptoms_sec(Option).

% Funcao principal de listagem dos sintomas por sistema
list_symptoms_per_system(System) :-
    read_json("../db/symptoms.JSON", Symptoms),
    get_symptoms_per_system(Symptoms, System, SymptomsSystem),
    length(SymptomsSystem, R),
    (R >= 1 -> true ; print_error("SISTEMA NÃO LISTADO OU SEM DOENÇAS DO SISTEMA ESCOLHIDO, SISTEMA EM LETRAS MAIÚSCULAS\n")),
    format_symptoms(SymptomsSystem, "", Result),
    print_bold(Result),
    !.