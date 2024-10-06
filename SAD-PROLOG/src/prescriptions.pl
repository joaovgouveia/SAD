:- module(prescriptions, [
    prescriptions_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% PRESCRIPTON RUN ADM
run_prescription_adm("1") :- menu_enumerate_symptoms_adm.
run_prescription_adm("2") :- menu_generate_prescription_adm.
run_prescription_adm("logout") :- exit_system.
run_prescription_adm("back") :- start_menu_adm.
run_prescription_adm(_):- print_warning("Função não existe\n"), sleep(2), prescriptions_menu.

% PRESCRIPTON RUN MED
run_prescription_med("1") :- menu_enumerate_symptoms_med.
run_prescription_med("2") :- menu_generate_prescription_med.
run_prescription_med("logout") :- exit_system.
run_prescription_med("back") :- start_menu_med.
run_prescription_med(_):- print_warning("Função não existe\n"), sleep(2), prescriptions_menu.

% PRESCRIPTON RUN SEC
run_prescription_sec("1") :- menu_enumerate_symptoms_sec.
run_prescription_sec("2") :- menu_generate_prescription_sec.
run_prescription_sec("logout") :- exit_system.
run_prescription_sec("back") :- start_menu_sec.
run_prescription_sec(_):- print_warning("Função não existe\n"), sleep(2), prescriptions_menu.

prescriptions_menu_adm :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                   ╔═╗╦═╗╔═╗╔═╗╔═╗╦═╗╦╔═╗╔╦╗╦╔═╗╔╗╔\n"),
    print_bold_highlighted_blue("                                   ╠═╝╠╦╝║╣ ╚═╗║  ╠╦╝║╠═╝ ║ ║║ ║║║║\n"),
    print_bold_highlighted_blue("                                   ╩  ╩╚═╚═╝╚═╝╚═╝╩╚═╩╩   ╩ ╩╚═╝╝╚╝\n"), 
    print_bold(                 "                                  (1)                            (2)\n"),
    print_highlighted_yellow(   "                      LISTAGEM SINTOMAS PRESCRIÇÃO      GERA RECEITA MÉDICA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_adm(Option).

prescriptions_menu_med :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                   ╔═╗╦═╗╔═╗╔═╗╔═╗╦═╗╦╔═╗╔╦╗╦╔═╗╔╗╔\n"),
    print_bold_highlighted_blue("                                   ╠═╝╠╦╝║╣ ╚═╗║  ╠╦╝║╠═╝ ║ ║║ ║║║║\n"),
    print_bold_highlighted_blue("                                   ╩  ╩╚═╚═╝╚═╝╚═╝╩╚═╩╩   ╩ ╩╚═╝╝╚╝\n"), 
    print_bold(                 "                                  (1)                            (2)\n"),
    print_highlighted_yellow(   "                      LISTAGEM SINTOMAS PRESCRIÇÃO      GERA RECEITA MÉDICA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_med(Option).

prescriptions_menu_sec :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                   ╔═╗╦═╗╔═╗╔═╗╔═╗╦═╗╦╔═╗╔╦╗╦╔═╗╔╗╔\n"),
    print_bold_highlighted_blue("                                   ╠═╝╠╦╝║╣ ╚═╗║  ╠╦╝║╠═╝ ║ ║║ ║║║║\n"),
    print_bold_highlighted_blue("                                   ╩  ╩╚═╚═╝╚═╝╚═╝╩╚═╩╩   ╩ ╩╚═╝╝╚╝\n"), 
    print_bold(                 "                                  (1)                            (2)\n"),
    print_highlighted_yellow(   "                      LISTAGEM SINTOMAS PRESCRIÇÃO      GERA RECEITA MÉDICA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_sec(Option).



% Função auxiliar que gera a listagem de sintomas
list_symptoms([], _, []).
list_symptoms([X|Rest], N, [XF|Resp]) :-
    atom_number(Atom,N),
    atom_string(Atom, S),
    string_concat(S, ". ", Temp),
    string_concat(Temp, X, Temp1),
    string_concat(Temp1, "\n", XF),
    NN is N + 1,
    list_symptoms(Rest, NN, Resp).

menu_enumerate_symptoms_adm:-
    enumerate_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_adm(Option).

menu_enumerate_symptoms_med:-
    enumerate_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_med(Option).

menu_enumerate_symptoms_sec:-
    enumerate_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription_sec(Option).


% Função principal que lista os sintomas
enumerate_symptoms :-
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Symptom, (member(Doc, DadosReceitas), get_dict(sintoma, Doc, Symptom)), Symptoms),
    list_symptoms(Symptoms, 1, FormattedSymptoms),
    atomics_to_string(FormattedSymptoms, '', ListSymptoms),
    print_bold(ListSymptoms),
    !.

% Função auxiliar que verifica os sintomas passados
valid_symptoms(Symptoms) :-
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Symptom, (member(Doc, DadosReceitas), get_dict(sintoma, Doc, Symptom), member(Symptom, Symptoms)), ListSymptoms),
    length(ListSymptoms, R),
    length(Symptoms, R1),
    R =:= R1.

% Função auxiliar que gera a receita MÉDICA
generate_string(Symptoms) :-
    format_symptoms(Symptoms, "", Result),
    print_highlighted("\nRECEITA MÉDICA:\n"),
    print_bold(Result),
    !.

% Função auxiliar que gera a receita MÉDICA
format_symptoms([], Acc, Acc).
format_symptoms([Symptom|Rest], Acc, Result) :-
    format_one_symptom(Symptom, FormattedSymptom),
    string_concat(Acc, FormattedSymptom, NewAcc),
    format_symptoms(Rest, NewAcc, Result).

% Função auxiliar que gera a receita MÉDICA
format_one_symptom(Symptom, Formatted) :-
    get_dict(medicamento_nome, Symptom, MedNome),
    get_dict(medicamento_dosagem, Symptom, MedDosagem),
    get_dict(medicamento_frequencia, Symptom, MedFreq),
    get_dict(medicamento_tipo, Symptom, MedTipo),
    get_dict(medicamento_observacoes, Symptom, MedObs),
    format(string(Formatted), "\nMedicamento: ~w\nDosagem: ~w\nFrequência: ~w\nTipo: ~w\nObservações: ~w\n",
           [MedNome, MedDosagem, MedFreq, MedTipo, MedObs]).


menu_generate_prescription_adm:-
    print_bold_highlighted_blue("SINTOMA(S): "),
    read_line_to_string(user_input, Sintoma),
    generate_prescription(Sintoma), sleep(2),
    prescriptions_menu_adm.

menu_generate_prescription_med:-
    print_bold_highlighted_blue("SINTOMA(S): "),
    read_line_to_string(user_input, Sintoma),
    generate_prescription(Sintoma), sleep(2),
    prescriptions_menu_med.

menu_generate_prescription_sec:-
    print_bold_highlighted_blue("SINTOMA(S): "),
    read_line_to_string(user_input, Sintoma),
    generate_prescription(Sintoma), sleep(2),
    prescriptions_menu_sec.

% Função principal que gera a receita MÉDICA
generate_prescription(Symptoms) :-
    (valid_symptoms(Symptoms) -> true ; print_error("SINTOMA(S) INFORMADO(S) NÃO CADASTRADO(S)")),
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Symptom, (member(Symptom, DadosReceitas), get_dict(sintoma, Symptom, SymptomN), member(SymptomN, Symptoms)), SymptomsDoc),
    generate_string(SymptomsDoc),
    !.