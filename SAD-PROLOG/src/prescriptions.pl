:- module(prescriptions, [
    prescriptions_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% PRESCRIPTON RUN
run_prescription("1") :- menu_enumerate_symptoms.
run_prescription("2") :- menu_generate_prescription.
run_prescription("logout") :- exit_system.
run_prescription("back") :- start_menu.
run_prescription(_):- print_warning("Função não existe\n"), sleep(2), prescriptions_menu.

prescriptions_menu :-
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
    run_prescription(Option).



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

menu_enumerate_symptoms:-
    enumerate_symptoms,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_prescription(Option).


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


menu_generate_prescription:-
    print_bold_highlighted_blue("SINTOMA(S): "),
    read_line_to_string(user_input, Sintoma),
    generate_prescription(Sintoma), sleep(2),
    prescriptions_menu.

% Função principal que gera a receita MÉDICA
generate_prescription(Symptoms) :-
    (valid_symptoms(Symptoms) -> true ; print_error("SINTOMA(S) INFORMADO(S) NÃO CADASTRADO(S)")),
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Symptom, (member(Symptom, DadosReceitas), get_dict(sintoma, Symptom, SymptomN), member(SymptomN, Symptoms)), SymptomsDoc),
    generate_string(SymptomsDoc),
    !.