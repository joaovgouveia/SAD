:- module(medications, [
    medications_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% MEDICATION RUN ADM
run_medication_adm("1") :- menu_create_medication.
run_medication_adm("2") :- menu_update_medication.
run_medication_adm("3") :- menu_view_medication_bula_adm.
run_medication_adm("4") :- menu_list_medications_adm.
run_medication_adm("5") :- menu_delete_medication.
run_medication_adm("logout") :- exit_system.
run_medication_adm("back") :- start_menu_adm.
run_medication_adm(_):- print_warning("Função não existe\n"), sleep(2), medications_menu_adm.


% MEDICATION RUN MED
run_medication_med("1") :- menu_view_medication_bula_med.
run_medication_med("2") :- menu_list_medications_med.
run_medication_med("logout") :- exit_system.
run_medication_med("back") :- start_menu_med.
run_medication_med(_):- print_warning("Função não existe\n"), sleep(2), medications_menu_med.


% MEDICATION RUN SEC
run_medication_sec("1") :- menu_view_medication_bula_sec.
run_medication_sec("2") :- menu_list_medications_sec.
run_medication_sec("logout") :- exit_system.
run_medication_sec("back") :- start_menu_sec.
run_medication_sec(_):- print_warning("Função não existe\n"), sleep(2), medications_menu_sec.


% =====================================================================================================================================================================
medications_menu_adm :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  

    print_bold_highlighted_blue("                                     ╔╦╗╔═╗╔╦╗╦╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔═╗\n"),
    print_bold_highlighted_blue("                                     ║║║║╣  ║║║║  ╠═╣ ║ ║║ ║║║║╚═╗\n"),
    print_bold_highlighted_blue("                                     ╩ ╩╚═╝═╩╝╩╚═╝╩ ╩ ╩ ╩╚═╝╝╚╝╚═╝\n"), 
    print_bold(                 "        (1)                    (2)                   (3)                   (4)                          (5)\n"),
    print_highlighted_yellow(   "ADICIONAR MEDICAÇÃO       ATUALIZAR BULA           VER BULA         LISTA MEDICAMENTO           REMOVE MEDICAMENTO\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_adm(Option).

% =====================================================================================================================================================================
medications_menu_med :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  

    print_bold_highlighted_blue("                                     ╔╦╗╔═╗╔╦╗╦╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔═╗\n"),
    print_bold_highlighted_blue("                                     ║║║║╣  ║║║║  ╠═╣ ║ ║║ ║║║║╚═╗\n"),
    print_bold_highlighted_blue("                                     ╩ ╩╚═╝═╩╝╩╚═╝╩ ╩ ╩ ╩╚═╝╝╚╝╚═╝\n"), 
    print_bold(                 "                                       (3)                   (4)       \n"),
    print_highlighted_yellow(   "                                     VER BULA         LISTA MEDICAMENTO\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_med(Option).

% =====================================================================================================================================================================
medications_menu_sec :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  

    print_bold_highlighted_blue("                                     ╔╦╗╔═╗╔╦╗╦╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔═╗\n"),
    print_bold_highlighted_blue("                                     ║║║║╣  ║║║║  ╠═╣ ║ ║║ ║║║║╚═╗\n"),
    print_bold_highlighted_blue("                                     ╩ ╩╚═╝═╩╝╩╚═╝╩ ╩ ╩ ╩╚═╝╝╚╝╚═╝\n"), 
    print_bold(                 "                                       (3)                   (4)       \n"),
    print_highlighted_yellow(   "                                     VER BULA         LISTA MEDICAMENTO\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_sec(Option).



menu_create_medication:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("BULA:\n "),
    read_line_to_string(user_input, Bula),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    create_medication(NomeMed, Bula, Dosagem), sleep(2),
    medications_menu.


% Funcao principal de criacao de medicamento
create_medication(Nome, Bula, Dosagem):-
    read_json("../db/medications.JSON", Medications),
    NewMedication = _{
        nome: Nome,
        bula: Bula,
        dosagem: Dosagem
    },
    append(Medications, [NewMedication], MedicationsNew),
    write_json("../db/medications.JSON", MedicationsNew),
    print_success("MEDICAÇÃO CADASTRADA COM SUCESSO!\n"),!.

eh_medicamento(Nome, Dosagem, Medications) :-
    member(Medication, Medications),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem).


menu_update_medication:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    print_bold_highlighted_blue("NOVA BULA:\n "),
    read_line_to_string(user_input, NewBula),
    update_medication(NomeMed, Dosagem, NewBula), sleep(2),
    medications_menu.


% Funcao principal de atualizacao da bula de um medicamento
update_medication(Nome, Dosagem, NewBula) :-
    read_json("../db/medications.JSON", Medications),
    (eh_medicamento(Nome, Dosagem, Medications) -> true ; print_error("MEDICAÇÃO NÃO CADASTRADA NO SISTEMA\n")),
    select(Medication, Medications, Rest),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    put_dict(bula, Medication, NewBula, MedicationAtt),
    append(Rest, [MedicationAtt], MedicationsAtt),
    write_json("../db/medications.JSON", MedicationsAtt),
    print_success("BULA DA MEDICAÇÃO ATUALIZADA COM SUCESSO!\n"),
    !.


format_medications([], Formatted, Formatted).
format_medications([Medication|Rest], FormattedTemp, FormattedResult) :-
    format_medication(Medication, FormattedMedication),
    string_concat(FormattedTemp, FormattedMedication, NewFormattedMedication),
    format_medications(Rest, NewFormattedMedication, FormattedResult).

format_medication(Medication, FormattedMedication) :-
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    get_dict(bula, Medication, Bula),
    format(string(FormattedMedication), "\nMedicamento: ~w\nDosagem: ~w\nBula: ~w\n",
           [Nome, Dosagem, Bula]).



menu_view_medication_bula_adm:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    view_medication_bula(NomeMed, Dosagem),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_adm(Option).

menu_view_medication_bula_med:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    view_medication_bula(NomeMed, Dosagem),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_med(Option).

menu_view_medication_bula_sec:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    view_medication_bula(NomeMed, Dosagem),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_sec(Option).

% Funcao principal de visualizacao da bula do medicamento
view_medication_bula(Nome, Dosagem) :-
    read_json("../db/medications.JSON", Medications),
    (eh_medicamento(Nome, Dosagem, Medications) -> true ; print_error("MEDICAÇÃO NÃO CADASTRADA NO SISTEMA\n")),
    member(Medication, Medications),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    format_medication(Medication, FormattedMedication),
    print_bold(FormattedMedication),
    write("\n"),
    !.

menu_list_medications_adm:-
    list_medications,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_adm(Option).

menu_list_medications_med:-
    list_medications,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_med(Option).

menu_list_medications_sec:-
    list_medications,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication_sec(Option).

% Funcao principal de listagem de medicamentos
list_medications :-
    read_json("../db/medications.JSON", Medications),
    format_medications(Medications, "", Result),
    print_bold(Result),
    !.

menu_delete_medication:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    delete_medication(NomeMed, Dosagem), sleep(2),
    medications_menu.


% Funcao principal de remocao de medicamentos
delete_medication(Nome, Dosagem) :-
    read_json("../db/medications.JSON", Medications),
    (eh_medicamento(Nome, Dosagem, Medications) -> true ; print_error("MEDICAÇÃO NÃO CADASTRADA NO SISTEMA\n")),
    select(Medication, Medications, Rest),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    print_success("MEDICAÇÃO DELETADA COM SUCESSO!\n"),
    write_json("../db/medications.JSON", Rest),
    !.
