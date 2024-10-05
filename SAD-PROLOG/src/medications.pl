:- module(medications, [
    medications_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% MEDICATION RUN
run_medication("1") :- menu_create_medication.
run_medication("2") :- menu_update_medication.
run_medication("3") :- menu_view_medication_bula.
run_medication("4") :- menu_list_medications.
run_medication("5") :- menu_delete_medication.
run_medication("logout") :- exit_system.
run_medication("back") :- start_menu.
run_medication(_):- print_warning("Função não existe\n"), sleep(2), medications_menu.


medications_menu :-
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
    run_medication(Option).


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



menu_view_medication_bula:-
    print_bold_highlighted_blue("NOME MEDICAMENTO:\n "),
    read_line_to_string(user_input, NomeMed),
    print_bold_highlighted_blue("DOSAGEM:\n "),
    read_line_to_string(user_input, Dosagem),
    view_medication_bula(NomeMed, Dosagem),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication(Option).

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

menu_list_medications:-
    list_medications,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_medication(Option).

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
    view_medication_bula(NomeMed, Dosagem), sleep(2),
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
