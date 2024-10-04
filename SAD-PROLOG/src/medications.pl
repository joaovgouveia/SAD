:- module(medications, [
    medications_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

medications_menu :-
    write("menu de medicamentos\n").

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

% Funcao principal de visualizacao da bula do medicamento
view_medication_bula(Nome, Dosagem) :-
    read_json("../db/medications.JSON", Medications),
    (eh_medicamento(Nome, Dosagem) -> true ; print_error("MEDICAÇÃO NÃO CADASTRADA NO SISTEMA\n")),
    member(Medication, Medications),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    format_medication(Medication, FormattedMedication),
    print_bold(FormattedMedication),
    write("\n"),
    !.

% Funcao principal de listagem de medicamentos
list_medications :-
    read_json("../db/medications.JSON", Medications),
    format_medications(Medications, "", Result),
    print_bold(Result), 
    !.

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
