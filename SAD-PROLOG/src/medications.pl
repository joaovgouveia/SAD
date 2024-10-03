:- module(medications, [
    medications_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

medications_menu :-
    write("menu de medicamentos\n").

% Cria um medicamento
create_medication(Nome, Bula, Dosagem):-
    read_json("../db/medications.JSON", Medications),
    NewMedication = _{
        nome: Nome,
        bula: Bula,
        dosagem: Dosagem
    },
    append(Medications, [NewMedication], MedicationsNew),
    write_json("../db/medications.JSON", MedicationsNew),
    print_success("MEDICACAO CADASTRADA COM SUCESSO!\n"),!.

% Atualiza a bula de uma medicação pelo nome e dosagem
update_medication(Nome, Dosagem, NovaBula) :-
    read_json("../db/medications.JSON", Medications),
    (   select(Medication, Medications, Rest),  % Seleciona a medicação que bate com o nome e dosagem
        get_dict(nome, Medication, Nome),
        get_dict(dosagem, Medication, Dosagem)
    ->  % Atualiza a bula
        put_dict(bula, Medication, NovaBula, UpdatedMedication),
        append(Rest, [UpdatedMedication], UpdatedMedications),  % Atualiza a lista
        write_json("../db/medications.JSON", UpdatedMedications),
        print_success("BULA DA MEDICACAO ATUALIZADA COM SUCESSO!\n")
    ;   % Caso não encontre a medicação
        print_error("MEDICACAO NAO ENCONTRADA!\n")
    ),
    !.

% Encontra um medicamento
read_medication(Nome, Dosagem) :-
    read_json("../db/medications.JSON", Medications),
    % Encontra a medicação pelo nome e dosagem
    select(Medication, Medications, _),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    get_dict(bula, Medication, Bula),
    % Formata a saída
    string_concat("Nome: ", Nome, Header),
    string_concat("\nDosagem: ", Dosagem, Temp),
    string_concat(Temp, "\nBula: ", Text),
    % Exibe as informações
    print_success(Header),
    print_bold(Text),
    print_bold(Bula),
    write("\n"),
    !.

% Deleta um medicamento
delete_medication(Nome, Dosagem) :-
    read_json("../db/medications.JSON", Medications),
    % Encontra e remove o medicamento pelo nome e dosagem
    select(Medication, Medications, Rest),
    get_dict(nome, Medication, Nome),
    get_dict(dosagem, Medication, Dosagem),
    % Exibe mensagem de sucesso e salva a lista atualizada
    print_success("MEDICACAO DELETADA COM SUCESSO!\n"),
    write_json("../db/medications.JSON", Rest),
    !.
