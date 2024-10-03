:- module(symptons, [
    symptons_menu/0
]).

symptons_menu :-
    write("menu de sintomas\n").

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

% Cria um sintoma
create_symptom(Sintoma, Sistemas):-
    read_json("../db/symptons.JSON", Symptons),
        NewSymptom = _{  % Corrige para NewSymptom
            sintoma: Sintoma,
            sistemas: Sistemas
        },
        append(Symptons, [NewSymptom], SymptonsNew),  % Mantém a consistência com NewSymptom
        write_json("../db/symptons.JSON", SymptonsNew),
        print_success("SINTOMA CADASTRADO COM SUCESSO!\n"),!.

% Atualiza um sintoma
update_symptom(Sintoma, SistemasNovos):-
    read_json("../db/symptons.JSON", Symptoms),
    % Verifica se o sintoma existe na lista
    select(Symptom, Symptoms, Rest),
    get_dict(sintoma, Symptom, Sintoma),
    % Atualiza os sistemas relacionados ao sintoma
    NewSymptom = _{  % Corrige para NewSymptom
        sintoma: Sintoma,
        sistemas: SistemasNovos
    },
    % Adiciona o sintoma atualizado à lista e salva no arquivo
    append(Rest, [NewSymptom], NewData),  % Mantém a consistência com NewSymptom
    write_json("../db/symptons.JSON", NewData),
    print_success("SINTOMA ATUALIZADO COM SUCESSO!\n"),
    !.

% Encontra um sintoma
read_symptom(Sintoma):-
    read_json("../db/symptons.JSON", Symptoms),
    % Verifica se o sintoma existe na lista
    select(Symptom, Symptoms, _),
    get_dict(sintoma, Symptom, Sintoma),
    % Obtém os detalhes do sintoma
    get_dict(sistemas, Symptom, Sistemas),
    % Formata e exibe os detalhes do sintoma
    string_concat("Sintoma: ", Sintoma, Header),
    string_concat("\nSistemas Relacionados: ", Sistemas, Text),
    print_success(Header),
    print_bold(Text),
    write("\n"),
    !.

% Deleta um sintoma
delete_symptom(Sintoma):-
    read_json("../db/symptons.JSON", Symptoms),
    % Verifica se o sintoma existe na lista
    select(Symptom, Symptoms, Rest),
    get_dict(sintoma, Symptom, Sintoma),
    % Remove o sintoma da lista e salva a nova lista
    write_json("../db/symptons.JSON", Rest),
    print_success("SINTOMA DELETADO COM SUCESSO!\n"),
    !.
