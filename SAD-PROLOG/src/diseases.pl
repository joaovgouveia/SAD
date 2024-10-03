:- module(diseases, [
    diseases_menu/0
]).

diseases_menu :-
    write("menu de doencas\n").


    doenca                    :: String,
    especialidade_relacionada  :: String,
    sintomas_associados        :: [String],
    possivel_causa             :: String,
    medicamentos              :: [String]


% Cria uma doenca
create_disease(Doenca, Especialidade, Sintomas, Causa, Medicamentos):-
    read_json("../db/diseases.JSON", Diseases),
        NewDisease = _{
            doenca: Doenca,
            especialidade_relacionada: Especialidade,
            sintomas_associados: Sintomas,
            possivel_causa: Causa,
            medicamentos: Medicamentos
        },
        append(Diseases, [NewDisease], DiseasesNew),
        write_json("../db/diseases.JSON", DiseasesNew),
        print_success("DOENCA CADASTRADA COM SUCESSO!\n"),!.

% Atualiza uma doença
update_disease(NomeDoenca, Especialidade, Sintomas, Causa, Medicamentos):-
    read_json("../db/diseases.JSON", Diseases),
    % Verifica se a doença existe na lista
    select(Disease, Diseases, Rest),
    get_dict(doenca, Disease, NomeDoenca),
    % Atualiza os dados da doença
    NewDisease = _{
        doenca: NomeDoenca,
        especialidade_relacionada: Especialidade,
        sintomas_associados: Sintomas,
        possivel_causa: Causa,
        medicamentos: Medicamentos
    },
    % Adiciona a doença atualizada à lista e salva no arquivo
    append(Rest, [NewDisease], NewData),
    write_json("../db/diseases.JSON", NewData),
    print_success("DOENCA ATUALIZADA COM SUCESSO!\n"),
    !.

% Encontra uma doença
read_disease(NomeDoenca):-
    read_json("../db/diseases.JSON", Diseases),
    % Verifica se a doença existe na lista
    select(Disease, Diseases, _),
    get_dict(doenca, Disease, NomeDoenca),
    % Obtém os detalhes da doença
    get_dict(especialidade_relacionada, Disease, Especialidade),
    get_dict(sintomas_associados, Disease, Sintomas),
    get_dict(possivel_causa, Disease, Causa),
    get_dict(medicamentos, Disease, Medicamentos),
    % Formata e exibe os detalhes da doença
    string_concat("Doenca: ", NomeDoenca, Header),
    string_concat("\nEspecialidade: ", Especialidade, Temp1),
    string_concat(Temp1, "\nSintomas: ", Temp2),
    string_concat(Temp2, "\nCausa: ", Temp3),
    string_concat(Temp3, Causa, Text),
    print_success(Header),
    print_bold(Text),
    print_bold(Sintomas),
    print_bold(Medicamentos),
    write("\n"),
    !.

% Deleta uma doença
delete_disease(NomeDoenca):-
    read_json("../db/diseases.JSON", Diseases),
    % Verifica se a doença existe na lista
    select(Disease, Diseases, Rest),
    get_dict(doenca, Disease, NomeDoenca),
    % Remove a doença da lista e salva a nova lista
    write_json("../db/diseases.JSON", Rest),
    print_success("DOENCA DELETADA COM SUCESSO!\n"),
    !.
