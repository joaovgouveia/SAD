:- module(diseases, [
    diseases_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

diseases_menu :-
    write("menu de doencas\n").

eh_doenca(NomeDoenca, Diseases) :-
    member(Disease, Diseases),
    get_dict(doenca, Disease, NomeDoenca).

% Funcao principal de atualizacao de dados de uma doenca
update_disease_sympton_medication(NomeDoenca, NewSintoma, NewMedicamento):-
    read_json("../db/diseases.JSON", Diseases),
    select(Disease, Diseases, Rest),
    get_dict(doenca, Disease, NomeDoenca),
    get_dict(sintomas_associados, Disease, Sintomas),
    get_dict(medicamentos, Disease, Medicamentos),
    (NewSintoma == "" -> append(Sintomas, [], SintomasAtt) ; append(Sintomas, [NewSintoma], SintomasAtt)),
    (NewMedicamento == "" -> append(Medicamentos, [], MedicamentosAtt) ; append(Medicamentos, [NewMedicamento], MedicamentosAtt)),
    put_dict(sintomas_associados, Disease, SintomasAtt, DiseaseAtt1),
    put_dict(medicamentos, DiseaseAtt1, MedicamentosAtt, DiseaseAtt2),
    append(Rest, [DiseaseAtt2], DiseasesAtt),
    write_json("../db/diseases.JSON", DiseasesAtt),    
    print_success("DOENÇA ATUALIZADA COM SUCESSO!\n"),
    !.

format_diseases([], Formatted, Formatted).
format_diseases([Disease|Rest], FormattedTemp, FormattedResult) :-
    format_disease(Disease, FormattedDisease),
    string_concat(FormattedTemp, FormattedDisease, NewFormattedDisease),
    format_diseases(Rest, NewFormattedDisease, FormattedResult).

format_disease(Disease, FormattedDisease) :-
    get_dict(doenca, Disease, NomeDoenca),
    get_dict(especialidade_relacionada, Disease, Especialidade),
    get_dict(sintomas_associados, Disease, Sintomas),
    get_dict(possivel_causa, Disease, Causa),
    get_dict(medicamentos, Disease, Medicamentos),
    format(string(FormattedDisease), "\nDoença: ~w\nEspecialidade: ~w\nSintomas: ~w\nCausa: ~w\nMedicamentos Recomendados: ~w\n",
           [NomeDoenca, Especialidade, Sintomas, Causa, Medicamentos]).

% Funcao principal de visualizacao de doenca
view_disease(NomeDoenca):-
    read_json("../db/diseases.JSON", Diseases),
    (eh_doenca(NomeDoenca, Diseases) -> true ; print_error("DOENÇA NÃO CADASTRADA NO SISTEMA\n")),
    member(Disease, Diseases),
    get_dict(doenca, Disease, NomeDoenca),
    format_disease(Disease, FormattedDisease),
    print_bold(FormattedDisease),
    write("\n"),
    !.

% Funcao principal de listagem de doencas
list_diseases :-
    read_json("../db/diseases.JSON", Diseases),
    format_diseases(Diseases, "", Result),
    print_bold(Result),
    !.