:- module(diseases, [
    diseases_menu/0,
    diseases_menu_sec/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% DISEASES RUN
run_diseases("1") :- menu_update_disease_sympton_medication.
run_diseases("2") :- menu_view_disease.
run_diseases("3") :- menu_list_diseases.
run_diseases("logout") :- exit_system.
run_diseases("back") :- start_menu_adm.
run_diseases(_):- print_warning("Função não existe\n"), sleep(2), diseases_menu.

% DISEASES RUN SEC
run_diseases_sec("1") :- menu_update_disease_sympton_medication_sec.
run_diseases_sec("2") :- menu_view_disease_sec.
run_diseases_sec("3") :- menu_list_diseases_sec.
run_diseases_sec("logout") :- exit_system.
run_diseases_sec("back") :- start_menu_sec.
run_diseases_sec(_):- print_warning("Função não existe\n"), sleep(2), diseases_menu_sec.


diseases_menu :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),

  % ==================================================================================================================================
    print_bold_highlighted_blue("                                        ╔╦╗╦╔═╗╔═╗╔═╗╔═╗╔═╗╔═╗\n"),
    print_bold_highlighted_blue("                                         ║║║╚═╗║╣ ╠═╣╚═╗║╣ ╚═╗\n"),
    print_bold_highlighted_blue("                                        ═╩╝╩╚═╝╚═╝╩ ╩╚═╝╚═╝╚═╝\n"), 
    print_bold(                 "                            (1)                    (2)                   (3)\n"),
    print_highlighted_yellow(   "                      ATUALIZAR DOENÇA          VER DOENÇA           LISTA DOENÇAS\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases(Option).


diseases_menu_sec :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),

  % ==================================================================================================================================
    print_bold_highlighted_blue("                                        ╔╦╗╦╔═╗╔═╗╔═╗╔═╗╔═╗╔═╗\n"),
    print_bold_highlighted_blue("                                         ║║║╚═╗║╣ ╠═╣╚═╗║╣ ╚═╗\n"),
    print_bold_highlighted_blue("                                        ═╩╝╩╚═╝╚═╝╩ ╩╚═╝╚═╝╚═╝\n"), 
    print_bold(                 "                            (1)                    (2)                   (3)\n"),
    print_highlighted_yellow(   "                      ATUALIZAR DOENÇA          VER DOENÇA           LISTA DOENÇAS\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases_sec(Option).




eh_doenca(NomeDoenca, Diseases) :-
    member(Disease, Diseases),
    get_dict(doenca, Disease, NomeDoenca).

menu_update_disease_sympton_medication:-
    print_bold_highlighted_blue("NOME DOENÇA:\n "),
    read_line_to_string(user_input, NomeDoenca),
    print_bold_highlighted_blue("ADICIONAR SINTOMA? ( y / n(default))\n "),
    read_line_to_string(user_input, Resposta),
    (Resposta == "y" ->
    print_bold_highlighted_blue("NOVO SINTOMA:\n "),
    read_line_to_string(user_input, NewSintoma);
    NewSintoma = ""),
    print_bold_highlighted_blue("ADICIONAR MEDICAMENTO? ( y / n(default))\n "),
    read_line_to_string(user_input, RespostaMed),
    (RespostaMed == "y" ->
    print_bold_highlighted_blue("NOVO MEDICAMENTO:\n "),
    read_line_to_string(user_input, NewMedicamento);
    NewMedicamento = ""),
    update_disease_sympton_medication(NomeDoenca, NewSintoma, NewMedicamento), sleep(2),
    diseases_menu.

menu_update_disease_sympton_medication_sec:-
    print_bold_highlighted_blue("NOME DOENÇA:\n "),
    read_line_to_string(user_input, NomeDoenca),
    print_bold_highlighted_blue("ADICIONAR SINTOMA? ( y / n(default))\n "),
    read_line_to_string(user_input, Resposta),
    (Resposta == "y" ->
    print_bold_highlighted_blue("NOVO SINTOMA:\n "),
    read_line_to_string(user_input, NewSintoma);
    NewSintoma = ""),
    print_bold_highlighted_blue("ADICIONAR MEDICAMENTO? ( y / n(default))\n "),
    read_line_to_string(user_input, RespostaMed),
    (RespostaMed == "y" ->
    print_bold_highlighted_blue("NOVO MEDICAMENTO:\n "),
    read_line_to_string(user_input, NewMedicamento);
    NewMedicamento = ""),
    update_disease_sympton_medication(NomeDoenca, NewSintoma, NewMedicamento), sleep(2),
    diseases_menu_sec.


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


menu_view_disease:-
    print_bold_highlighted_blue("NOME DOENÇA:\n "),
    read_line_to_string(user_input, NomeDoenca),
    view_disease(NomeDoenca),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases(Option).

menu_view_disease_sec:-
    print_bold_highlighted_blue("NOME DOENÇA:\n "),
    read_line_to_string(user_input, NomeDoenca),
    view_disease(NomeDoenca),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases_sec(Option).


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

menu_list_diseases:-
    list_diseases, 
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases(Option).

menu_list_diseases_sec:-
    list_diseases, 
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_diseases_sec(Option).


% Funcao principal de listagem de doencas
list_diseases :-
    read_json("../db/diseases.JSON", Diseases),
    format_diseases(Diseases, "", Result),
    print_bold(Result),
    !.