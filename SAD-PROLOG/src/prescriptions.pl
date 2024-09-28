:- module(prescriptions, [
    prescriptions_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

prescriptions_menu :-
    write("menu de receitas\n").

list_symptons([], _, []).
list_symptons([X|Rest], N, [XF|Resp]) :-
    atom_number(Atom,N),
    atom_string(Atom, S),
    string_concat(S, ". ", Temp),
    string_concat(Temp, X, Temp1),
    string_concat(Temp1, "\n", XF),
    NN is N + 1,
    list_symptons(Rest, NN, Resp).

enumerate_symptons :-
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Sympton, (member(Doc, DadosReceitas), get_dict(sintoma, Doc, Sympton)), Symptons),
    list_symptons(Symptons, 1, FormattedSymptons),
    atomics_to_string(FormattedSymptons, '', ListSymptons),
    print_bold(ListSymptons),
    !.

valid_symptons(Symptons) :-
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Sympton, (member(Doc, DadosReceitas), get_dict(sintoma, Doc, Sympton), member(Sympton, Symptons)), ListSymptons),
    length(ListSymptons, R),
    length(Symptons, R1),
    R =:= R1.

generate_string(Symptons) :-
    format_symptons(Symptons, "", Result),
    print_highlighted("\nRECEITA MÉDICA:\n"),
    print_bold(Result),
    !.

format_symptons([], Acc, Acc).
format_symptons([Sympton|Rest], Acc, Result) :-
    format_one_sympton(Sympton, FormattedSympton),
    string_concat(Acc, FormattedSympton, NewAcc),
    format_symptons(Rest, NewAcc, Result).

format_one_sympton(Sympton, Formatted) :-
    get_dict(medicamento_nome, Sympton, MedNome),
    get_dict(medicamento_dosagem, Sympton, MedDosagem),
    get_dict(medicamento_frequencia, Sympton, MedFreq),
    get_dict(medicamento_tipo, Sympton, MedTipo),
    get_dict(medicamento_observacoes, Sympton, MedObs),
    format(string(Formatted), "\nMedicamento: ~w\nDosagem: ~w\nFrequência: ~w\nTipo: ~w\nObservações: ~w\n",
           [MedNome, MedDosagem, MedFreq, MedTipo, MedObs]).

generate_prescription(Symptons) :-
    (valid_symptons(Symptons) -> true ; print_error("SINTOMA(S) INFORMADO(S) NÃO CADASTRADO(S)")),
    read_json("../db/prescriptions.JSON", DadosReceitas),
    findall(Sympton, (member(Sympton, DadosReceitas), get_dict(sintoma, Sympton, SymptonN), member(SymptonN, Symptons)), SymptonsDoc),
    generate_string(SymptonsDoc),
    !.