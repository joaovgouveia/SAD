% Trabalhe Luciano
:- module(user_io, [menu/0, exit_system/0]).

:- use_module("../utils/Utils").
:- use_module("./Controller").

menu:-
    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run(Option).
