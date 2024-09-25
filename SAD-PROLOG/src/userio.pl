% Trabalhe Luciano
:- module(userio, [menu/0, exit_system/0]).

:- use_module("../utils/utils").
:- use_module("./controller").

menu:-
    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run(Option).
