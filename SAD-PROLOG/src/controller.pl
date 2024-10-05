:- module(controller, [
    run/1
    ]).

:- use_module("../utils/utils").
:- use_module("./appointments").
:- use_module("./diagnosis").
:- use_module("./diseases").
:- use_module("./medications").
:- use_module("./patients").
:- use_module("./prescriptions").
:- use_module("./symptoms").
:- use_module("./users").
:- use_module("./userio").
:- use_module(library(system)).

run("1") :- appointments_menu.
run("2") :- medications_menu.
run("3") :- diseases_menu.
run("4") :- symptoms_menu.
run("5") :- patients_menu.
run("6") :- prescriptions_menu.
run("7") :- diagnosis_menu.
run("8") :- users_menu.
run("logout") :- exit_system.
run(_):- print_warning("Função não existe"), sleep(2), start_menu.


