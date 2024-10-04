:- module(controller, [run/1]).

:- use_module("../utils/utils").
:- use_module("./appointments").
:- use_module("./diagnosis").
:- use_module("./diseases").
:- use_module("./medications").
:- use_module("./patients").
:- use_module("./prescriptions").
:- use_module("./symptons").
:- use_module("./users").

run("1") :- appointments_menu.
run("2") :- medications_menu.
run("3") :- diseases_menu.
run("4") :- symptons_menu.
run("5") :- patients_menu.
run("6") :- prescriptions_menu.
run("7") :- diagnosis_menu.
run("logout") :- exit_system.
run(_):- write("Função não existe").