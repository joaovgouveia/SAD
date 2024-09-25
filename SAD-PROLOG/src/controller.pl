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

run("teste") :- 
    write("Hell yeah\n").
run("appointments") :- appointments_menu.
run("diagnosis") :- diagnosis_menu.
run("diseases") :- diseases_menu.
run("medications") :- medications_menu.
run("patients") :- patients_menu.
run("prescriptions") :- prescriptions_menu.
run("symptons") :- symptons_menu.
run("users") :- users_menu.
run("logout") :- 
    exit_system.
run(_):- 
    write("Função não existe").