:- module(controller, [
    run_adm/1,
    run_med/1,
    run_sec/1
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

% ADM ====================================================================
run_adm("1") :- appointments_menu_adm.
run_adm("2") :- medications_menu_adm.
run_adm("3") :- diseases_menu.
run_adm("4") :- symptoms_menu_adm.
run_adm("5") :- patients_menu_adm.
run_adm("6") :- prescriptions_menu_adm.
run_adm("7") :- diagnosis_menu_adm.
run_adm("8") :- users_menu_adm.
run_adm("logout") :- exit_system.
run_adm(_):- print_warning("Função não existe"), sleep(2), start_menu_adm.

% MED ====================================================================
run_med("1") :- appointments_menu_med.
run_med("2") :- medications_menu_med.
run_med("3") :- symptoms_menu_med.
run_med("4") :- patients_menu_med.
run_med("5") :- prescriptions_menu_med.
run_med("6") :- diagnosis_menu_med.
run_med("logout") :- exit_system.
run_med(_):- print_warning("Função não existe"), sleep(2), start_menu_med.

% SEC ====================================================================
run_sec("1") :- appointments_menu_sec.
run_sec("2") :- medications_menu_sec.
run_sec("3") :- diseases_menu_sec.
run_sec("4") :- symptoms_menu_sec.
run_sec("5") :- patients_menu_sec.
run_sec("6") :- prescriptions_menu_sec.
run_sec("7") :- diagnosis_menu_sec.
run_sec("8") :- users_menu_sec.
run_sec("logout") :- exit_system.
run_sec(_):- print_warning("Função não existe"), sleep(2), start_menu_sec.
