:- module(controller, [run/1]).

:- use_module("../utils/Utils").


run("teste"):- write("Hell yeah\n").
run("logout"):- exit_system.
run:- write("Não é um comando").