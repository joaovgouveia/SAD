:- module(users, [
    users_menu/0,
    view_doctor/1
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

users_menu :-
    write("menu de usuarios\n").


get_user(Id, User) :-
    (is_user(Id) -> true; print_error("USUÁRIO NÃO EXISTE!")),
    read_json("../db/users.JSON", Users),
    select(User, Users, _),
    get_dict(id, User, Id),!.

view_doctor(Id) :-
    get_user(Id, User),
    (get_dict(funcao, User, "MEDICO") -> true; print_error("ID NÃO PERTENCE A UM MÉDICO!")),
    get_dict(nome, User, Name),
    get_dict(especialidade, User, Area),
    get_dict(dias_atendimento, User, WorkDays),
    get_dict(horarios_atendimento, User, Schedule),
    get_dict(pacientes_atendidos, User, PatientCount),
    string_concat("Nome: ", Name, Header),
    print_success(Header),
    print_bold("\nEspecialidade: "),
    write(Area),
    print_bold("\nDias de Atendimento: "),
    write(WorkDays),
    print_bold("\nHorários de Atendimento: "),
    write(Schedule),
    print_bold("\nPacientes Atendidos: "),
    write(PatientCount),
    write("\n"),!.