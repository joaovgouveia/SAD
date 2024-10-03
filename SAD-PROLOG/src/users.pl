:- module(users, [
    users_menu/0,
    view_doctor/1,
    view_user/1,
    view_users_by_function/1,
    view_medicos/0,
    view_atuation/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

% Menu de usuários
users_menu :-
    write("menu de usuarios\n").

% Busca um usuário pelo ID
get_user(Id, User) :-
    (is_user(Id) -> true; print_error("USUÁRIO NÃO EXISTE!")),
    read_json("../db/users.JSON", Users),
    select(User, Users, _),
    get_dict(id, User, Id),!.

% Visualiza um medico pelo ID
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

% Visualiza um usuário pelo ID
view_user(Id) :-
    get_user(Id, User),
    get_dict(nome, User, Name),
    get_dict(funcao, User, Function),
    string_concat("Nome: ", Name, Header),
    string_concat("\nFunção: ", Function, Text),
    print_success(Header),
    print_bold(Text),
    write("\n"),!.

% Visualiza usuários por função
view_users_by_function(Function) :-
    read_json("../db/users.JSON", Users),
    include(is_function(Function), Users, FilteredUsers),
    (FilteredUsers == [] -> print_error("NENHUM USUÁRIO ENCONTRADO COM ESTA FUNÇÃO."); 
    format_users(FilteredUsers)).

is_function(Function, User) :-
    get_dict(funcao, User, UserFunction),
    UserFunction = Function.

% Formata a exibição de multiplos usuários
format_users([]) :- !.
format_users([User | Rest]) :-
    format_user(User),
    format_users(Rest).

format_user(User) :-
    get_dict(nome, User, Name),
    get_dict(funcao, User, Function),
    string_concat("Nome: ", Name, Header),
    string_concat("\nFunção: ", Function, Text),
    print_success(Header),
    print_bold(Text),
    write("\n").

% Visualiza todos os medicos
view_medicos :-
    read_json("../db/users.JSON", Users),
    include(is_doctor, Users, Medicos),
    (Medicos == [] -> print_error("NENHUM MÉDICO ENCONTRADO."); 
    format_medicos(Medicos)).

is_doctor(User) :-
    get_dict(funcao, User, Funcao),
    Funcao = "MEDICO".

format_medicos([]) :- !.
format_medicos([Medico | Rest]) :-
    format_medico(Medico),
    format_medicos(Rest).

format_medico(Medico) :-
    get_dict(nome, Medico, Name),
    get_dict(especialidade, Medico, Especialidade),
    get_dict(dias_atendimento, Medico, WorkDays),
    get_dict(horarios_atendimento, Medico, Schedule),
    string_concat("Nome: ", Name, Header),
    print_success(Header),
    print_bold("\nEspecialidade: "),
    write(Especialidade),
    print_bold("\nDias de Atendimento: "),
    write(WorkDays),
    print_bold("\nHorários de Atendimento: "),
    write(Schedule),
    write("\n").

% Visualiza a atuação de medicos por especialidade
view_atuation :-
    read_json("../db/users.JSON", Users),
    include(is_doctor, Users, Medicos),
    group_by_specialty(Medicos, GroupedBySpecialty),
    format_specialties(GroupedBySpecialty).

group_by_specialty(Medicos, Grouped) :-
    sort(2, @=<, Medicos, SortedMedicos),
    group_by(especialidade, SortedMedicos, Grouped).

group_by(_, [], []).
group_by(Key, [First | Rest], [[First | Same] | Groups]) :-
    get_dict(Key, First, Value),
    include(same_value(Key, Value), Rest, Same),
    exclude(same_value(Key, Value), Rest, Different),
    group_by(Key, Different, Groups).

same_value(Key, Value, Item) :-
    get_dict(Key, Item, ItemValue),
    Value = ItemValue.

format_specialties([]) :- !.
format_specialties([[First | Rest] | Groups]) :-
    get_dict(especialidade, First, Especialidade),
    print_bold(Especialidade),
    write(":\n"),
    format_names([First | Rest]),
    write("\n"),
    format_specialties(Groups).

format_names([]) :- !.
format_names([User | Rest]) :-
    get_dict(nome, User, Name),
    print_success(Name),
    write("\n"),
    format_names(Rest).
