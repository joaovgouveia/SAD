:- module(users, [
    users_menu_adm/0,
    users_menu_sec/0,
    view_doctor/1
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% USERS RUN ADM
run_users_adm("1") :- menu_view_user_adm.
run_users_adm("2") :- menu_view_doctor_adm.
run_users_adm("3") :- menu_view_users_by_function_adm.
run_users_adm("4") :- menu_view_medicos_adm.
run_users_adm("5") :- menu_view_atuation_adm.
run_users_adm("logout") :- exit_system.
run_users_adm("back") :- start_menu_adm.
run_users_adm(_):- print_warning("Função não existe\n"), sleep(2), users_menu_adm.

% USERS RUN SEC
run_users_sec("1") :- menu_view_user_sec.
run_users_sec("2") :- menu_view_doctor_sec.
run_users_sec("3") :- menu_view_users_by_function_sec.
run_users_sec("4") :- menu_view_medicos_sec.
run_users_sec("5") :- menu_view_atuation_sec.
run_users_sec("logout") :- exit_system.
run_users_sec("back") :- start_menu_sec.
run_users_sec(_):- print_warning("Função não existe\n"), sleep(2), users_menu_sec.

% Menu de usuários
users_menu_adm :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                            ╦ ╦╔═╗╔═╗╦═╗╔═╗\n"),
    print_bold_highlighted_blue("                                            ║ ║╚═╗║╣ ╠╦╝╚═╗\n"),
    print_bold_highlighted_blue("                                            ╚═╝╚═╝╚═╝╩╚═╚═╝\n"), 
    print_bold(                 "       (1)                 (2)                    (3)                       (4)                      (5)\n"),
    print_highlighted_yellow(   "    VER USUÁRIO         VER MÉDICO        VER USUÁRIO POR FUNÇÃO        LISTA MÉDICOS      VER MEDICOS ESPECIALIDADE\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

users_menu_sec :-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                            ╦ ╦╔═╗╔═╗╦═╗╔═╗\n"),
    print_bold_highlighted_blue("                                            ║ ║╚═╗║╣ ╠╦╝╚═╗\n"),
    print_bold_highlighted_blue("                                            ╚═╝╚═╝╚═╝╩╚═╚═╝\n"), 
    print_bold(                 "       (1)                 (2)                    (3)                       (4)                      (5)\n"),
    print_highlighted_yellow(   "    VER USUÁRIO         VER MÉDICO        VER USUÁRIO POR FUNÇÃO        LISTA MÉDICOS      VER MEDICOS ESPECIALIDADE\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).


% Busca um usuário pelo ID
get_user(Id, User) :-
    (is_user(Id) -> true; print_error("USUÁRIO NÃO EXISTE!")),
    read_json("../db/users.JSON", Users),
    select(User, Users, _),
    get_dict(id, User, Id),!.

menu_view_doctor_adm:-
    print_bold_highlighted_blue("ID MÉDICO: "),
    read_line_to_string(user_input, IdUser),
    view_doctor(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_adm(Option).

menu_view_doctor_sec:-
    print_bold_highlighted_blue("ID MÉDICO: "),
    read_line_to_string(user_input, IdUser),
    view_doctor(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_sec(Option).



% Visualiza um medico pelo ID
view_doctor(Id) :-
    get_user(Id, User),
    (get_dict(funcao, User, "MEDICO") -> true ; print_error("ID NÃO PERTENCE A UM MÉDICO!")),
    get_dict(funcao, User, "MEDICO"),
    get_dict(nome, User, Name),
    get_dict(especialidade, User, Area),
    get_dict(dias_atendimento, User, WorkDays),
    get_dict(horarios_atendimento, User, Schedule),
    get_dict(pacientes_atendidos, User, PatientCount),
    string_concat("Nome: ", Name, Header),
    print_bold(Header),
    print_bold("\nEspecialidade: "),
    write(Area),
    print_bold("\nDias de Atendimento: "),
    write(WorkDays),
    print_bold("\nHorários de Atendimento: "),
    write(Schedule),
    print_bold("\nPacientes Atendidos: "),
    write(PatientCount),
    write("\n"),!.


menu_view_user_adm:-
    print_bold_highlighted_blue("ID USUÁRIO: "),
    read_line_to_string(user_input, IdUser),
    view_user(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_adm(Option).

menu_view_user_sec:-
    print_bold_highlighted_blue("ID USUÁRIO: "),
    read_line_to_string(user_input, IdUser),
    view_user(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_sec(Option).

% Visualiza um usuário pelo ID
view_user(Id) :-
    get_user(Id, User),
    get_dict(nome, User, Name),
    get_dict(funcao, User, Function),
    string_concat("Nome: ", Name, Header),
    string_concat("\nFunção: ", Function, Text),
    print_bold(Header),
    print_bold(Text),
    write("\n"),!.

menu_view_users_by_function_adm:-
    print_bold_highlighted_blue("FUNÇÃO USUÁRIO: "),
    read_line_to_string(user_input, Funcao),
    view_users_by_function(Funcao),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_adm(Option).

menu_view_users_by_function_sec:-
    print_bold_highlighted_blue("FUNÇÃO USUÁRIO: "),
    read_line_to_string(user_input, Funcao),
    view_users_by_function(Funcao),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_sec(Option).

% Visualiza usuários por função
view_users_by_function(Function) :-
    read_json("../db/users.JSON", Users),
    include(is_function(Function), Users, FilteredUsers),
    (FilteredUsers == [] -> print_error("NENHUM USUÁRIO ENCONTRADO COM ESTA FUNÇÃO."); 
    format_users(FilteredUsers)).

is_function(Function, User) :-
    get_dict(funcao, User, Function).

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
    print_bold(Header),
    print_bold(Text),
    write("\n").



menu_view_medicos_adm:-
    view_medicos,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_adm(Option).


menu_view_medicos_sec:-
    view_medicos,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_sec(Option).

% Visualiza todos os medicos
view_medicos :-
    read_json("../db/users.JSON", Users),
    include(is_doctor, Users, Medicos),
    (Medicos == [] -> print_error("NENHUM MÉDICO ENCONTRADO."); 
    format_medicos(Medicos)).

is_doctor(User) :-
    get_dict(funcao, User, "MEDICO").

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
    print_bold(Header),
    print_bold("\nEspecialidade: "),
    write(Especialidade),
    print_bold("\nDias de Atendimento: "),
    write(WorkDays),
    print_bold("\nHorários de Atendimento: "),
    write(Schedule),
    write("\n").



menu_view_atuation_adm:-
    view_atuation,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_adm(Option).

menu_view_atuation_sec:-
    view_atuation,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users_sec(Option).


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
    get_dict(Key, Item, Value).

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
