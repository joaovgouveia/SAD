:- module(users, [
    users_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% USERS RUN
run_users("1") :- menu_view_user.
run_users("2") :- menu_view_doctor.
run_users("3") :- menu_view_users_by_function.
run_users("4") :- menu_view_medicos.
run_users("5") :- menu_view_atuation.
run_users("logout") :- exit_system.
run_users("back") :- start_menu.
run_users(_):- print_warning("Função não existe\n"), sleep(2), users_menu.

% Permite que as cláusulas de group_by_specialty/2 e format_specialties/1
% sejam definidas em diferentes locais do arquivo
:- discontiguous group_by_specialty/2.
:- discontiguous format_specialties/1.

% Menu de usuários
users_menu :-
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

menu_view_doctor:-
    print_bold_highlighted_blue("ID MÉDICO: "),
    read_line_to_string(user_input, IdUser),
    view_doctor(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

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

menu_view_user:-
    print_bold_highlighted_blue("ID USUÁRIO: "),
    read_line_to_string(user_input, IdUser),
    view_user(IdUser),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

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

menu_view_users_by_function:-
    print_bold_highlighted_blue("FUNÇÃO USUÁRIO: "),
    read_line_to_string(user_input, Funcao),
    view_users_by_function(Funcao),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

% Visualiza usuários por função
view_users_by_function(Function) :-
    read_json("../db/users.JSON", Users),
    include(is_function(Function), Users, FilteredUsers),
    (FilteredUsers == [] -> print_error("NENHUM USUÁRIO ENCONTRADO COM ESTA FUNÇÃO."); 
    format_users(FilteredUsers)).

is_function(Function, User) :-
    get_dict(funcao, User, Function).

% Formata a exibicao de multiplos usuários
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

menu_view_medicos:-
    view_medicos,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

% Visualiza todos os medicos
view_medicos :-
    read_json("../db/users.JSON", Users),
    include(is_doctor, Users, Medicos),
    (Medicos == [] -> print_error("NENHUM MÉDICO ENCONTRADO."); 
    format_medicos(Medicos)).

% Verifica se o usuário é um medico
is_doctor(User) :-
    User.funcao == "MEDICO".

group_by_specialty(Medicos, GroupedBySpecialty) :-
    findall(Specialty-MedicosPerSpecialty,
        (   member(Medico, Medicos),
            Specialty = Medico.especialidade,
            findall(M, (member(M, Medicos), M.especialidade == Specialty), MedicosPerSpecialty)
        ),
        GroupedBySpecialty).

menu_view_atuation:-
    view_atuation,
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_users(Option).

group_by(_, [], []).
group_by(Key, [First | Rest], [[First | Same] | Groups]) :-
    get_dict(Key, First, Value),
    include(same_value(Key, Value), Rest, Same),
    exclude(same_value(Key, Value), Rest, Different),
    group_by(Key, Different, Groups).

same_value(Key, Value, Item) :-
    get_dict(Key, Item, Value).

format_specialties([]) :- !.
format_specialties([Specialty-MedicosPerSpecialty | Rest]) :-
    format('Especialidade: ~w~n', [Specialty]),
    format_medicos(MedicosPerSpecialty),
    format_specialties(Rest).

format_names([]) :- !.
format_names([User | Rest]) :-
    get_dict(nome, User, Name),
    print_success(Name),
    write("\n"),
    format_names(Rest).

format_medicos([]).
format_medicos([Medico | Rest]) :-
    format('  Médico: ~w, Atendimentos: ~w~n', [Medico.nome, Medico.pacientes_atendidos]),
    format_medicos(Rest).

% Visualiza a atuação dos medicos agrupados por especialidade
view_atuation :-
    read_json("../db/users.JSON", Users),
    include(is_doctor, Users, Medicos),
    group_by_specialty(Medicos, GroupedBySpecialty),
    format_specialties(GroupedBySpecialty),
    !.
