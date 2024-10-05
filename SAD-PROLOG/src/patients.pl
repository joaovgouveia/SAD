:- module(patients, [
    patients_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% PATIENTS RUN
run_patients("1") :- menu_create_patient.
run_patients("2") :- menu_update_patient.
run_patients("3") :- menu_read_patient.
run_patients("4") :- menu_delete_patient.
run_patients("logout") :- exit_system.
run_patients("back") :- start_menu.
run_patients(_):- print_warning("Função não existe\n"), sleep(2), patients_menu.

% Menu de pacientes
patients_menu :- 
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                        ╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔╦╗╔═╗\n"),
    print_bold_highlighted_blue("                                        ╠═╝╠═╣ ║ ║║╣ ║║║ ║ ╚═╗\n"),
    print_bold_highlighted_blue("                                        ╩  ╩ ╩ ╩ ╩╚═╝╝╚╝ ╩ ╚═╝\n"), 
    print_bold(                 "                (1)                    (2)                   (3)                   (4)\n"),
    print_highlighted_yellow(   "           CRIA PACIENTE         ATUALIZA PACIENTE       VER PACIENTE        DELETA PACIENTE\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients(Option).


menu_create_patient:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOME:\n "),
    read_line_to_string(user_input, Name),
    print_bold_highlighted_blue("IDADE:\n "),
    read_line_to_string(user_input, Age),
    create_patient(Id, Name, Age), sleep(2),
    patients_menu.



% Cria um paciente
create_patient(Id, Name, Age):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> print_error("PACIENTE JA EXISTE NO SISTEMA.\n");
        NewPatient = _{
            consultas: [],
            id_patient: Id,
            idade: Age,
            nome_patient: Name 
        },
        append(Patients, [NewPatient], PatientsNew),
        write_json("../db/patients.JSON", PatientsNew),
        print_success("PACIENTE CADASTRADO COM SUCESSO!\n")),!.


menu_update_patient:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOVO NOME:\n "),
    read_line_to_string(user_input, Nome),
    update_patient(Id, Nome), sleep(2),
    patients_menu.

% Atualiza um paciente
update_patient(Id, Name):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.\n")),
    select(Patient, Patients, Rest),
    get_dict(id_patient, Patient, Id),
    put_dict(nome_patient, Patient, Name, NewPatient),
    append(Rest, [NewPatient], NewData),
    print_success("PACIENTE ATUALIZADO COM SUCESSO!\n"),
    write_json("../db/patients.JSON", NewData),!.


menu_read_patient:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    read_patient(Id),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients(Option).

% Encontra um paciente
read_patient(Id):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.\n")),
    select(Patient, Patients, _),
    get_dict(id_patient, Patient, Id),
    get_dict(nome_patient, Patient, Name),
    get_dict(idade, Patient, Age),
    get_dict(consultas, Patient, Appointments),
    string_concat("Nome: ", Name, Header),
    string_concat("\nIdade: ", Age, Temp),
    string_concat(Temp, "\nConsultas: ", Text),
    print_success(Header),
    print_bold(Text),
    print_bold(Appointments),
    write("\n"),!.
    

menu_delete_patient:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    delete_patient(Id),
    read_line_to_string(user_input, Option),
    run_appointment(Option).

% Deleta um paciente
delete_patient(Id):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.\n")),
    select(Patient, Patients, Rest),
    get_dict(id_patient, Patient, Id),
    print_success("PACIENTE DELETADO COM SUCESSO!\n"),
    write_json("../db/patients.JSON", Rest),!.