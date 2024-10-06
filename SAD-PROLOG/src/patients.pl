:- module(patients, [
    patients_menu_adm/0,
    patients_menu_med/0,
    patients_menu_sec/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(system)).

% PATIENTS RUN ADM
run_patients_adm("1") :- menu_create_patient_adm.
run_patients_adm("2") :- menu_update_patient_adm.
run_patients_adm("3") :- menu_read_patient_adm.
run_patients_adm("4") :- menu_delete_patient_adm.
run_patients_adm("logout") :- exit_system.
run_patients_adm("back") :- start_menu_adm.
run_patients_adm(_):- print_warning("Função não existe\n"), sleep(2), patients_menu_adm.

% PATIENTS RUN MED
run_patients_med("1") :- menu_read_patient_med.
run_patients_med("logout") :- exit_system.
run_patients_med("back") :- start_menu_med.
run_patients_med(_):- print_warning("Função não existe\n"), sleep(2), patients_menu_med.

% PATIENTS RUN SEC
run_patients_sec("1") :- menu_create_patient_sec.
run_patients_sec("2") :- menu_update_patient_sec.
run_patients_sec("3") :- menu_read_patient_sec.
run_patients_sec("4") :- menu_delete_patient_sec.
run_patients_sec("logout") :- exit_system.
run_patients_sec("back") :- start_menu_sec.
run_patients_sec(_):- print_warning("Função não existe\n"), sleep(2), patients_menu_sec.

% Menu de pacientes
patients_menu_adm :- 
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
    run_patients_adm(Option).

patients_menu_med :- 
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                        ╔═╗╔═╗╔╦╗╦╔═╗╔╗╔╔╦╗╔═╗\n"),
    print_bold_highlighted_blue("                                        ╠═╝╠═╣ ║ ║║╣ ║║║ ║ ╚═╗\n"),
    print_bold_highlighted_blue("                                        ╩  ╩ ╩ ╩ ╩╚═╝╝╚╝ ╩ ╚═╝\n"), 
    print_bold(                 "                                                 (1)\n"),
    print_highlighted_yellow(   "                                            VER PACIENTE\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients_med(Option).

patients_menu_sec :- 
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
    run_patients_sec(Option).


menu_create_patient_adm:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOME:\n "),
    read_line_to_string(user_input, Name),
    print_bold_highlighted_blue("IDADE:\n "),
    read_line_to_string(user_input, Age),
    create_patient(Id, Name, Age),
    read_line_to_string(user_input, Option),
    run_appointment_adm(Option).


menu_create_patient_sec:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOME:\n "),
    read_line_to_string(user_input, Name),
    print_bold_highlighted_blue("IDADE:\n "),
    read_line_to_string(user_input, Age),
    create_patient(Id, Name, Age),
    read_line_to_string(user_input, Option),
    run_appointment_sec(Option).



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


menu_update_patient_adm:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOME:\n "),
    read_line_to_string(user_input, Nome),
    update_patient(Id, Nome),
    read_line_to_string(user_input, Option),
    run_appointment_adm(Option).


menu_update_patient_sec:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    print_bold_highlighted_blue("NOME:\n "),
    read_line_to_string(user_input, Nome),
    update_patient(Id, Nome),
    read_line_to_string(user_input, Option),
    run_appointment_sec(Option).

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


menu_read_patient_adm:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    read_patient(Id),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients_adm(Option).

menu_read_patient_med:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    read_patient(Id),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients_med(Option).

menu_read_patient_sec:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    read_patient(Id),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_patients_sec(Option).

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
    

menu_delete_patient_adm:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    delete_patient(Id),
    read_line_to_string(user_input, Option),
    run_appointment_adm(Option).

menu_delete_patient_sec:-
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, Id),
    delete_patient(Id),
    read_line_to_string(user_input, Option),
    run_appointment_sec(Option).

% Deleta um paciente
delete_patient(Id):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.\n")),
    select(Patient, Patients, Rest),
    get_dict(id_patient, Patient, Id),
    print_success("PACIENTE DELETADO COM SUCESSO!\n"),
    write_json("../db/patients.JSON", Rest),!.