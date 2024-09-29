:- module(patients, [
    patients_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

patients_menu :- 
    write("menu de pacientes\n").

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
    
% Deleta um paciente
delete_patient(Id):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.\n")),
    select(Patient, Patients, Rest),
    get_dict(id_patient, Patient, Id),
    print_success("PACIENTE DELETADO COM SUCESSO!\n"),
    write_json("../db/patients.JSON", Rest),!.