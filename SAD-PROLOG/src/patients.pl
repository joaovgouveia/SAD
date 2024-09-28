:- module(patients, [
    patients_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).

patients_menu :- 
    write("menu de pacientes\n").

create_patient(Id, Name, Age).
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> print_error("PACIENTE JA EXISTE NO SISTEMA."; true)),
    NewPatient = _{
        consultas: [],
        id_patient: Id,
        idade: Age,
        nome_patient: Name 
    },
    append(Patients, [NewPatient], PatientsNew),
    print_success("PACIENTE CADASTRADO COM SUCESSO!"),!.

update_patient(Id, Name):-
    read_json("../db/patients.JSON", Patients),
    (eh_paciente(Id) -> true; print_error("PACIENTE NAO CADASTRADO NO SISTEMA.")),
    select(Patient, Patients, Rest),
    get_dict(id_patient, Patient, Id),
    put_dict(nome_patient, Patient, Name, NewPatient),
    append(Rest, [NewPatient], NewData),
    print_success("PACIENTE ATUALIZADO COM SUCESSO\n"),
    write_json("../db/patients.JSON", NewData),
    !.

read_patient(Id).
delete_patient(Id).