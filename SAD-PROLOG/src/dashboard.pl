:- module(dashboard, [show_dashbaord/0]).

:- use_module("../utils/utils").
:- use_module("./users.pl").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).


show_dashbaord :-
    dashboard_header,
    system_counts_field,
    most_appointments_field,
    trending_diseases_field.

dashboard_header :-
    print_spacer, print_bold("                    DASHBOARD\n"), print_spacer.

most_appointments_header :-
    print_spacer, print_bold("          Médico(s) com mais consultas\n"), print_spacer.

system_counts_field :-
    count_doctors(DoctorCount),
    count_patients(PatientCount),
    print_bold("Quantidade Médicos: "),
    print_highlighted(DoctorCount),
    print_bold("\nQuantidade Pacientes: "),
    print_highlighted(PatientCount),
    write("\n").

trending_diseases_header :-
    print_spacer, print_bold("                Principais Doenças\n"), print_spacer.

trending_diseases_field :-
    trending_diseases_header,
    show_trending_diseases.

show_trending_diseases :-
    print_error("[implementar]").


most_appointments_field :-
    most_appointments_header,
    read_json("../db/users.JSON", Users),
    select_doctors(Users, Doctors),
    max_appointments(Doctors, Value),
    most_appointments(Doctors, Value, R),
    print_all_doctors(R).

count_doctors(Count) :-
    read_json("../db/users.JSON", Users),
    select_doctors(Users, Doctors),
    length(Doctors, Count),
    print_bold(Count).

count_patients(Count) :-
    read_json("../db/patients.JSON", Patients),
    length(Patients, Count),
    print_bold(Count).


most_appointments([], _, []).
most_appointments([H|T], Value, Doctors) :-
    most_appointments(T, Value ,A),
    get_dict(pacientes_atendidos, H, Line),
    number_string(X, Line),
    (X =:= Value -> 
        Doctors = [H|A];
        Doctors = A
    ),!.

select_doctors([], []).
select_doctors([H|T], Doctors) :-
    select_doctors(T, A),
    (get_dict(funcao, H, "MEDICO") -> 
        Doctors = [H|A];
        Doctors = A
    ),!.

max_appointments([], 0).
max_appointments([H|T], Quantity) :-
    max_appointments(T, Qt),
    get_dict(pacientes_atendidos, H, Line),
    number_string(Qh, Line),
    (Qt > Qh -> Quantity is Qt; Quantity is Qh).

print_all_doctors([]).
print_all_doctors([H|T]) :-
    get_dict(id, H, Id),
    view_doctor(Id),
    write("\n"),
    print_all_doctors(T).

test :- 
    show_dashbaord.