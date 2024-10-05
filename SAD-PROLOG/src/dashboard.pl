:- module(dashboard, [show_dashboard/0]).

:- use_module("../utils/utils").
:- use_module("./users.pl").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).


show_dashboard :-
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
    read_json("../db/appointments.JSON", Appointments),
    get_diseases(Appointments, Diseases),
    trending_disease_amount(Diseases, Value),
    get_trending_diseases(Diseases, Value, TrendingDiseases),
    print_bold("Quantidade de Casos (de cada Doença): "),
    print_highlighted(Value),
    print_diseases(TrendingDiseases).

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
    length(Doctors, Count).

count_patients(Count) :-
    read_json("../db/patients.JSON", Patients),
    length(Patients, Count).

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

get_diseases([], []).
get_diseases([H|T], Diseases) :-
    get_diseases(T, Rest),
    get_dict(diagnostico, H, DiseaseName),
    (find_disease_in_list(Rest, DiseaseName, Disease) ->
        get_dict(amount, Disease, Amount),
        NewAmount is Amount + 1,
        get_dict(amount, Disease, _, NewDisease, NewAmount),
        delete(Rest, Disease, NewList),
        Diseases = [NewDisease|NewList];
        NewDisease = _{disease: DiseaseName, amount: 1},
        append(Rest, [NewDisease], Diseases)
   ).

find_disease_in_list([H|T], Name, Disease):-
    get_dict(disease, H, DiseaseName),
    (DiseaseName == Name -> Disease = H; find_disease_in_list(T, Name, Disease)),!.

trending_disease_amount([], 0).
trending_disease_amount([H|T], A) :-
    trending_disease_amount(T, OtherAmount),
    get_dict(amount, H, Amount),
    (Amount > OtherAmount -> A is Amount; A is OtherAmount).

get_trending_diseases([], _,[]).
get_trending_diseases([H|T], Value, Diseases) :-
    get_trending_diseases(T, Value, Jose),
    (get_dict(amount, H, Value) -> Diseases = [H|Jose]; Diseases = Jose).

print_diseases([]) :- write("\n").
print_diseases([H|T]) :-
    get_dict(disease, H, Name),
    print_bold("\n-> "),
    print_success(Name), 
    print_diseases(T).