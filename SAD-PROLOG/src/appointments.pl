:- module(appointments, [
    appointments_menu/0
]).

:- use_module("../utils/utils").
:- use_module(library(strings)).

appointments_menu :-
    write("menu de consultas\n").

teste :-
    read_line_to_string(user_input, Data),
    view_patient_appointments(Data).


quebrar_data(DataString, Dia, Mes, Ano) :-
    atomic_list_concat([DiaString, MesString, AnoString], '/', DataString),
    atom_number(DiaString, Dia),
    atom_number(MesString, Mes),
    atom_number(AnoString, Ano).

dia_da_semana(data(Dia, Mes, Ano), DiaDaSemana) :-
    (Mes < 3 -> Mes2 is Mes + 12, Ano2 is Ano - 1 ; Mes2 is Mes, Ano2 is Ano),
    K is Ano2 mod 100,
    J is Ano2 // 100,
    F is Dia + (13 * (Mes2 + 1)) // 5 + K + K // 4 + J // 4 + 5 * J,
    DiaDaSemanaNum is F mod 7,
    dia_da_semana_nome(DiaDaSemanaNum, DiaDaSemana).

dia_da_semana_nome(0, "SABADO").
dia_da_semana_nome(1, "DOMINGO").
dia_da_semana_nome(2, "SEGUNDA").
dia_da_semana_nome(3, "TERCA").
dia_da_semana_nome(4, "QUARTA").
dia_da_semana_nome(5, "QUINTA").
dia_da_semana_nome(6, "SEXTA").

eh_medico(Medico) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico).

dia_valido(Data, Medico) :-
    quebrar_data(Data, Dia, Mes, Ano),
    dia_da_semana(data(Dia, Mes, Ano), DiaDaSemana),
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(dias_atendimento, Doc, DiasAtendimento),
    member(DiaDaSemana, DiasAtendimento).

horario_valido(Horario, Medico) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(horarios_atendimento, Doc, Horarios),
    member(Horario, Horarios).

eh_paciente(IdPaciente) :-
    read_json("../db/patients.JSON", DadosPacientes),
    member(Doc, DadosPacientes),
    get_dict(id_patient, Doc, IdPaciente).

gera_id(Medico, Data, Horario, IdConsulta) :-
    atomics_to_string([Medico, Data, Horario], '_', IdConsulta).

existe_id(IdConsulta) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    member(Doc, DadosConsultas),
    get_dict(id_consulta, Doc, IdConsulta).

check_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, Status) :-
    gera_id(Medico, Data, Horario, IdConsulta),
    (existe_id(IdConsulta) ->
    write("JÁ EXISTE UMA CONSULTA DESSE MÉDICO PARA ESSE DIA E HORÁRIO\n")
    ;
    update_json(IdConsulta, Medico, Data, Horario, Diagnostico, IdPaciente, Status)
    ).

update_json(IdConsulta, Medico, Data, Horario, Diagnostico, IdPaciente, Status) :-
    read_json("../db/appointments.JSON", Appointments),
    NovaConsulta = _{id_consulta: IdConsulta,
                     data_consulta: Data,
                     horario_consulta: Horario,
                     medico_responsavel: Medico,
                     diagnostico: Diagnostico,
                     id_paciente: IdPaciente,
                     status_consulta: Status},
    append(Appointments, [NovaConsulta], AppointmentsAtualizados),
    update_doctor_appointments(Medico),
    insert_patient_appointment(IdConsulta, IdPaciente),
    write_json("../db/appointments.JSON", AppointmentsAtualizados).

update_doctor_appointments(Medico) :-
    read_json("../db/users.JSON", DadosMedicos),
    select(Doc, DadosMedicos, DemaisDados),
    get_dict(nome, Doc, Medico),
    get_dict(pacientes_atendidos, Doc, Npacientes),
    atom_string(Atom1, Npacientes),
    atom_number(Atom1, NpacientesI),
    NpacientesAtt is NpacientesI + 1,
    atom_number(Atom2, NpacientesAtt),
    atom_string(Atom2, NpacientesAtt2),
    put_dict(pacientes_atendidos, Doc, NpacientesAtt2, NovoDoc),
    append(DemaisDados, [NovoDoc], DadosAtualizados),
    write_json("../db/users.JSON", DadosAtualizados).

write_appointment(Medico, Data, Horario, Diagnostico, IdPaciente) :-
    (eh_medico(Medico) ->
        (dia_valido(Data, Medico) ->
            (horario_valido(Horario, Medico) -> 
                (eh_paciente(IdPaciente) ->
                check_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, "Em andamento")
                ;
                write("PACIENTE NÃO CADASTRADO NO SISTEMA\n"))
            ;
            write("HORÁRIO INVÁLIDO OU NÃO É UM HORÁRIO DE ATENDIMENTO, HORÁRIOS NO PADRÃO HH:MM\n"))
        ;
        write("DATA INVÁLIDA OU NÃO É UM DIA DE ATENDIMENTO, DATA NO PADRÃO DD/MM/AAAA\n"))
    ;
    write("MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO\n")
    ).

insert_patient_appointment(IdConsulta, IdPaciente) :-
    read_json("../db/patients.JSON", DadosPacientes),
    select(Doc, DadosPacientes, DemaisDados),
    get_dict(id_patient, Doc, IdPaciente),
    get_dict(consultas, Doc, ConsultasPaciente),
    append(ConsultasPaciente, [IdConsulta], ConsultasAtualizadas),
    put_dict(consultas, Doc, ConsultasAtualizadas, NovoDoc),
    append(DemaisDados, [NovoDoc], DadosAtualizados),
    write_json("../db/patients.JSON", DadosAtualizados).

eh_status_valido(Status) :-
    member(Status, ["Cancelada","Concluída"]).

update_status(IdConsulta, NovoStatus, DadosConsultas) :-
    select(Doc, DadosConsultas, DemaisDados),
    get_dict(id_consulta, Doc, IdConsulta),
    put_dict(status_consulta, Doc, NovoStatus, NovoDoc),
    append(DemaisDados, [NovoDoc], DadosAtualizados),
    write_json("../db/appointments.JSON", DadosAtualizados).

existe_consulta_atualizavel(IdConsulta, DadosConsultas) :-
    member(Doc, DadosConsultas),
    get_dict(id_consulta, Doc, IdConsulta),
    get_dict(status_consulta, Doc, StatusConsulta),
    StatusConsulta == "Em andamento".

update_appointment(IdConsulta, NovoStatus) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    (existe_consulta_atualizavel(IdConsulta, DadosConsultas) ->
        (eh_status_valido(NovoStatus) ->
        update_status(IdConsulta, NovoStatus, DadosConsultas)
        ;
        write("NOVO STATUS INVÁLIDO\n")
        )
    ;
    write("ID DA CONSULTA INVÁLIDO/CONSULTA NÃO EXISTE OU CONSULTA JÁ FINALIZADA\n")
    ).

consultas_paciente(Consultas, ConsultasPaciente) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    findall(Consulta, (member(Consulta, DadosConsultas), get_dict(id_consulta, Consulta, IdConsulta), member(IdConsulta, Consultas)), ConsultasPaciente).

generate_string(Consultas) :-
    consultas_paciente(Consultas, ConsultasPaciente),
    format_consultas(ConsultasPaciente, "", Result),
    write(Result).

format_consultas([], Acc, Acc).
format_consultas([Consulta|Rest], Acc, Result) :-
    format_one_consulta(Consulta, FormattedConsulta),
    string_concat(Acc, FormattedConsulta, NewAcc),
    format_consultas(Rest, NewAcc, Result).

format_one_consulta(Consulta, Formatted) :-
    get_dict(id_consulta, Consulta, IdConsulta),
    get_dict(data_consulta, Consulta, DataConsulta),
    get_dict(horario_consulta, Consulta, HorarioConsulta),
    get_dict(medico_responsavel, Consulta, MedicoResponsavel),
    get_dict(diagnostico, Consulta, Diagnostico),
    get_dict(status_consulta, Consulta, StatusConsulta),
    format(string(Formatted), "\nID da Consulta: ~w\nData: ~w\nHorário: ~w\nMédico Responsável: ~w\nDiagnóstico: ~w\nStatus: ~w\n",
           [IdConsulta, DataConsulta, HorarioConsulta, MedicoResponsavel, Diagnostico, StatusConsulta]).

view_patient_appointments(IdPaciente) :-
    read_json("../db/patients.JSON", DadosPacientes),
    member(Doc, DadosPacientes),
    get_dict(id_patient, Doc, IdPaciente),
    get_dict(consultas, Doc, Consultas),
    length(Consultas, Size),
    (Size > 0 ->
    generate_string(Consultas)
    ;
    write("O PACIENTE NÃO POSSUI CONSULTAS\n")
    ).
