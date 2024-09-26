:- module(appointments, [
    appointments_menu/0
]).

:- use_module("../utils/utils").

appointments_menu :-
    write("menu de consultas\n").

teste :-
    read_line_to_string(user_input, Medico),
    read_line_to_string(user_input, Data),
    read_line_to_string(user_input, Horario),
    read_line_to_string(user_input, Diagnostico),
    read_line_to_string(user_input, IdPaciente),
    write_appointment(Medico, Data, Horario, Diagnostico, IdPaciente).


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

add_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, Status) :-
    gera_id(Medico, Data, Horario, IdConsulta),
    (existe_id(IdConsulta) ->
    write("JÁ EXISTE UMA CONSULTA DESSE MÉDICO PARA ESSE DIA E HORÁRIO")
    ;
    write_json("../db/appointments.JSON", _{id_consulta : IdConsulta,
                                            data_consulta : Data,
                                            horario_consulta : Horario,
                                            medico_responsavel : Medico,
                                            diagnostico : Diagnostico,
                                            id_paciente : IdPaciente,
                                            status_consulta : Status})).

write_appointment(Medico, Data, Horario, Diagnostico, IdPaciente) :-
    (eh_medico(Medico) ->
        (dia_valido(Data, Medico) ->
            (horario_valido(Horario, Medico) -> 
                (eh_paciente(IdPaciente) ->
                add_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, "Em andamento")
                ;
                write("PACIENTE NÃO CADASTRADO NO SISTEMA"))
            ;
            write("HORÁRIO INVÁLIDO OU NÃO É UM HORÁRIO DE ATENDIMENTO, HORÁRIOS NO PADRÃO HH:MM"))
        ;
        write("DATA INVÁLIDA OU NÃO É UM DIA DE ATENDIMENTO, DATA NO PADRÃO DD/MM/AAAA"))
    ;
    write("MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO")
    ).