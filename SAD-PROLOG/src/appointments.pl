:- module(appointments, [
    appointments_menu/0
    ]).

:- use_module("../utils/utils").
:- use_module("./userio").
:- use_module(library(readutil)).
:- set_prolog_flag(encoding, utf8).
:- use_module(library(strings)).
:- use_module(library(date)).
:- use_module(library(system)).


% APPOINTMENT RUN
run_appointment("1") :- menu_write_appointment.
run_appointment("2") :- menu_check_schedule.
run_appointment("3") :- menu_update_appointment.
run_appointment("logout") :- exit_system.
run_appointment("back") :- start_menu.
run_appointment(_):- print_warning("Função não existe\n"), sleep(2), appointments_menu.


appointments_menu:-
    clear_screen(),
    write(" ← VOLTAR"),
    print_bold_highlighted_black(" (back)\n"),
  
    print_bold_highlighted_blue("                                     ╔═╗╔═╗╔═╗╔═╗ ╦ ╔╗╔╔╦╗╔╦╗╔═╗╔╗╔╔╦╗\n"),
    print_bold_highlighted_blue("                                     ╠═╣╠═╝╠═╝║ ║ ║ ║║║ ║ ║║║║╣ ║║║ ║ \n"),
    print_bold_highlighted_blue("                                     ╩ ╩╩  ╩  ╚═╝ ╩ ╝╚╝ ╩ ╩ ╩╚═╝╝╚╝ ╩ \n\n"), 
    print_bold("                     (1)                           (2)                            (3)\n"),
    print_highlighted_yellow("              ADICIONAR CONSULTA        VER CONSULTAS DISPONÍVEIS       MUDAR STATUS DA CONSULTA\n\n"),
                                                              

    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run_appointment(Option).

% Função auxiliar de checagem de data
quebrar_data(DataString, Dia, Mes, Ano) :-
    atomic_list_concat([DiaString, MesString, AnoString], '/', DataString),
    atom_number(DiaString, Dia),
    atom_number(MesString, Mes),
    atom_number(AnoString, Ano).

% Função auxiliar de checagem de data
dia_da_semana(data(Dia, Mes, Ano), DiaDaSemana) :-
    (Mes < 3 -> Mes2 is Mes + 12, Ano2 is Ano - 1 ; Mes2 is Mes, Ano2 is Ano),
    K is Ano2 mod 100,
    J is Ano2 // 100,
    F is Dia + (13 * (Mes2 + 1)) // 5 + K + K // 4 + J // 4 + 5 * J,
    DiaDaSemanaNum is F mod 7,
    dia_da_semana_nome(DiaDaSemanaNum, DiaDaSemana).

% Função auxiliar de checagem de data
dia_da_semana_nome(0, "SABADO").
dia_da_semana_nome(1, "DOMINGO").
dia_da_semana_nome(2, "SEGUNDA").
dia_da_semana_nome(3, "TERCA").
dia_da_semana_nome(4, "QUARTA").
dia_da_semana_nome(5, "QUINTA").
dia_da_semana_nome(6, "SEXTA").

% Função auxiliar de checagem de MÉDICO
eh_medico(Medico) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico).

% Função auxiliar de checagem de data
dia_valido(Data, Medico) :-
    quebrar_data(Data, Dia, Mes, Ano),
    dia_da_semana(data(Dia, Mes, Ano), DiaDaSemana),
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(dias_atendimento, Doc, DiasAtendimento),
    member(DiaDaSemana, DiasAtendimento).

% Função auxiliar de checagem de horário
horario_valido(Horario, Medico) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(horarios_atendimento, Doc, Horarios),
    member(Horario, Horarios).

% Função auxiliar de geração de id
gera_id(Medico, Data, Horario, IdConsulta) :-
    atomics_to_string([Medico, Data, Horario], '_', IdConsulta).

% Função auxiliar de checagem de id
existe_id(IdConsulta) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    member(Doc, DadosConsultas),
    get_dict(status_consulta, Doc, "Em andamento"),
    get_dict(id_consulta, Doc, IdConsulta).

% Função auxiliar de checagem de consulta
check_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, Status) :-
    gera_id(Medico, Data, Horario, IdConsulta),
    (existe_id(IdConsulta) ->
    print_error("JÁ EXISTE UMA CONSULTA DESSE MÉDICO PARA ESSE DIA E HORÁRIO\n")
    ;
    update_json(IdConsulta, Medico, Data, Horario, Diagnostico, IdPaciente, Status)
    ),
    !.

% Função auxiliar de criação de consulta
update_json(IdConsulta, Medico, Data, Horario, Diagnostico, IdPaciente, Status) :-
    read_json("../db/appointments.JSON", Appointments),
    find_doctor_specialty(Medico, Especialidade),
    get_different_doctor(Medico, Especialidade, DiffMedico),
    get_doctor_appointments_unfinished(Medico, ConsultasM),
    get_doctor_appointments_unfinished(DiffMedico, ConsultasMD),
    DiffConsultas is ConsultasM - ConsultasMD,
    abs(DiffConsultas, DConsultas),
    (DConsultas =:= 5 -> doctor_more_busy(Medico, DiffMedico, MedicoAtt, Data, Horario, IdConsulta, IdConsultaAtt) ; MedicoAtt = Medico, IdConsultaAtt = IdConsulta),
    NovaConsulta = _{id_consulta: IdConsultaAtt,
                     data_consulta: Data,
                     horario_consulta: Horario,
                     medico_responsavel: MedicoAtt,
                     diagnostico: Diagnostico,
                     id_paciente: IdPaciente,
                     status_consulta: Status},
    append(Appointments, [NovaConsulta], AppointmentsAtualizados),
    update_doctor_appointments(MedicoAtt),
    insert_patient_appointment(IdConsultaAtt, IdPaciente),
    print_success("\nCONSULTA REGISTRADA COM SUCESSO\n"),
    write_json("../db/appointments.JSON", AppointmentsAtualizados),
    !.

% Função auxiliar de atualização de MÉDICO
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


% MENU write_appointment
menu_write_appointment:-
    print_bold_highlighted_blue("MEDICO: \n "),
    read_line_to_string(user_input, Medico),
    print_bold_highlighted_blue("DATA: "),
    print_highlighted_black("DD/MM/AAAA\n "),
    read_line_to_string(user_input, Data),
    print_bold_highlighted_blue("HORÁRIO: "),
    print_highlighted_black("HH:MM\n "),
    read_line_to_string(user_input, Horario),
    print_bold_highlighted_blue("DIAGNOSTICO:\n "),
    read_line_to_string(user_input, Diagnostico),
    print_bold_highlighted_blue("CPF:\n "),
    read_line_to_string(user_input, IdPaciente),    
    write_appointment(Medico, Data, Horario, Diagnostico, IdPaciente), sleep(2),    
    appointments_menu.


% Função principal de criação de consulta
write_appointment(Medico, Data, Horario, Diagnostico, IdPaciente) :-
    (eh_medico(Medico) ->
        (dia_valido(Data, Medico) ->
            (horario_valido(Horario, Medico) -> 
                (eh_paciente(IdPaciente) ->
                check_appointment(Medico, Data, Horario, Diagnostico, IdPaciente, "Em andamento")
                ;
                print_error("PACIENTE NÃO CADASTRADO NO SISTEMA\n"))
            ;
            print_error("HORÁRIO INVÁLIDO OU NÃO É UM HORÁRIO DE ATENDIMENTO, HORÁRIOS NO PADRÃO HH:MM\n"))
        ;
        print_error("DATA INVÁLIDA OU NÃO É UM DIA DE ATENDIMENTO, DATA NO PADRÃO DD/MM/AAAA\n"))
    ;
    print_error("MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO\n")
    ),
    !.

% Função auxiliar de atualização de paciente
insert_patient_appointment(IdConsulta, IdPaciente) :-
    read_json("../db/patients.JSON", DadosPacientes),
    select(Doc, DadosPacientes, DemaisDados),
    get_dict(id_patient, Doc, IdPaciente),
    get_dict(consultas, Doc, ConsultasPaciente),
    append(ConsultasPaciente, [IdConsulta], ConsultasAtualizadas),
    put_dict(consultas, Doc, ConsultasAtualizadas, NovoDoc),
    append(DemaisDados, [NovoDoc], DadosAtualizados),
    write_json("../db/patients.JSON", DadosAtualizados).

% Função auxiliar de checagem de status
eh_status_valido(Status) :-
    member(Status, ["Cancelada","Concluída"]).

% Função auxiliar de atualização de status
update_status(IdConsulta, NovoStatus, DadosConsultas) :-
    select(Doc, DadosConsultas, DemaisDados),
    get_dict(id_consulta, Doc, IdConsulta),
    put_dict(status_consulta, Doc, NovoStatus, NovoDoc),
    append(DemaisDados, [NovoDoc], DadosAtualizados),
    print_success("CONSULTA ATUALIZADA COM SUCESSO\n"),
    write_json("../db/appointments.JSON", DadosAtualizados),
    !.

% Função auxiliar de checagem de consulta atualizável
existe_consulta_atualizavel(IdConsulta, DadosConsultas) :-
    member(Doc, DadosConsultas),
    get_dict(id_consulta, Doc, IdConsulta),
    get_dict(status_consulta, Doc, StatusConsulta),
    StatusConsulta == "Em andamento".


% menu_update_appointment
menu_update_appointment:-
    print_bold_highlighted_blue("ID CONSULTA: "),
    read_line_to_string(user_input, IdConsulta),
    print_bold_highlighted_blue("NOVO STATUS: "),
    read_line_to_string(user_input, NovoStatus),
    update_appointment(IdConsulta, NovoStatus), sleep(2),
    appointments_menu.



% Função principal de atualização de consulta
update_appointment(IdConsulta, NovoStatus) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    (existe_consulta_atualizavel(IdConsulta, DadosConsultas) ->
        (eh_status_valido(NovoStatus) ->
        update_status(IdConsulta, NovoStatus, DadosConsultas)
        ;
        print_error("NOVO STATUS INVÁLIDO\n")
        )
    ;
    print_error("ID DA CONSULTA INVÁLIDO/CONSULTA NÃO EXISTE OU CONSULTA JÁ FINALIZADA\n")
    ),
    !.

% Função auxiliar de busca de consultas_paciente
consultas_paciente(Consultas, ConsultasPaciente) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    findall(Consulta, (member(Consulta, DadosConsultas), get_dict(id_consulta, Consulta, IdConsulta), member(IdConsulta, Consultas)), ConsultasPaciente).

% Função auxiliar de formatação de consultas_paciente
generate_string(Consultas, NomePaciente) :-
    consultas_paciente(Consultas, ConsultasPaciente),
    format_consultas(ConsultasPaciente, "", Result),
    string_concat("\nCONSULTAS DO PACIENTE: ", NomePaciente, Temp),
    string_concat(Temp, "\n", Paciente),
    print_highlighted(Paciente),
    print_bold(Result),
    !.

% Função auxiliar de formatação de consultas_paciente
format_consultas([], Acc, Acc).
format_consultas([Consulta|Rest], Acc, Result) :-
    format_one_consulta(Consulta, FormattedConsulta),
    string_concat(Acc, FormattedConsulta, NewAcc),
    format_consultas(Rest, NewAcc, Result).

% Função auxiliar de formatação de consultas_paciente
format_one_consulta(Consulta, Formatted) :-
    get_dict(id_consulta, Consulta, IdConsulta),
    get_dict(data_consulta, Consulta, DataConsulta),
    get_dict(horario_consulta, Consulta, HorarioConsulta),
    get_dict(medico_responsavel, Consulta, MedicoResponsavel),
    get_dict(diagnostico, Consulta, Diagnostico),
    get_dict(status_consulta, Consulta, StatusConsulta),
    format(string(Formatted), "\nID da Consulta: ~w\nData: ~w\nHorário: ~w\nMédico Responsável: ~w\nDiagnóstico: ~w\nStatus: ~w\n",
           [IdConsulta, DataConsulta, HorarioConsulta, MedicoResponsavel, Diagnostico, StatusConsulta]).

% Função principal de visualização de consultas_paciente
view_patient_appointments(IdPaciente) :-
    (eh_paciente(IdPaciente) -> true ; print_error("PACIENTE NÃO CADASTRADO NO SISTEMA\n")),
    read_json("../db/patients.JSON", DadosPacientes),
    member(Doc, DadosPacientes),
    get_dict(id_patient, Doc, IdPaciente),
    get_dict(nome_patient, Doc, NomePaciente),
    get_dict(consultas, Doc, Consultas),
    length(Consultas, Size),
    (Size > 0 ->
    generate_string(Consultas, NomePaciente)
    ;
    print_warning("O PACIENTE NÃO POSSUI CONSULTAS\n")
    ),
    !.

% Função auxiliar de busca de especialidade_medico
find_doctor_specialty(Medico, Especialidade) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(especialidade, Doc, Especialidade).

% Função auxiliar de busca de outro MÉDICO da mesma especialidade
get_different_doctor(Medico, Especialidade, DiffMedico) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, DiffMedico),
    get_dict(especialidade, Doc, Especialidade),
    Medico \= DiffMedico.

% Função auxiliar de busca da quantidade de consultas em andamento
get_doctor_appointments_unfinished(Medico, ConsultasEmAndamento) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    findall(Consulta, (member(Consulta, DadosConsultas),
                       get_dict(medico_responsavel, Consulta, Medico),
                       get_dict(status_consulta, Consulta, "Em andamento")), Consultas),
    length(Consultas, ConsultasEmAndamento).

% Função auxiliar de busca medico_mais_ocupado
doctor_more_busy(Medico, DiffMedico, MedicoAtt, Data, Horario, IdConsulta, IdConsultaAtt) :-
    get_doctor_appointments_unfinished(Medico, ConsultasM),
    get_doctor_appointments_unfinished(DiffMedico, ConsultasMD),
    Medico \= DiffMedico,
    (ConsultasM >= ConsultasMD -> balance(DiffMedico, MedicoAtt, Data, Horario, IdConsultaAtt) ; MedicoAtt = Medico, IdConsultaAtt = IdConsulta).

% Função auxiliar de balanceamento de consulta
balance(DiffMedico, MedicoAtt, Data, Horario, IdConsultaAtt) :-
    gera_id(DiffMedico, Data, Horario, IdConsultaAtt),
    MedicoAtt = DiffMedico,
    string_concat("\nMÉDICO SOLICITADO ESTÁ COM A AGENDA CHEIA, CONSULTA MARCADA PARA: ", DiffMedico, Temp),
    string_concat(Temp, "\n", Troca),
    print_warning(Troca).

% Função auxiliar de busca dos dias que o MÉDICO trabalha
get_days_of_work(Medico, DiasAtendimento) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(dias_atendimento, Doc, DiasAtendimento).

% Função auxiliar de busca das horas em que o MÉDICO trabalha
get_hours_of_work(Medico, HorariosAtendimento) :-
    read_json("../db/users.JSON", DadosMedicos),
    member(Doc, DadosMedicos),
    get_dict(nome, Doc, Medico),
    get_dict(horarios_atendimento, Doc, HorariosAtendimento).

% Função auxiliar de geração da lista de dias e horas de trabalho
generate_list_of_dayshours([], _, []).
generate_list_of_dayshours([X|Xs], Lista2, Resultados) :-
    generate_list_of_dayshours(Xs, Lista2, ResultadosXs),
    help_generate(X, Lista2, Combinacoes),
    append(Combinacoes, ResultadosXs, Resultados).

% Função auxiliar de geração da lista de dias e horas de trabalho
help_generate(_, [], []) :- !.
help_generate(X, [Y|Ys], [CombS|Combs]) :-
    atomic_list_concat([X, Y], ' ', Comb),
    atom_string(Comb, CombS),
    help_generate(X, Ys, Combs).

% Função auxiliar de filtragem de dias de trabalho
filter_list([], _, []).
filter_list([X|Rest], DaysWithAppointments, Resp) :-
    member(X, DaysWithAppointments),
    !,
    filter_list(Rest, DaysWithAppointments, Resp).
filter_list([X|Rest], DaysWithAppointments, [X|Resp]) :-
    filter_list(Rest, DaysWithAppointments, Resp).

% Função auxiliar de busca dos dias da próxima semana com consultas
get_days_with_appointments(Medico, DaysWithAppointments) :-
    read_json("../db/appointments.JSON", DadosConsultas),
    findall(Consulta, (member(Consulta, DadosConsultas),
    get_dict(data_consulta, Consulta, DataConsulta),
    get_dict(medico_responsavel, Consulta, Medico),
    get_dict(status_consulta, Consulta, "Em andamento"),
    date_in_interval(DataConsulta)),
    Consultas),
    generate_days_with_appointments(Consultas, DaysWithAppointments).

% Função auxiliar de busca dos dias da próxima semana com consultas
generate_days_with_appointments(Consultas, DaysWithAppointments) :-
    findall(DayWithTime, (
        member(Consulta, Consultas),
        get_dict(data_consulta, Consulta, Data),
        quebrar_data(Data, Dia, Mes, Ano),
        dia_da_semana(data(Dia, Mes, Ano), DiaDaSemana),
        get_dict(horario_consulta, Consulta, Horario),
        atomic_list_concat([DiaDaSemana, Horario], ' ', DayWithTimeA),
        atom_string(DayWithTimeA, DayWithTime)
    ), DaysWithAppointments).

% Função auxiliar de checagem de intervalo de dias
date_in_interval(DataStr) :-
    get_time(Now),
    stamp_date_time(Now, DateTimeNow, local),
    date_time_value(date, DateTimeNow, DateNow),
    NextWeekTimestamp is Now + 7 * 86400,
    stamp_date_time(NextWeekTimestamp, DateTimeNext, local),
    date_time_value(date, DateTimeNext, DateNext),
    split_string(DataStr, "/", "", [DiaStr, MesStr, AnoStr]),
    atom_number(DiaStr, Dia),
    atom_number(MesStr, Mes),
    atom_number(AnoStr, Ano),
    Data = date(Ano, Mes, Dia),
    Data @>= DateNow,
    Data @=< DateNext.

% Função auxiliar de formatação de horários disponíveis
format_schedule(FreeDays, FormattedDays) :-
    group_by_day(FreeDays, GroupedDays),
    format_groups(GroupedDays, FormattedDays).

% Função auxiliar de formatação de horários disponíveis
group_by_day(FreeDays, GroupedDays) :-
    findall(Day-Hour, (
        member(DayHour, FreeDays),
        split_string(DayHour, " ", "", [Day|Hour])
    ), Pairs),
    group_pairs_by_key(Pairs, GroupedDays).

% Função auxiliar de formatação de horários disponíveis
format_groups(GroupedDays, FormattedDays) :-
    findall(FormattedDay, (
        member(Day-Hours, GroupedDays),
        flatten(Hours, FlatHours),
        atomic_list_concat(FlatHours, ' ', HourStr),
        atomic_list_concat([Day, HourStr], ' ', FormattedDay)
    ), Formatted),
    atomic_list_concat(Formatted, '\n', FormattedDays).


% menu_check_schedule
menu_check_schedule:-
    print_bold_highlighted_blue("MÉDICO: "),
    read_line_to_string(user_input, Medico),
    check_schedule(Medico),
    write("\nPressione [enter] para voltar para o menu "),
    read_line_to_string(user_input, _),
    write("\nOpção:\n> "), 
    read_line_to_string(user_input, Option),
    run_appointment(Option).




% Função principal de criação de tabela de horários disponíveis para atendimento
check_schedule(Medico) :-
    (eh_medico(Medico) -> true ; print_error("MÉDICO NÃO ENCONTRADO OU NÃO É MÉDICO\n")),
    get_days_of_work(Medico, DiasAtendimento),
    get_hours_of_work(Medico, HorariosAtendimento),
    generate_list_of_dayshours(DiasAtendimento, HorariosAtendimento, DaysHoursList),
    get_days_with_appointments(Medico, DaysWithAppointments),
    filter_list(DaysHoursList, DaysWithAppointments, FreeDays),
    format_schedule(FreeDays, FormattedDays),
    string_concat("\nHORÁRIOS DISPONÍVEIS DA SEMANA SEGUINTE (PRÓXIMOS 7 DIAS) PARA O(A) MÉDICO(A) ", Medico, Temp),
    string_concat(Temp, "\n", Tittle),
    print_highlighted_blue(Tittle),
    print_bold_highlighted_blue(FormattedDays),
    !.




