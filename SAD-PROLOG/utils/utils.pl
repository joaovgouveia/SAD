:- module(utils, [
    read_json/2,
    write_json/2,
    clear_screen/0,
    print_warning/1,
    print_success/1,
    print_error/1,
    print_bold/1,
    print_spacer/0,
    remove_duplicates/2,
    intersection/3,
    eh_paciente/1,
    is_user/1,
    print_highlighted_blue/1,
    print_highlighted_yellow/1,
    print_highlighted_cyan/1,
    print_highlighted_red/1,
    print_highlighted_green/1,
    print_highlighted_magenta/1,    
    print_highlighted_black/1,    
    print_bold_highlighted_blue/1,
    print_bold_highlighted_yellow/1,
    print_bold_highlighted_cyan/1,
    print_bold_highlighted_red/1,
    print_bold_highlighted_green/1,
    print_bold_highlighted_magenta/1,
    print_bold_highlighted_black/1,

    exit_system/0
]).

:- use_module(library(http/json)).
:- use_module(library(ansi_term)).

% JSON Stuff
read_json(Path, D):-
    open(Path, read, Stream),
    json_read_dict(Stream, D),
    close(Stream).

write_json(Path, D):-
    open(Path, write, Stream),
    json_write_dict(Stream, D),
    close(Stream).

% IO releated
clear_screen:-
    (   current_prolog_flag(unix, true) % Verifica se é linux e limpa, se não limpa para outros OS.
    ->  shell(clear)
    ;   process_create(path(cmd), ['/C', 'cls'], [process(PID)]),
        process_wait(PID, _Status)
    ).

print_warning(Text):-
    ansi_format([bold, fg(yellow)], '~w', [Text]).

print_bold(Text) :-
    ansi_format([bold, fg(white)], '~w', [Text]).

print_success(Text):-
    ansi_format([bold, fg(green)], '~w', [Text]).

print_error(Text):-
    ansi_format([bold, fg(red)], '~w', [Text]).

print_spacer :-
    print_bold("==================================================\n").

% Lists
remove_duplicates([], []).
remove_duplicates([H|T], [H|R]):-
    \+ member(H, T),
    remove_duplicates(T, R).
remove_duplicates([H|T], R):-
    member(H, T),
    remove_duplicates(T, R).

intersection([], _, []).
intersection(L, L, L).
intersection([H|T], L, [H|R]) :-
    member(H, L),
    intersection(T, L, R).
intersection([_|T], L, R) :-
    intersection(T, L, R).

% Checks
eh_paciente(IdPaciente) :-
    read_json("../db/patients.JSON", DadosPacientes),
    member(Doc, DadosPacientes),
    get_dict(id_patient, Doc, IdPaciente).

is_user(Id) :-
    read_json("../db/users.JSON", Users),
    member(User, Users),
    get_dict(id, User, Id).

% exit_system
exit_system:-
    print_warning("Fechando Sistema..."),
    halt.

%===== HIGHLIGHT ==============================================
% highlight colors

print_highlighted_blue(Text) :-
    ansi_format([fg(blue)], '~w', [Text]).

print_highlighted_cyan(Text) :-
    ansi_format([fg(cyan)], '~w', [Text]).

print_highlighted_yellow(Text) :-
    ansi_format([fg(yellow)], '~w', [Text]).

print_highlighted_red(Text) :-
    ansi_format([fg(red)], '~w', [Text]).

print_highlighted_green(Text) :-
    ansi_format([fg(green)], '~w', [Text]).

print_highlighted_magenta(Text) :-
    ansi_format([fg(magenta)], '~w', [Text]).

print_highlighted_black(Text) :-
    ansi_format([fg(black)], '~w', [Text]).



% BOLD highlight colors

print_bold_highlighted_blue(Text) :-
    ansi_format([bold, fg(blue)], '~w', [Text]).

print_bold_highlighted_cyan(Text) :-
    ansi_format([bold, fg(cyan)], '~w', [Text]).

print_bold_highlighted_yellow(Text) :-
    ansi_format([bold, fg(yellow)], '~w', [Text]).

print_bold_highlighted_red(Text) :-
    ansi_format([bold, fg(red)], '~w', [Text]).

print_bold_highlighted_green(Text) :-
    ansi_format([bold, fg(green)], '~w', [Text]).

print_bold_highlighted_magenta(Text) :-
    ansi_format([bold, fg(magenta)], '~w', [Text]).

print_bold_highlighted_black(Text) :-
    ansi_format([bold, fg(black)], '~w', [Text]).
%==========================================================


% Nothing to see here.
duck:- write('quack').