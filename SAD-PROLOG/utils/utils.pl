:- module(utils, [
    read_json/2,
    write_json/2,
    clear_screen/0,
    print_warning/1,
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

print_succes(Text):-
    ansi_format([bold, fg(green)], '~w', [Text]).

print_error(Text):-
    ansi_format([bold, fg(red)], '~w', [Text]).

% exit_system
exit_system:-
    print_warning("Fechando Sistema..."),
    halt.

% Nothing to see here.
duck:- write('quack').