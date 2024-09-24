:- module(utils, [
    read_json/2,
    write_json/2,
    limpa_tela/0
]).

:- use_module(library(http/json)).

# Ler do JSON
read_json(Path, D):-
    open(Path, read, Stream),
    json_read_dict(Stream, D),
    close(Stream).

#Escrever no JSON
write_json(Path, D):-
    open(Path, write, Stream),
    json_write_dict(Stream, D),
    close(Stream).

#Limpa a tela
clear_screen:-
    (   current_prolog_flag(unix, true) % Verifica se é linux e limpa, se não limpa para outros OS.
    ->  shell(clear)
    ;   process_create(path(cmd), ['/C', 'cls'], [process(PID)]),
        process_wait(PID, _Status)
    ).

#Nothing to see here    
duck:- write('quack').