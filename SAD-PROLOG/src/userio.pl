% Trabalhe Luciano
:- module(userio, [
    menu/0,
    exit_system/0,
    start_menu/0
]).

:- use_module("../utils/utils").
:- use_module("./controller").


verifica_login(IdUser, SenhaUser) :-
    read_json("../db/users.JSON", DadosUser),
    member(Doc, DadosUser),
    get_dict(id, Doc, IdUser),
    get_dict(senha, Doc, SenhaUser).

menu:-
    print_bold_highlighted_blue("LOGIN\n "),
    read_line_to_string(user_input, InputLogin),
    print_bold_highlighted_blue("SENHA\n "),
    read_line_to_string(user_input, InputSenha),
    (verifica_login(InputLogin, InputSenha) -> start_menu ; print_warning("LOGIN OU SENHA INVÁLIDOS\n"), menu).  




start_menu:-
    clear_screen(),
    
    print_highlighted_blue("AAAA"),
    print_highlighted_red("AAAA"),
    print_highlighted_green("AAAA"),
    print_highlighted_yellow("AAAA"),
    print_highlighted_magenta("AAAA"),
    print_highlighted_cyan("AAAA"),
    print_highlighted_black("AAAA\n"),

    
    print_bold_highlighted_blue("AAAA"),
    print_bold_highlighted_red("AAAA"),
    print_bold_highlighted_green("AAAA"),
    print_bold_highlighted_yellow("AAAA"),
    print_bold_highlighted_magenta("AAAA"),
    print_bold_highlighted_cyan("AAAA"),
    print_bold_highlighted_black("AAAA\n\n\n"),



print_bold_highlighted_blue("                                                                             ███████╗ █████╗ ██████╗ \n"),
print_bold_highlighted_blue("                                                                             ██╔════╝██╔══██╗██╔══██╗\n"),
print_bold_highlighted_blue("                                                                             ███████╗███████║██║  ██║\n"),
print_bold_highlighted_blue("                                                                             ╚════██║██╔══██║██║  ██║\n"),
print_bold_highlighted_blue("                                                                             ███████║██║  ██║██████╔╝\n"),
print_bold_highlighted_blue("                                                                             ╚══════╝╚═╝  ╚═╝╚═════╝ \n"),
                        

print_bold_highlighted_blue("                                             ┌─┐┬┌─┐┌┬┐┌─┐┌┬┐┌─┐  ┌─┐┬ ┬┌┬┐┌─┐┌┬┐┌─┐┌┬┐┬┌─┐┌─┐  ┌┬┐┌─┐  ┌┬┐┬┌─┐┌─┐┌┐┌┌─┐┌─┐┌┬┐┬┌─┐┌─┐\n"),
print_bold_highlighted_blue("                                             └─┐│└─┐ │ ├┤ │││├─┤  ├─┤│ │ │ │ ││││├─┤ │ ││  │ │   ││├┤    │││├─┤│ ┬││││ │└─┐ │ ││  │ │\n"),
print_bold_highlighted_blue("                                             └─┘┴└─┘ ┴ └─┘┴ ┴┴ ┴  ┴ ┴└─┘ ┴ └─┘┴ ┴┴ ┴ ┴ ┴└─┘└─┘  ─┴┘└─┘  ─┴┘┴┴ ┴└─┘┘└┘└─┘└─┘ ┴ ┴└─┘└─┘\n\n"),


    print_bold("                      (1)                       (2)                      (3)                     (4)                    (5)                      (6)                       (7)\n"),
    print_highlighted_yellow("                  APPOINTMENT                MEDICATION                DISEASES                SYMPTOM                PATIENT                PRESCRIPTON                DIAGNOSIS\n"),
    write("Opção:"),
    print_bold_highlighted_black("(logout) "),
    write("→ SAIR\n>"),
    read_line_to_string(user_input, Option),
    run(Option).