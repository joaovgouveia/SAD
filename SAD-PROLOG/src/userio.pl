% Trabalhe Luciano
:- module(userio, [menu/0, exit_system/0]).

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



    print_bold_highlighted_black("                                                                ╔════════════════════════ Bem vindo ao ═══════════════════════╗\n\n"),
    print_bold("                                                                     d888888o.             .8.            8 8888888888o.     \n"),      
    print_bold("                                                                   .`8888:' `88.          .888.           8 88888    `^888.  \n"), 
    print_bold("                                                                   8.`8888.   Y8         :88888.          8 88888        `88.\n"), 
    print_bold("                                                                   `8.`8888.            .`888888.         8 88888         `88\n"), 
    print_bold("                                                                    `8.`8888.          .8.`888888.        8 88888          88\n"), 
    print_bold("                                                                     `8.`8888.        .8`8.`888888.       8 88888          88\n"), 
    print_bold("                                                                      `8.`8888.      .8' `8.`888888.      8 88888         ,88\n"), 
    print_bold("                                                                  8b   `8.`8888.    .8'   `8.`888888.     8 88888        ,88'\n"), 
    print_bold("                                                                  `8b.  ;8.`8888   .888888888.`888888.    8 88888    ,o88P'  \n"), 
    print_bold("                                                                   `Y8888P ,88P'  .8'       `8.`888888.   8 8888888888P'   \n\n"),
    print_bold("                                                                             (Sistema Automático de Diagnósticos)\n"),
    print_bold_highlighted_black("                                                                ╚═══════════════════════════PROTÓTIPO═════════════════════════╝\n\n"),
    print_bold("                      (1)                       (2)                      (3)                     (4)                    (5)                      (6)                       (7)\n"),
    print_highlighted_cyan("                  APPOINTMENT                MEDICATION                DISEASES                SYMPTOM                PATIENT                PRESCRIPTON                DIAGNOSIS\n"),
    write("Opção:\n> "), 
    read_line_to_string(user_input, Option),
    run(Option).