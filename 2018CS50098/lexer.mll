{
exception Eof
exception Nil
type token=
    |   FLOAT of float
    |   PLUS
    |   MINUS
    |   STAR
    |   SLASH
    |   LPAREN
    |   RPAREN
    |   LBRACK
    |   RBRACK
    |   COMMA
    |   COLON
    |   ASSIGN
    |   SEMI
    |   UNARY of string
    |   BINARY of string
    |   INVALID
}
rule token = parse
    |   "COUNT"
    |   "ROWCOUNT"
    |   "COLCOUNT"
    |   "SUM"
    |   "ROWSUM"
    |   "COLSUM"
    |   "AVG"
    |   "ROWAVG"
    |   "COLAVG"
    |   "MIN"
    |   "ROWMIN"
    |   "COLMIN"
    |   "MAX"
    |   "ROWMAX"
    |   "COLMAX"                                                          as lxm { UNARY lxm }
    |   "ADD"
    |   "SUBT"
    |   "MULT"
    |   "DIV"                                                             as lxm { BINARY lxm }
    |   ['-''+']?['0'-'9']+'.'['0'-'9']+                                  as lxm { FLOAT(float_of_string lxm) }
    |   '+'                                                                      { PLUS }
    |   '-'                                                                      { MINUS }
    |   '*'                                                                      { STAR }
    |   '/'                                                                      { SLASH }
    |   '('                                                                      { LPAREN }
    |   ')'                                                                      { RPAREN }
    |   '['                                                                      { LBRACK }
    |   ']'                                                                      { RBRACK }
    |   ','                                                                      { COMMA }
    |   ':'                                                                      { COLON }
    |   ":="                                                                     { ASSIGN }
    |   ';'                                                                      { SEMI }
    |   [' ''\t''\n']+                                                           { token lexbuf }
    |   [^' ''\t''\n''+''-''*''/''('')''['']'','':'';']+                         { INVALID }
    |   eof                                                                      { raise Eof }
{
    let main () = begin
        try
            let filename = Sys.argv.(1) in
            let file_handle = open_in filename in
            let lexbuf = Lexing.from_channel file_handle in
            while true do
                let result = token lexbuf in match result with
                    | FLOAT(i)  -> Printf.printf"float: %f\n" i
                    | PLUS      -> Printf.printf"+\n"
                    | MINUS     -> Printf.printf"-\n"
                    | STAR      -> Printf.printf"*\n"
                    | SLASH     -> Printf.printf"/\n"
                    | LPAREN    -> Printf.printf"LPAREN\n"
                    | RPAREN    -> Printf.printf"RPAREN\n"
                    | LBRACK    -> Printf.printf"LBRAC\n"
                    | RBRACK    -> Printf.printf"RBRAC\n"
                    | COLON     -> Printf.printf"COLON\n"
                    | COMMA     -> Printf.printf"COMMA\n"
                    | SEMI      -> Printf.printf"SEMICOLON\n"
                    | ASSIGN    -> Printf.printf"ASSIGNMENT\n"
                    | UNARY(x)  -> Printf.printf"UNARY: %s\n" x
                    | BINARY(y) -> Printf.printf"BINARY_OP: '%s'\n" y
                    | INVALID   -> Printf.printf"INVALID_TOKEN\n"
            done
            with Eof -> exit 0
        end;;
    main ();;
}
