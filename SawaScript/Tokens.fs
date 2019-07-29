module SawaScript.Tokens
open Language.Lexer

let EOL = createToken {
    name = "EOL";
    pattern = "\n";
 }

let WHITESPACE = createToken {
    name = "WHITESPACE";
    pattern = @"\s+";
 }

let VARNAME = createToken {
    name = "VARNAME";
    pattern = "[a-z]+";
 }

let VAL = createToken {
    name = "VAL";
    pattern = "val";
 }

let VAR = createToken {
    name = "VAR";
    pattern = "var";
 }

let NUMBERLITERAL = createToken {
    name = "NUMBERLITERAL";
    pattern = "[0-9]+";
 }

let EQUAL = createToken {
    name = "EQUAL";
    pattern = "=";
 }


let tokens = [
    EOL
    WHITESPACE
    VAL
    VARNAME
    NUMBERLITERAL
    EQUAL
 ]
