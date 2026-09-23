type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

datatype myState =
    NORMAL
  | IN_STRING
  | IN_COMMENT

val myState = ref NORMAL

val commentDepth = ref 0
val stringBuilder = ref ""
val stringStart = ref 0

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = 
    let 
        val pos = hd(!linePos) 
    in 
        (case !myState of
             NORMAL => ()
           | IN_STRING =>
               ErrorMsg.error pos "unterminated string"
           | IN_COMMENT =>
               ErrorMsg.error pos "unterminated comment");

        myState := NORMAL;

        Tokens.EOF(pos,pos)
    end

%% 
%s STRING COMMENT;
%%

<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>[ \t\r]+ => (continue());

<INITIAL>"/*" => (myState := IN_COMMENT; commentDepth := 1; YYBEGIN COMMENT; continue());
<COMMENT>"/*" => (commentDepth := !commentDepth + 1; continue());
<INITIAL>"*/" => (ErrorMsg.error yypos ("ended a comment that never started"); continue());
<COMMENT>"*/" => (commentDepth := !commentDepth - 1;
                    if !commentDepth = 0
                    then (myState := NORMAL; YYBEGIN INITIAL)
                    else ();
                    continue());

<COMMENT>. => (continue());
<COMMENT>\n =>
    (lineNum := !lineNum + 1;
     linePos := yypos :: !linePos;
     continue());

<INITIAL>\" => (myState := IN_STRING; stringStart := yypos; stringBuilder := ""; YYBEGIN STRING; continue());
<STRING>\" => (myState := NORMAL; YYBEGIN INITIAL; Tokens.STRING(!stringBuilder, !stringStart, yypos + 1));

<STRING>[\032-\126] => (stringBuilder := !stringBuilder ^ yytext; continue());

<STRING>\\\" => (stringBuilder := !stringBuilder ^ "\""; continue());
<STRING>\\\\ => (stringBuilder := !stringBuilder ^ "\\"; continue());
<STRING>\\t => (stringBuilder := !stringBuilder ^ "\t"; continue());
<STRING>\\n => (stringBuilder := !stringBuilder ^ "\n"; continue());
<STRING>\\[0-9]{3} => (let
                         val digits = String.substring (yytext, 1, 3)
                       in
                         case Int.fromString digits of
                           SOME code => if code >= 0 andalso code <= 255
                                        then (stringBuilder := !stringBuilder ^ (str (Char.chr code)); continue())
                                        else (ErrorMsg.error yypos ("illegal character code in literal string: " ^ yytext); continue())
                           | NONE => (ErrorMsg.impossible digits ^ " must be an int"; continue())
                       end);
<STRING>\\\^[@-_] => (let
                        val ltr = String.sub (yytext, 2)
                      in
                        stringBuilder := !stringBuilder ^ (str (Char.chr (ord ltr - 64)));
                        continue()
                      end);
<STRING>\\[ \t\n\012]+\\ => (continue());
<STRING>\\. => (ErrorMsg.error yypos ("illegal escape code in string: " ^ yytext); continue());
<STRING>[.\n] => (ErrorMsg.error yypos ("string can only contain printable characters and escape codes: " ^ yytext); continue());

<INITIAL>[0-9]+ => (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos + size yytext));

<INITIAL>"," => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));

<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));

<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));

<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));

<INITIAL>"." => (Tokens.DOT(yypos, yypos+1));

<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));

<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));

<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+2));

<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL>"end" => (Tokens.END(yypos, yypos+3));
<INITIAL>"in" => (Tokens.IN(yypos, yypos+2));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL>"let" => (Tokens.LET(yypos, yypos+3));
<INITIAL>"do" => (Tokens.DO(yypos, yypos+2));
<INITIAL>"to" => (Tokens.TO(yypos, yypos+2));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos+3));

<INITIAL>"while" => (Tokens.WHILE(yypos, yypos+5));

<INITIAL>"else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL>"if" => (Tokens.IF(yypos, yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL>[a-zA-Z][a-zA-Z_0-9]* => (Tokens.ID(yytext, yypos, yypos + size yytext));

<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
