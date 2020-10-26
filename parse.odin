package main;

import "core:fmt";





Token :: enum {
  RPAREN,
  LPAREN,
  NUM,
  SYM,
  EOF,
  ERROR
}






Lexer :: struct {
  code: string,
  cursor: int,
  token : Token,
  sym_val : string,
  int_val : int,
};

init_lexer :: proc(using l: ^Lexer, source: string) {
  code = source;
  next_token(l);
}



is_digit :: proc(char: byte) -> bool {
  return char >= '0' && char <= '9';
}

is_sym_char :: inline proc(char: byte) -> bool {
  return char >= 33 && char <= 126;
}

is_whitespace :: inline proc(char: byte) ->bool {
  switch char {
    case ' ', '\n', '\r', '\f', '\v':
      return true;
  }
  return false;
}


parse_int :: proc(using l: ^Lexer) -> int {
  val: int;

  for cursor < len(code) && is_digit(code[cursor]) {
    val *= 10;
    val += int(code[cursor] - '0');
    cursor += 1;
  }

  return val;
}

parse_sym :: proc(using l: ^Lexer) -> string {
  start := cursor;
  for cursor < len(code) && is_sym_char(code[cursor]) {
    cursor += 1;
  }
  return string(code[start:cursor]);
}


next_token :: proc(using l: ^Lexer) {
  
  for cursor + 1 < len(code) && is_whitespace(code[cursor]) {
    cursor += 1;
  }

  if cursor == len(code) {
    token = .EOF;
    return;
  }

  switch code[cursor] {
    case '(':      token = .LPAREN; cursor += 1;
    case ')':      token = .RPAREN; cursor += 1;
    case '0'..'9': token = .NUM; int_val = parse_int(l);
    case:
      if is_sym_char(code[cursor]) {
        token = .SYM; sym_val = parse_sym(l);
      } else {
        token = .ERROR; 
        cursor += 1;
      }
  }
}


init_node :: proc(n: ^Node, kind: NodeType, using l: ^Lexer){
  n.kind = kind;
  n.source_pos = l.cursor;
}



parse :: proc(using l: ^Lexer) -> Node {
  node := Node{source_pos = cursor};
  switch token {
    case .LPAREN:
      node.kind = .LIST;
      next_token(l);
      for token != .RPAREN && token != .EOF {
        append_elem(&node.list, parse(l));
      }
      if token == .RPAREN {
        next_token(l);
      } else {
        assert(token == .EOF);
        fmt.printf("'(' at pos % does not have a matching ')'!", node.source_pos);
        return Node{kind = .ERROR, source_pos = node.source_pos};
      }

    case .RPAREN:
      fmt.println(token);
      fmt.println("We did not expect this token type here");

    case .NUM:
      node.kind = .NUMBER;
      node.val = int_val;
      next_token(l);

    case .SYM:
      node.kind = .SYMBOL;
      node.sym = sym_val;
      next_token(l);

    case .EOF:
      fmt.printf("We reached an EOF!");

    case .ERROR:
      fmt.printf("ERROR token!");
      next_token(l);
  }

  return node;

}




