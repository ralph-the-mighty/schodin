package main;

import "core:fmt";
import "core:strings";


NodeType :: enum {
  SYMBOL,
  NUMBER,
  LIST,
  PROC,
  ERROR
};


Node :: struct {
  kind: NodeType,
  list: [dynamic]Node,
  sym: string,
  val: int,
  source_pos: int
};


new_int_node :: proc(val: int) -> Node {
  return Node{kind=.NUMBER, val=val};
}

new_sym_node :: proc(sym: string) -> Node {
  return Node{kind=.SYMBOL, sym=sym};
}

new_list_node :: proc(list: ..Node) -> Node {
  node := Node{kind=.LIST};
  for n in list {
    append_elem(&node.list, n);
  }
  return node;
}


//builtin procedures
proc_add :: proc(nodes: []Node) -> Node {
  return new_int_node(eval(nodes[0]).val + eval(nodes[1]).val);
}

proc_sub :: proc(nodes: []Node) -> Node {
  return new_int_node(eval(nodes[0]).val - eval(nodes[1]).val);
}

proc_mul :: proc(nodes: []Node) -> Node {
  return new_int_node(eval(nodes[0]).val * eval(nodes[1]).val);
}

proc_div :: proc(nodes: []Node) -> Node {
  return new_int_node(eval(nodes[0]).val / eval(nodes[1]).val);
}

global_env := map[string] (proc([]Node) -> Node){
  "+" = proc_add,
  "-" = proc_sub,
  "*" = proc_mul,
  "/" = proc_div
};


eval :: proc(node: Node) -> Node {
  #partial switch node.kind  {
    case .SYMBOL:
      assert(false, "Don't evaluate symbols yet");
    case .NUMBER:
      return node;
    case .LIST:
      if node.list[0].kind == .SYMBOL {
        f, ok := global_env[node.list[0].sym]; //TODO check actual local env
        if ok {
          return f(node.list[1:]);
        } else {
          //not in the env, then throw a fit
          assert(false, "Node not in environment");
        }
      } else {
        assert(false, "first node is not a symbol");
      }
  }
  return {};
}



node_equal :: proc(lhs, rhs: Node) -> bool {
  res: bool = lhs.kind == rhs.kind;
  if res {
    switch lhs.kind {
      case .SYMBOL: res = lhs.sym == rhs.sym;
      case .NUMBER: res = lhs.val == rhs.val;
      case .LIST:
        if len(lhs.list) == len(rhs.list) {
          for i in 0..len(lhs.list) {
            if !node_equal(lhs.list[i], rhs.list[i]) {
              res = false;
              break;
            }
          }
        } else {
          res = false;
        }
      case .PROC, .ERROR:
        fmt.println("Checking euality for proc/error, but we don't handle that yet");
    }
  }
  return res;
  
}



test_eval_line :: proc(code: string, expected: Node) {
  l: Lexer;
  init_lexer(&l, code);
  node := parse(&l);
  res := eval(node);

  assert(node_equal(res, expected), "Test failed.  Nodes are not equal.");
}




test :: proc() {
  test_eval_line("1", Node{kind = .NUMBER, val = 1});
  test_eval_line("( + 1 2)", Node{kind = .NUMBER, val = 3});
  test_eval_line("(* 2 3)", Node{kind = .NUMBER, val = 6});
  test_eval_line("( + 0 ( * 2 3 ) )", Node{kind = .NUMBER, val = 6});
  test_eval_line("( + 1 ( * 2 3 ) )", Node{kind = .NUMBER, val = 7});
  test_eval_line("(+ (* 4 8) (* 20 10))", Node{kind = .NUMBER, val = 232});
}





main :: proc() {

  // l : Lexer;
  // init_lexer(&l, "( + 1 ( * 2 3 ) )");


  // fmt.println("Hellope, world!");
  // //node := new_list_node(new_sym_node("a"), new_int_node(3), new_int_node(4));
  // node := parse(&l);
  // fmt.println(node);
  // fmt.println(eval(node));




  // fmt.println(type_of(proc_add));
  test();
}