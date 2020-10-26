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
  procedure: (proc([]Node) -> Node),
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



true_sym  :: Node{kind=.SYMBOL, sym="#t"};
false_sym :: Node{kind=.SYMBOL, sym="#f"};




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

proc_gt :: proc(nodes: []Node) -> Node {
  return eval(nodes[0]).val > eval(nodes[1]).val ? true_sym : false_sym;
}

proc_lt :: proc(nodes: []Node) -> Node {
  return eval(nodes[0]).val < eval(nodes[1]).val ? true_sym : false_sym;
}

proc_eq :: proc(nodes: []Node) -> Node {
  return eval(nodes[0]).val == eval(nodes[1]).val ? true_sym : false_sym;
}

global_env := map[string]Node {
  "+" = Node{kind=.PROC, procedure=proc_add},
  "-" = Node{kind=.PROC, procedure=proc_sub},
  "*" = Node{kind=.PROC, procedure=proc_mul},
  "/" = Node{kind=.PROC, procedure=proc_div},
  ">" = Node{kind=.PROC, procedure=proc_gt},
  "<" = Node{kind=.PROC, procedure=proc_lt},
  "=" = Node{kind=.PROC, procedure=proc_eq},
};





eval :: proc(node: Node) -> Node {
  #partial switch node.kind  {
    case .SYMBOL:
      assert(false, "Don't evaluate symbols yet");
    case .NUMBER:
      return node;
    case .LIST:
      head := node.list[0];
      tail := node.list[1:];
      if head.kind == .SYMBOL {
        if head.sym == "if" {
          // (if (> 1 2) 3 4)
          test := eval(node.list[1]);
          if(test.kind == .SYMBOL && test.sym == "#f") {
            return eval(node.list[3]);
          } else {
            return eval(node.list[2]);
          }
        } else if head.sym == "define" {
          //do define stuff
          assert(false, "We haven't defined define yet");
        } else {
          //sym table lookup
          proc_node, ok := global_env[head.sym]; //TODO check actual local env
          if ok {
            //TODO: evaluate all the arguments first, then pass them to the proc
            return proc_node.procedure(tail);
          } else {
            //not in the env, then throw a fit
            fmt.printf("%i, Symbol %s is not defined\n", head.source_pos, head.sym);
            assert(false, "Symbol not defined");
          }
        }
      } else {
        assert(false, "first node of list is not a symbol, cannot evaluate");
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




test :: proc() {

  test_eval_line :: proc(code: string, expected: Node) {
    l: Lexer;
    init_lexer(&l, code);
    node := parse(&l);
    res := eval(node);

    assert(node_equal(res, expected), "Test failed.  Nodes are not equal.");
  }

  Num :: proc(n: int) -> Node { return Node{kind=.NUMBER, val=n}};


  test_eval_line("1", Num(1));
  test_eval_line("12345", Num(12345));
  test_eval_line("( + 1 2)", Num(3));
  test_eval_line("(* 2 3)", Num(6));
  test_eval_line("( + 0 ( * 2 3 ) )",  Num(6));
  test_eval_line("( + 1 ( * 2 3 ) )", Num(7));
  test_eval_line("(+ (* 4 8) (* 20 10))", Num(232));
  test_eval_line("(if (= 1 1) 1 0)", Num(1));
  test_eval_line("(if (= 1 0) 1 0)", Num(0));
  test_eval_line("(if (> 5 5) 1 0)", Num(0));
  test_eval_line("(if (> 5 3) 1 0)", Num(1));
  test_eval_line("(if (> 5 9) 1 0)", Num(0));
  test_eval_line("(if (< 1 1) 1 0)", Num(0));
  test_eval_line("(if (< 5 0) 1 0)", Num(0));
  test_eval_line("(if (< 5 9) 1 0)", Num(1));
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