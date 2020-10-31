package main;

import "core:fmt";
import "core:strings";
import "core:os";


NodeType :: enum {
  SYMBOL,
  NUMBER,
  LIST,
  PROC,
  LAMBDA,
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
nil_sym   :: Node{kind=.LIST};




//builtin procedures
proc_add :: proc(nodes: []Node) -> Node {
  return new_int_node(nodes[0].val + nodes[1].val);
}

proc_sub :: proc(nodes: []Node) -> Node {
  return new_int_node(nodes[0].val - nodes[1].val);
}

proc_mul :: proc(nodes: []Node) -> Node {
  return new_int_node(nodes[0].val * nodes[1].val);
}

proc_div :: proc(nodes: []Node) -> Node {
  return new_int_node(nodes[0].val / nodes[1].val);
}

proc_gt :: proc(nodes: []Node) -> Node {
  return nodes[0].val > nodes[1].val ? true_sym : false_sym;
}

proc_lt :: proc(nodes: []Node) -> Node {
  return nodes[0].val < nodes[1].val ? true_sym : false_sym;
}

proc_eq :: proc(nodes: []Node) -> Node {
  return nodes[0].val == nodes[1].val ? true_sym : false_sym;
}


init_default_env :: proc(env: ^Env) {
  env._map["+"] = Node{kind=.PROC, procedure=proc_add};
  env._map["-"] = Node{kind=.PROC, procedure=proc_sub};
  env._map["*"] = Node{kind=.PROC, procedure=proc_mul};
  env._map["/"] = Node{kind=.PROC, procedure=proc_div};
  env._map[">"] = Node{kind=.PROC, procedure=proc_gt};
  env._map["<"] = Node{kind=.PROC, procedure=proc_lt};
  env._map["="] = Node{kind=.PROC, procedure=proc_eq};
}


Env :: struct {
  _map: map[string]Node,
  outer: ^Env,
};

new_env :: proc(outer: ^Env) -> Env {
  return Env{outer=outer};
}

insert :: proc(env: ^Env, sym: string, node: Node) {
  env._map[sym] = node;
}

lookup :: proc(env: ^Env, sym: string) -> Node {
  node, ok := env._map[sym];
  if ok {
    return node;
  } else {
    if env.outer != nil {
      return lookup(env.outer, sym);
    } else {
      fmt.printf("Symbol '%s' does not exist in the environment\n");
      os.exit(1);
    }
  }
}





eval :: proc(env: ^Env, node: Node) -> Node {
  #partial switch node.kind  {
    case .SYMBOL:
      return lookup(env, node.sym);
    case .NUMBER:
      return node;
    case .LIST:
      head := node.list[0];
      tail := node.list[1:];
      if head.kind == .SYMBOL {
        if head.sym == "if" {
          // (if (> 1 2) 3 4)
          test := eval(env, node.list[1]);
          if(test.kind == .SYMBOL && test.sym == "#f") {
            return eval(env, node.list[3]);
          } else {
            return eval(env, node.list[2]);
          }
        } else if head.sym == "define" {  //(define x 4)
          insert(env, tail[0].sym, eval(env, tail[1]));
        } else if head.sym == "begin" { //(begin (define x 4) x)
          ret_node : Node;
          for n in tail {
            ret_node = eval(env, n);
          }
          return ret_node;
        } else if head.sym == "lambda" {
          assert(false, "we do not handle lambda yet");
        } else {
          //sym table lookup
          proc_node := lookup(env, head.sym); //TODO check actual local env
          //TODO: evaluate all the arguments first, then pass them to the proc
          evaluated_args : [dynamic]Node;
          for arg in tail {
            append_elem(&evaluated_args, eval(env, arg));
          }
          return proc_node.procedure(evaluated_args[:]);
        }
      } else {
        assert(false, "first node of list is not a symbol, cannot evaluate");
      }
    case .LAMBDA:
      assert(false, "do not evaluate a lambda");
  }
  return {};
}









//TESTING


test :: proc() {

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
        case .PROC, .ERROR, .LAMBDA:
          fmt.println("Checking euality for proc/error/lambda, but we don't handle that yet");
      }
    }
    return res;
  }


  test_eval_line :: proc(code: string, expected: Node) {
    l: Lexer;
    init_lexer(&l, code);
    node := parse(&l);

    if l.error {
      fmt.printf("TEST FAILED: %s\ntest failed due to syntax error\n\n", code);
      return;
    }

    env := new_env(nil);
    init_default_env(&env);

    res := eval(&env, node);

    if !node_equal(res, expected) {
      fmt.printf("TEST FAILED: %s\nExpected %, got %\n\n", code, expected, res);
      os.exit(1);
    }
  }

  Num :: proc(n: int) -> Node { return Node{kind=.NUMBER, val=n}};


  test_eval_line("1", Num(1));
  test_eval_line("12345", Num(12345));
  test_eval_line("( + 1 2)", Num(3));
  test_eval_line("(* 2 3)", Num(6));
  test_eval_line("( + 0 ( * 2 3 ) )", Num(6));
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
  test_eval_line("(begin (define x 4) x)", Num(4));
  test_eval_line("(begin (define x 4) (define y 5) (+ x y))", Num(9));
  test_eval_line("(begin (define x 5) (* x x))", Num(25));
  test_eval_line("(begin (define x 5) (define y 5) (define z 5) (+ x (+ y z)))", Num(15));
  test_eval_line("(begin (define x 4) (define y (+ x 7)) (* 7 y))", Num(77));
  test_eval_line("(begin (define x 4) (define y 5) (+ x y))", Num(9));
  test_eval_line("(begin (define x 2) (begin (define x 5) x))", Num(5));
  test_eval_line("(begin (define x 2) (begin (define x 5) x) x)", Num(2));


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