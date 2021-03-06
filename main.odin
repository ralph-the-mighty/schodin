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
  source_pos: int,

  //lambda fields
  args: [dynamic]Node,
  body: [dynamic]Node,
  env: Env,
};


new_int_node :: proc(val: int) -> Node {
  return Node{kind=.NUMBER, val=val};
}

new_sym_node :: proc(sym: string) -> Node {
  return Node{kind=.SYMBOL, sym=sym};
}

new_lambda_node :: proc(env: Env, args, body: Node) -> Node {
  return Node{kind=.LAMBDA, env=env, args=args.list, body=body.list};
}

new_list_node :: proc(list: ..Node) -> Node {
  node := Node{kind=.LIST};
  for n in list {
    append_elem(&node.list, n);
  }
  return node;
}

err_node :: proc (node: Node) -> Node {
  return Node{kind=.ERROR, source_pos = node.source_pos};
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
      fmt.printf("Symbol '%s' does not exist in the environment\n", sym);
      os.exit(1);
    }
  }
}





print_node :: proc(node: Node) {
  switch node.kind {
    case .SYMBOL:
      fmt.printf("<SYMBOL: %s>", node.sym);
    case .NUMBER:
      fmt.printf("<NUMBER: %d>", node.val);
    case .LIST:
      fmt.print("<LIST: [");
      for n in node.list {
        print_node(n);
        fmt.print(", ");
      }
      fmt.print("]>");
    case .PROC:
      fmt.printf("<PROC>");
    case .LAMBDA:
      fmt.printf("<LAMBDA: (");
      for arg_node in node.args {
        fmt.printf("%s, ", arg_node.sym);
      }
      fmt.printf(")>");
    case .ERROR:
      fmt.printf("<ERROR>");
  }
}

print_env :: proc(env: ^Env) {
  for key, value in env._map {
    fmt.printf("%s -> ", key);
    print_node(value);
    fmt.println();
  }
  if env.outer != nil {
    print_env(env.outer);
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
          arg := eval(env, tail[1]);
          insert(env, tail[0].sym, arg);
          return arg;
        } else if head.sym == "begin" { //(begin (define x 4) x)
          ret_node : Node;
          for n in tail {
            ret_node = eval(env, n);
          }
          return ret_node;
        } else if head.sym == "lambda" {
          return new_lambda_node(env^, tail[0], tail[1]);
        }
      }
      
      //none of the other cases returned.  Time for evaluation!!

      

      proc_node := eval(env, head); //TODO check actual local env
      assert(proc_node.kind == .PROC || proc_node.kind == .LAMBDA, "first item in list must eval to a builtin proc or a lambda");

      //evaluate all the arguments first, then pass them to the proc
      evaluated_args : [dynamic]Node;
      for arg in tail {
        append_elem(&evaluated_args, eval(env, arg));
      }

      if proc_node.kind == .PROC {
        return proc_node.procedure(evaluated_args[:]);
      } else {
        assert(proc_node.kind == .LAMBDA);

        
        // fmt.printf("Evaluating lambda ");
        // print_node(node);
        // fmt.printf(" in this environment:");
        // print_env(env);


        if len(tail) != len(proc_node.args) {
          fmt.printf("error calling %s:\n",  head.sym);
          fmt.printf("expected %d args, got %d\n", len(proc_node.args), len(tail));
          assert(false);
          return err_node(node);
        }
        lambda_env := new_env(&proc_node.env);


        for arg_name_node, i in proc_node.args {
          insert(&lambda_env, arg_name_node.sym, evaluated_args[i]); 
        }
        return eval(&lambda_env, Node{kind=.LIST, list=proc_node.body}); //HORRIBLE HACK. SWITCH TO POINTERS OR SOMETHING TO STORE PROC ARGS AND BODY
      }
    case .LAMBDA:
      assert(false, "lambda's can't be evaluated");
    case .PROC:
      assert(false, "procs can't be evaluated");

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


  test_eval_line :: proc(source: string, expected: Node) {
    l: Lexer;
    init_lexer(&l, source);
    node := parse(&l);

    if l.error {
      fmt.printf("TEST FAILED: %s\ntest failed due to syntax error\n\n", source);
      return;
    }

    env := new_env(nil);
    init_default_env(&env);

    res := eval(&env, node);

    if !node_equal(res, expected) {
      fmt.printf("TEST FAILED: %s\nExpected %, got %\n\n", source, expected, res);
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
  test_eval_line("(begin (define x 2) (begin (define x 5) x) x)", Num(5));


  test_eval_line("(begin (define add1 (lambda (x) (+ x 1)))     (add1 2)   )", Num(3));


}





load_and_run :: proc(path: string) -> (Node, bool) {
  source, ok := os.read_entire_file(path);
  if !ok {
    fmt.printf("Could not load %s\n");
    return {}, false;
  }

  l: Lexer;
  init_lexer(&l, string(source));
  node := parse(&l);

  if l.error {
    return {}, false;
  }


    env := new_env(nil);
    init_default_env(&env);

    return eval(&env, node), true;
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
  //test();

  res, ok := load_and_run("test.scm");

  if ok {
    fmt.println(res);
  } else {
    fmt.println("NOT OKAY");
  }

}