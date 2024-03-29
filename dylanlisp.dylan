Module: dylanlisp

// <empty-list> -> nil
// <number> -> num
// <symbol> -> sym
// <string> -> error
// <pair> -> cons
// <function> -> subr
// <vector> -> expr

define constant kLPar = '(';
define constant kRPar = ')';
define constant kQuote = '\'';

define function safe-cxr(fn, obj)
  if (instance?(obj, <pair>))
    fn(obj);
  else
    #();
  end;
end;

define function safe-car(obj)
  safe-cxr(head, obj);
end;

define function safe-cdr(obj)
  safe-cxr(tail, obj);
end;

define function make-expr(args, env)
  vector(safe-car(args), safe-cdr(args), env);
end;

define function pairlis(lst1, lst2)
  local method rec(lst1, lst2, acc)
    if (instance?(lst1, <pair>) & instance?(lst2, <pair>))
      rec(tail(lst1), tail(lst2), pair(pair(head(lst1), head(lst2)), acc));
    else
      reverse!(acc);
    end;
  end;
  rec(lst1, lst2, #());
end;

define function space?(c)
  c = '\t' | c = '\r' | c = '\n' | c = ' ';
end;

define function delimiter?(c)
  c = kLPar | c = kRPar | c = kQuote | space?(c);
end;

define function skip-spaces(str)
  block (return)
   for (i from 0 below size(str))
      if (~space?(aref(str, i)))
        return(copy-sequence(str, start: i));
      end;
   end;
  ""
  end;
end;

define function make-num-or-sym(str)
  let num = string-to-integer(str, default: 0);
  if (integer-to-string(num) = str)
    num;
  else
    as(<symbol>, str);
  end;
end;

define function read-atom(str)
  let next = "";
  block (break)
    for (i from 0 below size(str))
      if (delimiter?(aref(str, i)))
        next := copy-sequence(str, start: i);
        str := copy-sequence(str, start: 0, end: i);
        break();
      end;
    end;
  end;
  values(make-num-or-sym(str), next)
end;

define function read1(str)
  str := skip-spaces(str);
  if (str = "")
    values("empty input", "");
  elseif (first(str) = kRPar)
    values(concatenate("invalid syntax: ", str), "");
  elseif (first(str) = kLPar)
    read-list(copy-sequence(str, start: 1));
  elseif (first(str) = kQuote)
    let (elm, next) = read1(copy-sequence(str, start: 1));
    values(pair(#"quote", pair(elm, #())), next);
  else
    read-atom(str);
  end;
end;

define function read-list(str)
  let ret = #();
  block (break)
    while (#t)
      str := skip-spaces(str);
      if (str = "")
        ret := "unfinished parenthesis";
        break();
      elseif (first(str) = kRPar)
        ret := reverse!(ret);
        str := copy-sequence(str, start: 1);
        break();
      else
        let (elm, next) = read1(str);
        if (instance?(elm, <string>))
          ret := elm;
          str := next;
          break();
        else
          ret := pair(elm, ret);
          str := next;
        end
      end;
    end;
  end;
  values(ret, str);
end;

define function print-obj(obj)
  select (obj by instance?)
    <empty-list> => "nil";
    <number> => integer-to-string(obj);
    <symbol> => as(<string>, obj);
    <string> => concatenate("<error: ", obj, ">");
    <pair> => print-list(obj);
    <function> => "<subr>";
    <vector> => "<expr>";
  end;
end;

define function print-list(obj)
  let ret = "";
  let first = #t;
  while (instance?(obj, <pair>))
    if (first)
      first := #f;
    else
      ret := concatenate(ret, " ");
    end;
    ret := concatenate(ret, print-obj(head(obj)));
    obj := tail(obj);
  end;
  if (obj == #())
    concatenate("(", ret, ")");
  else
    concatenate("(", ret, " . ", print-obj(obj), ")");
  end;
end;

define function find-var(sym, env)
  local method rec(alist)
    if (alist == #())
      #();
    elseif (head(head(alist)) == sym)
      head(alist);
    else
      rec(tail(alist));
    end
  end;
  if (env == #())
    #();
  else
    let result = rec(head(env));
    if (result == #())
      find-var(sym, tail(env));
    else
      result;
    end;
  end;
end;

define variable g-env = pair(#(), #());

define function add-to-env(sym, val, env)
  env.head := pair(pair(sym, val), head(env));
end;

define function eval(obj, env)
  select (obj by instance?)
    <empty-list> => obj;
    <number> => obj;
    <string> => obj;
    <symbol> =>
      let bind = find-var(obj, env);
      if (bind == #())
        concatenate(print-obj(obj), " has no value");
      else
        tail(bind);
      end;
    otherwise =>
      let op = safe-car(obj);
      let args = safe-cdr(obj);
      if (op == #"quote")
        safe-car(args);
      elseif (op == #"if")
        let c = eval(safe-car(args), env);
        if (instance?(c, <string>))
          c
        elseif (c == #())
          eval(safe-car(safe-cdr(safe-cdr(args))), env);
        else
          eval(safe-car(safe-cdr(args)), env);
        end;
      elseif (op == #"lambda")
        make-expr(args, env);
      elseif (op == #"defun")
        let expr = make-expr(safe-cdr(args), env);
        let sym = safe-car(args);
        add-to-env(sym, expr, g-env);
        sym;
      elseif (op == #"setq")
        let val = eval(safe-car(safe-cdr(args)), env);
        let sym = safe-car(args);
        let bind = find-var(sym, env);
        if (bind == #())
          add-to-env(sym, val, g-env);
        else
          bind.tail := val;
        end;
        val;
      else
        apply1(eval(op, env), evlis(args, env));
      end;
  end;
end;

define function evlis(lst, env)
  local method rec(lst, acc)
    if (lst == #())
      reverse!(acc);
    else
      let elm = eval(head(lst), env);
      if (instance?(elm, <string>))
        elm
      else
        rec(tail(lst), pair(elm, acc));
      end;
    end;
  end;
  rec(lst, #());
end;

define function progn(body, env)
  let ret = #();
  while (instance?(body, <pair>))
    ret := eval(head(body), env);
    body := tail(body);
  end;
  ret;
end;

define function apply1(fn, args)
  if (instance?(fn, <string>))
    fn;
  elseif (instance?(args, <string>))
    args;
  elseif (instance?(fn, <function>))
    fn(args);
  elseif (instance?(fn, <vector>))
    progn(second(fn), pair(pairlis(first(fn), args), third(fn)));
  else
    concatenate(print-obj(fn), " is not function");
  end;
end;

define function subr-car(args)
  safe-car(safe-car(args));
end;

define function subr-cdr(args)
  safe-cdr(safe-car(args));
end;

define function subr-cons(args)
  pair(safe-car(args), safe-car(safe-cdr(args)));
end;

define function subr-eq(args)
  let x = safe-car(args);
  let y = safe-car(safe-cdr(args));
  if (instance?(x, <number>) & instance?(y, <number>))
    if (x = y)
      #"t";
    else
      #();
    end;
  elseif (x == y)
    #"t";
  else
    #();
  end;
end;

define function subr-atom(args)
  if (instance?(safe-car(args), <pair>))
    #();
  else
    #"t";
  end;
end;

define function subr-numberp(args)
  if (instance?(safe-car(args), <number>))
    #"t";
  else
    #();
  end;
end;

define function subr-symbolp(args)
  if (instance?(safe-car(args), <symbol>))
    #"t";
  else
    #();
  end;
end;

define function subr-add-or-mul(fn, init-val)
  method (args)
    let ret = init-val;
    while (instance?(args, <pair>))
      if (~instance?(safe-car(args), <number>))
        ret := "wrong type";
        args := #();  // break
      else
        ret := fn(ret, safe-car(args));
        args := tail(args);
      end;
    end;
    ret;
  end;
end;

define function subr-sub-or-div-or-mod(fn)
  method (args)
    let x = safe-car(args);
    let y = safe-car(safe-cdr(args));
    if (~instance?(x, <number>) | ~instance?(y, <number>))
      "wrong type"
    else
      fn(x, y);
    end;
  end;
end;

define function main (name :: <string>, arguments :: <vector>)
  add-to-env(#"t", #"t", g-env);
  add-to-env(#"car", subr-car, g-env);
  add-to-env(#"cdr", subr-cdr, g-env);
  add-to-env(#"cons", subr-cons, g-env);
  add-to-env(#"eq", subr-eq, g-env);
  add-to-env(#"atom", subr-atom, g-env);
  add-to-env(#"numberp", subr-numberp, g-env);
  add-to-env(#"symbolp", subr-symbolp, g-env);
  add-to-env(#"+", subr-add-or-mul(method(x, y) x + y end, 0), g-env);
  add-to-env(#"*", subr-add-or-mul(method(x, y) x * y end, 1), g-env);
  add-to-env(#"-", subr-sub-or-div-or-mod(method(x, y) x - y end), g-env);
  add-to-env(#"/", subr-sub-or-div-or-mod(truncate/), g-env);
  add-to-env(#"mod", subr-sub-or-div-or-mod(modulo), g-env);

  let line = "";
  format-out("> ");
  force-output(*standard-output*);
  while (line := read-line(*standard-input*, on-end-of-stream: #f))
    let (elm, _) = read1(line);
    format-out(print-obj(eval(elm, g-env)));
    format-out("\n> ");
    force-output(*standard-output*);
  end;
  exit-application(0);
end;

main(application-name(), application-arguments());
