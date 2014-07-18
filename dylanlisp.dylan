Module: dylanlisp

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
    <number> => format-to-string("%d", obj);
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

define function apply1(fn, args)
  if (instance?(fn, <string>))
    fn;
  elseif (instance?(args, <string>))
    args;
  elseif (instance?(fn, <function>))
    fn(args);
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

define function main (name :: <string>, arguments :: <vector>)
  add-to-env(#"t", #"t", g-env);
  add-to-env(#"car", subr-car, g-env);
  add-to-env(#"cdr", subr-cdr, g-env);
  add-to-env(#"cons", subr-cons, g-env);

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
