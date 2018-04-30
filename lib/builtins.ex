defmodule Aspect.Compiler.Builtins do
  import Aspect.Compiler

  def plus(ast, [a, b | stack], ctx) do
    {[x], ctxx} = fresh(1, ctx)
    {[match(var(x), {:op, 6, :+, var(b), var(a)})], ast, [x | stack], ctxx}
  end

  def minus(ast, [a, b | stack], ctx) do
    {[x], ctxx} = fresh(1, ctx)
    {[match(var(x), {:op, 6, :-, var(b), var(a)})], ast, [x | stack], ctxx}
  end

  def swap(ast, [a, b | stack], ctx) do
    {[], ast, [b, a | stack], ctx}
  end

  def drop(ast, [_ | stack], ctx) do
    {[], ast, stack, ctx}
  end

  def dup(ast, [a | stack], ctx) do
    {[], ast, [a, a | stack], ctx}
  end

  def set_module([mod_name | ast], stack, ctx) do
    {[], ast, stack, %Aspect.Compiler.Ctx{ctx | module_name: mod_name}}
  end

  def parse_token([token | ast], stack, ctx) do
    {[], ast, [token | stack], ctx}
  end

  def pp(ast, [x | stack], ctx) do
    {[
       mfa_call(:io, :format, [
         {:string, 15, [126, 112, 126, 110]},
         {:cons, 15, var(x), {nil, 15}}
       ])
     ], ast, stack, ctx}
  end

  def quot(ast, stack, ctx) do
    {quot_r, ast_rest} = parse_quotation(ast, [])
    quot = Enum.reverse(quot_r)
    {[], ast_rest, [quot | stack], ctx}
  end


  def if(ast, [fc, tc, b | stack], ctx) do
    # TODO consider changing this to match on :false
    # and then everything else is considered true

    # TODO assuming the branches don't take any args, bad!!!
    # need way to tell statically how many they need, then
    # give them that much

    # TODO assuming the branches all return one arg, bad!!!

    {tc_code, tc_ret_args, ctxx} = gen_code(tc, [], ctx)
    {fc_code, fc_ret_args, ctxxx} = gen_code(fc, [], ctxx)

    1 = length(tc_ret_args)
    1 = length(fc_ret_args)

    tc_full_code =
    [{:call, 1,
      {:fun, 1, {:clauses, [{:clause, 1, [], [],
                             case tc_ret_args do
                               [] -> tc_code
                               [v] -> tc_code ++ [var(v)]
                               s -> tc_code ++ [tuple(Enum.map(s, &var/1))]
                             end}]}}, []}]

    fc_full_code =
      [{:call, 1,
        {:fun, 1, {:clauses, [{:clause, 1, [], [],
                               case fc_ret_args do
                                 [] -> fc_code
                                 [v] -> fc_code ++ [var(v)]
                                 s -> fc_code ++ [tuple(Enum.map(s, &var/1))]
                               end}]}}, []}]

    {[ret_var], ctxxxx} = fresh(1, ctxxx)

    code =
      match(var(ret_var),
        {:case, 1, var(b), [{:clause, 1, [{:atom, 1, :true}], [], tc_full_code},
                            {:clause, 1, [{:atom, 1, :false}], [], fc_full_code}]})
    {[code], ast, [ret_var | stack], ctxxxx}
  end

  def call(ast, [quot | stack], ctx) do
    {num_args, num_ret, ast_rest} = parse_effect(["(" | ast])

    {arg_vars, ctxx} = fresh(num_args, ctx)

    {args_for_call, stack_rest} = Enum.split(stack, num_args)

    {code, ret_args, ctxxx} = gen_code(quot, arg_vars, ctxx)

    full_code =
      case ret_args do
        [] -> code
        [v] -> code ++ [var(v)]
        s -> code ++ [tuple(Enum.map(s, &var/1))]
      end

    # assert the declared return stack effect is the same
    # as the number of values left on the stack
    ^num_ret = length(ret_args)

    # todo quots that return more than 1
    # todo can look at the effect of the called quot
    # for now let's assume it returns either 0 or 1 val
    call =
      {:call, 12,
       {:fun, 12, {:clauses, [{:clause, 12, Enum.map(arg_vars, &var/1), [], full_code}]}},
       Enum.map(args_for_call, &var/1)}

    {code_, stack_, ctx_} =
      case ret_args do
        [] ->
          {call, stack_rest, ctxxx}

        [_] ->
          {[v], c} = fresh(1, ctxxx)
          {match(var(v), call), [v | stack_rest], c}

        _ ->
          {vs, c} = fresh(length(ret_args), ctxxx)
          {match(tuple(Enum.map(vs, &var/1)), call), vs ++ stack_rest, c}
      end

    {[code_], ast_rest, stack_, ctx_}
  end

  def colon([func_name | ast], stack, ctx) do
    {arg_count, ret_count, ast_body} = parse_effect(ast)
    {arg_vars, ctxx} = fresh(arg_count, ctx)
    {body_r, ast_rest} = parse_body(ast_body, [])
    body = Enum.reverse(body_r)

    {code, stack_rest, ctxxx} = gen_code(body, arg_vars, ctxx)

    full_code = case stack_rest do
                  [] -> code
                  [v] -> code ++ [var(v)]
                  s -> code ++ [tuple(Enum.map(s, &var/1))]
                end

    full_code = case full_code do
                  [] -> [{:atom, 1, :ok}]
                  _ -> full_code
                end

    # assert the declared return stack effect is the same
    # as the number of values left on the stack
    ^ret_count = length(stack_rest)

    ctxxxx = define_with_effect(func_name, arg_count, ret_count, ctxxx)

    {[
       {:function, 5, String.to_atom(func_name), arg_count,
        [{:clause, 5, Enum.map(arg_vars, &var/1), [], full_code}]}
     ], ast_rest, stack, ctxxxx}
  end

  def parse_body([";" | ast], stack) do
    {stack, ast}
  end

  def parse_body([token | ast], stack) do
    parse_body(ast, [token | stack])
  end

  def parse_quotation(["]" | ast], stack) do
    {stack, ast}
  end

  def parse_quotation([token | ast], stack) do
    parse_quotation(ast, [token | stack])
  end

  def parse_effect(ast) do
    {["(" | front], ["--" | rest]} = Enum.split_while(ast, fn x -> x != "--" end)
    {back, [")" | next]} = Enum.split_while(rest, fn x -> x != ")" end)
    {length(front), length(back), next}
  end
end
