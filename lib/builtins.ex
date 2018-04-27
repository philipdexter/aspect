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

  def colon([func_name | ast], stack, ctx) do
    {arg_count, ret_count, ast_body} = Aspect.Compiler.parse_effect(ast)
    {arg_vars, ctxx} = fresh(arg_count, ctx)
    {body_r, ast_rest} = parse_body(ast_body, [])
    body = Enum.reverse(body_r)

    f = fn
      _, code, [], stack, ctx ->
        {code, stack, ctx}

      f, code, ast, stack, ctx ->
        case compile_forms(ast, stack, ctx) do
          {code_, [], [], ctx_} ->
            {code ++ code_, [], ctx_}

          {code_, ast_, stack_, ctx_} ->
            f.(f, code ++ code_, ast_, stack_, ctx_)
        end
    end

    {code, stack_rest, ctxxx} = f.(f, [], body, arg_vars, ctxx)

    full_code = case stack_rest do
                  [] -> code
                  [v] -> code ++ [var(v)]
                  s -> code ++ [tuple(Enum.map(s, &var/1))]
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
end