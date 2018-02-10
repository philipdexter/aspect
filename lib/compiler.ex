defmodule Aspect.Compiler do

  def compile(file) do

    IO.puts "Compiling #{file}"

    file
    |> File.read!
    |> :erlang.binary_to_list
    |> :string.tokens(' \n')
    |> compile_forms
  end

  def load(forms) do
    case :compile.forms(forms, [:return_errors, :return_warnings]) do
      {:ok, mod, binary, _warnings} -> :code.load_binary(mod, 'hi.as', binary)
    end
  end

  def cl(file) do
    load compile(file)
  end

  def compile_forms(ast) do
    [{:attribute,1,:file,{"hi.as",1}},
     {:attribute,1,:module,:hi},
     {:attribute,2,:compile,:export_all},
     compile_forms(ast, []),
     {:eof,7}]
  end
  def compile_forms(['swap'|ast], [a,b|stack]) do
    compile_forms(ast, [b,a|stack])
  end
  def compile_forms(['+'|ast], [a,b|stack]) do
    [{:op,6,:+,{:var,6,a},{:var,6,b}}]
    ++ compile_forms(ast, stack)
  end
  def compile_forms([':',func_name|ast], stack) do
    {arg_count, ast_body} = parse_effect(ast)
    arg_vars = create_vars(arg_count)
    {:function,5,:erlang.list_to_atom(func_name),arg_count,
     [{:clause,5,
       Enum.map(arg_vars, fn var -> {:var, 5, var} end),
       [],
       compile_forms(ast_body, arg_vars++stack)}]}
  end
  def compile_forms(['drop'|ast], [_a|stack]) do
    compile_forms(ast, stack)
  end
  def compile_forms(['parse-token',token|ast], stack) do
    compile_forms(ast, [token|stack])
  end
  def compile_forms([';'], []) do
    []
  end

  defp parse_effect(['('|ast]) do
    parse_effect(ast)
  end
  defp parse_effect([')'|ast]), do: {0, ast}
  defp parse_effect(['--'|ast]) do
    {_, next} = parse_effect(ast)
    {0, next}
  end
  defp parse_effect([_|ast]) do
    {count, next} = parse_effect(ast)
    {count+1, next}
  end

  defp create_vars(0), do: []
  defp create_vars(n), do: [:erlang.list_to_atom('X' ++ :erlang.integer_to_list(n))|create_vars(n-1)]

end
