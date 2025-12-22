Mix.install([
  {:jason, "~> 1.4"},
  {:elixir_xml_to_map, "~> 3.0"}
])

defmodule XmlToJsonConverter do
  @input_dir "input"
  @output_dir "output"

  def run do
    prepare_directories()
    
    @input_dir
    |> File.ls!()
    |> Enum.filter(fn file -> String.ends_with?(file, ".xml") end)
    |> Enum.each(&convert_file/1)
  end

  defp prepare_directories do
    unless File.dir?(@input_dir), do: File.mkdir_p!(@input_dir)
    unless File.dir?(@output_dir), do: File.mkdir_p!(@output_dir)
  end

  defp convert_file(filename) do
    input_path = Path.join(@input_dir, filename)
    output_filename = String.replace(filename, ".xml", ".json")
    output_path = Path.join(@output_dir, output_filename)

    xml_content = File.read!(input_path)
    
    # Преобразование XML в Map, а затем в JSON
    case XmlToMap.naive_map(xml_content) do
      map when is_map(map) ->
        json_content = Jason.encode!(map, pretty: true)
        File.write!(output_path, json_content)
        IO.puts("Успешно: #{filename} -> #{output_filename}")
      _ ->
        IO.puts("Ошибка при обработке файла: #{filename}")
    end
  end
end

XmlToJsonConverter.run()