require 'socket'
require 'json'

class HangmanClient
  def initialize(host, port)
    @host = host
    @port = port
  end

  def start
    socket = TCPSocket.new(@host, @port)

    begin
      loop do
        response = socket.gets
        break unless response

        data = JSON.parse(response.chomp)

        if data.key?('error')
          puts "Error: #{data['error']}"
          next
        end

        if data.key?('result')
          show_result(data['result'])
          break
        end

        show_state(data)
        print "Enter letter: "
        letter = gets.chomp.downcase
        socket.puts(letter)
      end
    rescue => e
      puts "Connection error: #{e.message}"
    ensure
      socket.close
    end
  end

  private

  def show_state(data)
    puts "\nWord: #{data['guessed'].chars.join(' ')}"
    puts "Mistakes: #{data['mistakes']}/#{data['max_mistakes']}"
    puts "Guessed letters: #{data['guessed_letters'].join(', ')}"
    draw_hangman(data['mistakes'])
  end

  def show_result(result)
    puts "\nGame over!"
    puts "Word was: #{result['word']}"
    puts result['won'] ? "You won!" : "You lost!"
  end

  def draw_hangman(mistakes)
    stages = [
      "  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n      |\n      |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n  |   |\n      |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n /|   |\n      |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n      |\n=========",
      "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n      |\n========="
    ]
    puts stages[mistakes]
  end
end

client = HangmanClient.new('ruby-server', 2000)
client.start