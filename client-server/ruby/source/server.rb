require 'socket'
require 'json'

class HangmanServer
  WORDS = ['flex', 'hype', 'method']

  def initialize(port)
    @port = port
  end

  def start
    begin
      server = TCPServer.new(@port)
      puts "Hangman server listening on port #{@port}"
      puts "Ready for connections..."
    rescue => e
      puts "Failed to start server: #{e.message}"
      puts "Is port #{@port} already in use?"
      exit 1
    end

    loop do
      begin
        client = server.accept
        puts "New client connected from #{client.peeraddr[2]}:#{client.peeraddr[1]}"
        
        Thread.new(client) do |socket|
          begin
            play_game(socket)
          rescue => e
            puts "Game error: #{e.message}"
          ensure
            socket.close if socket && !socket.closed?
            puts "Client disconnected"
          end
        end
      rescue Interrupt
        puts "\nServer shutting down..."
        break
      rescue => e
        puts "Accept error: #{e.message}"
      end
    end
    
    server.close if server
    puts "Server stopped"
  end

  private

  def play_game(socket)
    word = WORDS.sample
    guessed = '_' * word.length
    mistakes = 0
    max_mistakes = 6
    guessed_letters = []

    puts "Game started. Word: #{word}"

    while mistakes < max_mistakes && guessed.include?('_')
      send_state(socket, guessed, mistakes, max_mistakes, guessed_letters)

      data = socket.gets
      break if data.nil? || data.empty?
      
      letter = data.chomp.downcase.strip
      
      if letter.empty?
        send_error(socket, "Empty input")
        next
      end

      if letter.length != 1
        send_error(socket, "Enter only one letter")
        next
      end

      if guessed_letters.include?(letter)
        send_error(socket, "Letter already guessed")
        next
      end

      guessed_letters << letter

      if word.include?(letter)
        word.chars.each_with_index do |char, index|
          guessed[index] = letter if char == letter
        end
      else
        mistakes += 1
      end
    end

    send_result(socket, word, mistakes < max_mistakes)
  rescue => e
    puts "Game error: #{e.message}"
  end

  def send_state(socket, guessed, mistakes, max_mistakes, guessed_letters)
    state = {
      guessed: guessed,
      mistakes: mistakes,
      max_mistakes: max_mistakes,
      guessed_letters: guessed_letters
    }
    socket.puts(JSON.generate(state))
    socket.flush
  rescue
  end

  def send_error(socket, message)
    error = { error: message }
    socket.puts(JSON.generate(error))
    socket.flush
  rescue
  end

  def send_result(socket, word, won)
    result = {
      result: {
        word: word,
        won: won
      }
    }
    socket.puts(JSON.generate(result))
    socket.flush
  rescue
  end
end

if __FILE__ == $PROGRAM_NAME
  port = 2000
  puts "Starting Hangman server..."
  server = HangmanServer.new(port)
  server.start
end