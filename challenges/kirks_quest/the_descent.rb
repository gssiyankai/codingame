STDOUT.sync = true # DO NOT REMOVE

# Read init information from standard input, if any

loop do
    # Read information from standard input
    s = STDIN.gets.split(" ").map(&:to_i)
    ms = (0..7).map { |i|
                    [i, STDIN.gets.to_i]
                }

    # Compute logic here
    hm = ms.max_by(&:last)
    action = "HOLD"
    if hm[0]==s[0]
        action = "FIRE"
    end

    # STDERR.puts "Debug messages..."

    # Write action to standard output
    puts action
end
