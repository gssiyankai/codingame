STDOUT.sync = true # DO NOT REMOVE

# Read init information from standard input, if any
n, l, e = STDIN.gets.split(" ").map(&:to_i)

links = (1..l).map {
            STDIN.gets.split(" ").map(&:to_i)
        }

gateways = (1..e).map {
                STDIN.gets.to_i
            }
            
links = links.select { |link|
            gateways.include?(link[0]) or gateways.include?(link[1])
        }

loop do
    # Read information from standard input
    si = STDIN.gets.to_i

    # Compute logic here
    target = links.select { |link| link.include? si }
                  .first
    
    if target.nil?
        target = links.pop
    end

    # STDERR.puts "Debug messages..."

    # Write action to standard output
    puts target.join(" ")
end
