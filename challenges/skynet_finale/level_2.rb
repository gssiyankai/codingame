STDOUT.sync = true # DO NOT REMOVE

MAX_DISTANCE = 10000

def connected_node node, link
    if link[0]==node then link[1] else link[0] end
end

def distance_cache si, gateway, links, cache
    d = cache[si]
    if links.include?([si, gateway]) or links.include?([gateway, si])
        d + 1
    else
        next_sis = links.select { |link| link.include? si }
                        .map { |link| connected_node si, link }
                        .select { |next_si| (cache[next_si]||MAX_DISTANCE) > (d + 1) }
        next_sis.each { |next_si|
            cache[next_si] = d + 1
        }
        next_sis.map { |next_si| distance_cache next_si, gateway, links, cache }
                .min || MAX_DISTANCE
    end
end

def distance si, gateway, links
    distance_cache si, gateway, links, { si => 0 }
end

def gateway_links gateway, links
    links.select { |link| link.include? gateway }
end

# Read init information from standard input, if any
n, l, e = STDIN.gets.split(" ").map(&:to_i)

links = (1..l).map {
            STDIN.gets.split(" ").map(&:to_i)
        }

gateways = (1..e).map {
                STDIN.gets.to_i
            }

loop do
    # Read information from standard input
    si = STDIN.gets.to_i

    # Compute logic here
    targets = gateways.map { |gateway|
                            gateway_links = gateway_links gateway, links
                            gateway_links.map { |gateway_link|
                                [gateway, gateway_link, distance(si, gateway, links-gateway_links+[gateway_link])]
                            }
                        }.flatten(1)
    clusters = targets.reduce({}) { |acc, target|
                    node = connected_node(target[0], target[1])
                    n = acc[node]
                    if n.nil?
                        n = 1
                    else
                        n = n + 1
                    end
                    acc[node] = n
                    acc
                }
    
    targets = targets.map { |gateway, gateway_link, distance|
                    node = connected_node(gateway, gateway_link)
                    n = clusters[node]
                    if n.nil?
                        n = 0
                    end
                    distance = distance - n
                    [gateway, gateway_link, distance]
                }.sort_by(&:last)
    target = targets[0][1]
    links = links - [target]
    
    # STDERR.puts "Debug messages..."

    # Write action to standard output
    puts target.join(" ")
end