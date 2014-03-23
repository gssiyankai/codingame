require 'set'

STDOUT.sync = true # DO NOT REMOVE

$MOVES =  {[0,  1] => "RIGHT",
           [0, -1] => "LEFT",
           [-1, 0] => "UP",
           [1,  0] => "DOWN"}

def next_position current_position, delta
    current_position.zip(delta).map { |x,y|
                    x + y
                }
end

def next_move current_position, next_position
  $MOVES[current_position.zip(next_position).map { |x,y|
    y - x
  }]
end

def opposite_move move
  dx, dy = $MOVES.invert[move]
  $MOVES[[-dx, -dy]]
end

def possible_moves_to_cells moves, k, maze, visited_maze, visited
    moves.select { |delta, move|
        n_x, n_y = next_position(k, delta)
        visited_maze[n_x][n_y] == visited && maze[n_x][n_y]!='#'
    }.map(&:last)
end

def possible_moves k, maze, visited_maze, previous_move
    opposite_move = opposite_move previous_move
    moves = $MOVES.select { |delta, move|
                        move==previous_move
                   }.to_a +
            $MOVES.reject { |delta, move|
                        move==previous_move || move==opposite_move
                   }.to_a +
            $MOVES.select { |delta, move|
                        move==opposite_move
                   }.to_a
    [false, true].map { |visited|
        possible_moves_to_cells(moves, k, maze, visited_maze, visited)
    }.flatten
end



class Distance

  def self.manathan p1, p2
    p1.zip(p2).map { |c1, c2| (c1 - c2).abs }
    .reduce(:+)
  end

end

class A_star

  @@moves = { :left       => [ 0,-1],
              :right      => [ 0, 1],
              :up         => [-1, 0],
              :down       => [ 1, 0],
              :up_left    => [-1,-1],
              :up_right   => [-1, 1],
              :down_left  => [ 1,-1],
              :down_right => [ 1, 1] }


  def self.valid_neighbours map, possible_moves, movement_cost, position
    height, width = [map.size, map[0].size]
    x,y = position
    delta_moves = possible_moves.map { |move| @@moves[move] }
    delta_moves.map { |delta_x, delta_y| [x+delta_x, y+delta_y] }
    .select { |n_x, n_y|
      n_x>=0 && n_y>=0 && n_x<height && n_y<width && movement_cost.include?(map[n_x][n_y])
    }
  end

  def self.find_position map, tile
    map.each_with_index.map { |row, i| [i, row.index(tile)] }
    .select { |x, y| not y.nil? }
    .flatten
  end

  def self.heuristic_cost p1, p2
    Distance.manathan p1, p2
  end

  def self.find_path map, start, goal, possible_moves, movement_cost
    start_position, goal_position = [start, goal].map { |tile| find_position(map, tile) }
    evaluated_nodes = Set.new
    opened_nodes = Set.new [start_position]
    navigated_nodes = Hash.new
    g_score = { start_position => 0 }
    f_score = { start_position => heuristic_cost(start_position, goal_position) }

    while not opened_nodes.empty?
      current = opened_nodes.map { |node| [f_score[node], node] }.min.last
      if current == goal_position
        return reconstruct_path navigated_nodes, goal_position
      end

      opened_nodes.delete current
      evaluated_nodes.add current

      valid_neighbours(map, possible_moves, movement_cost, current).reject { |n| evaluated_nodes.include? n }
      .each { |n|
        n_x, n_y = n
        tentative_g_score = g_score[current] + movement_cost[map[n_x][n_y]]
        if not opened_nodes.include?(n) or tentative_g_score < g_score[n]
          navigated_nodes[n] = current
          g_score[n] = tentative_g_score
          f_score[n] = g_score[n] + heuristic_cost(n, goal_position)
          if not opened_nodes.include?(n)
            opened_nodes.add n
          end
        end
      }
    end

    raise "No path found"
  end

  private
  def self.reconstruct_path navigated_nodes, current_node
    previous_node = navigated_nodes[current_node]
    if previous_node.nil?
      [current_node]
    else
      reconstruct_path(navigated_nodes, previous_node) + [current_node]
    end
  end

end


# Read init information from standard input, if any
r, c, a = STDIN.gets.split(" ").map(&:to_i)
visited_cells = r.times.map {
                    c.times.map {
                      false
                    }
                  }

mode = :explore
$action = "RIGHT"

loop do
    # Read information from standard input
    k = STDIN.gets.split(" ").map(&:to_i)
    k_x, k_y = k
    visited_cells[k_x][k_y] = true
    maze = r.times.map { STDIN.gets }

    # Compute logic here
    if mode == :explore
      control_room = A_star.find_position maze, 'C'
      if not control_room.empty?
        begin
          movement_cost = {'C' => 1, 'T' => 1, '.' => 1, '+' => 1}
          possible_moves = [:left, :right, :up, :down]
          maze[k_x][k_y] = '+'
          path = A_star.find_path maze, '+', 'C', possible_moves, movement_cost
          back_path = A_star.find_path maze, 'C', 'T', possible_moves, movement_cost
          $path = path[1..-1] + back_path[1..-1]
          mode = :follow
        rescue
        end
      end
    end
    
    case mode
        when :explore
            possible_moves = possible_moves(k, maze, visited_cells, $action)
            $action = possible_moves.first
        when :follow
            next_position = $path.delete_at(0)
            $action = next_move k, next_position
    end
            
    
    # STDERR.puts "Debug messages..."

    # Write action to standard output
    puts $action
end 
