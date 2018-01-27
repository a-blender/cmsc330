# Synsets, Hypernyms, and CommandParser by Anna Blendermann
require_relative "graph.rb"

class Synsets

# initialize() is the constructor for the Synets class
# creates a new hash to store ids and their corresponding synets

    def initialize
	# create a hash to store synsets
	@hash = Hash.new
    end

# load() loads synset data from a file into the hash
# returns nil, returns an array of invalid lines if the load fails

    def load(synsets_file)

	temp_hash = Hash.new
	invalid_lines = Array.new	
	load_failed = false;
	count = 1

	File.readlines(synsets_file).each do |line|
		parsed_line = line.scan(/^id: (\d+) synset: (\S+)$/)

		if parsed_line.empty?
			invalid_lines.push count
			load_failed = true
		else
			id = parsed_line[0][0].to_i
			synset = parsed_line[0][1].split(",")
			
			if temp_hash.has_key? id
				invalid_lines.push count
				load_failed = true
			else
				temp_hash[id] = synset
			end
		end
		count = count + 1
	end

	if load_failed
		return invalid_lines
	else
		# add ids and synsets to the hashmap, return nil
		temp_hash.each do |key, value|
			addSet(key, value)
		end
		return nil
	end
    end 

# addSet() adds a synset id and corresponding array of nouns to a hash
# returns true, returns false for neg id, empty array, or duplicate id
			
    def addSet(synset_id, nouns)
	
	if (synset_id > -1) && (!nouns.empty?) && (!@hash.has_key? synset_id) 
		@hash[synset_id] = nouns
		return true
	else
		return false	
	end
    end 

# lookup() returns an array of nouns for the synset_id provided
# returns array, returns an empty array if the synset_id was not found

    def lookup(synset_id)
	arr = Array.new
	if @hash.has_key? synset_id
		return @hash[synset_id]
	else
		return arr
	end
    end

# findSynsets() returns either an array of synset ids (for a string) or
# a hash of nouns and corresponding arrays of ids (for an array)

    def findSynsets(to_find)
        result_hash = Hash.new

	if (to_find.class == String)
		return findSynset_helper(to_find)
	elsif (to_find.class == Array)
		to_find.each do |x|
			result_hash[x] = findSynset_helper(x)
		end
		return result_hash	
	else
		return nil
	end
    end

# findSynset_helper() searches through the @hash and finds all the 
# ids that correspond to synsets containing that noun, returns an array
	
    def findSynset_helper(to_find)
	array_ids = Array.new

	@hash.keys.each do |key|
		if @hash[key].include? to_find
			array_ids.push key
		end
	end
	return array_ids
    end

# get_ids() is my added student method that gets the object's synset_ids
# this function is used in CommandParser.load()

    def get_ids
	return @hash.keys
    end

end


class Hypernyms

# initialize() is the constructor for the Hypernyms class
# it creates a graph for synset ids and their relationships

    def initialize
	# create an instance of the graph
	@graph = Graph.new
    end

# load() loads edges between synset ids into a graph and loads the ids
# into graph if they don't exist, returns nil or an array of invalid lines

    def load(hypernyms_file)
	
	temp_hash = Hash.new
	invalid_lines = Array.new
	load_failed = false
	count = 1

	File.readlines(hypernyms_file).each do |line|
		hyp_line = line.scan(/^from: (\d+) to: (\S+)$/)
	
		if hyp_line.empty?
			invalid_lines.push count
			load_failed = true
		else
			from = hyp_line[0][0].to_i
			to = hyp_line[0][1].split(",")
			to_array = to.collect{|x| x.to_i}
	
			if to_array.include? from
				invalid_lines.push count
				load_failed = true
			else	
				temp_hash[from] = to_array
			end
		end
		count = count + 1
	end

	if load_failed == true
		return invalid_lines
	else
		# add edges to the graph (and synset ids if necessary)
		temp_hash.keys.each do |source|
			temp_hash[source].each do |dest|
				addHypernym source,dest
			end
		end
		return nil
	end
    end

# addHypernym() adds a relationship between the source and dest vertices
# returns true if edge was added to the graph, false if not

    def addHypernym(source, destination)
	
	if (source > -1) && (destination > -1) && (source != destination)
		if !@graph.hasVertex? source
			@graph.addVertex source
		end

		if !@graph.hasVertex? destination
			@graph.addVertex destination
		end
		if !@graph.hasEdge? source,destination
			@graph.addEdge source,destination
		end
		return true
	else
		return false
	end
    end

# lca() finds the lowest common ancestors of two synset ids
# returns an array of lcas, empty array, or nil if the ids don't exist

    def lca(id1, id2)
	result_arr = Array.new

	if (!@graph.hasVertex? id1) || (!@graph.hasVertex? id2)
		return nil
	else
		h1 = @graph.bfs(id1)
		h2 = @graph.bfs(id2)
		common_arr = find_common(h1,h2)
		
		if !common_arr.empty?
			min_length = common_arr.values.min
			common_arr.each do |id,length|
				if length == min_length
					result_arr.push id
				end
			end
		end
		return result_arr
	end
    end

# find_common() is a helper method for the lca
# returns a hash of common elements and their lengths, or an empty hash

    def find_common(hash1, hash2)
	common_elements = Hash.new		
	hash1.each do |id,length|
		if hash2.include? id
			common_elements[id] = hash1[id] + hash2[id]
		end
	end
	return common_elements
    end
end


class CommandParser

# initialize() sets up a CommandParser class
# creates local variables for a synsets and hypernyms object

    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

# parse() parses a command and updates the appropriate symbols
# if :recognized_command, then :result should be set to return value/error 

    def parse(command)

	hash = Hash.new
	split = command.split(" ")

	case split[0]
	when "load"
		hash[:recognized_command] = :load
		hash[:result] = parse_load(command)
	when "lookup" 
		hash[:recognized_command] = :lookup
		hash[:result] = parse_lookup(command)
	when "find"
		hash[:recognized_command] = :find
		hash[:result] = parse_find(command)  
	when "findmany"
		hash[:recognized_command] = :findmany
		hash[:result] = parse_findmany(command)
	when "lca"
		hash[:recognized_command] = :lca
		hash[:result] = parse_lca(command)
	else
		hash[:recognized_command] = :invalid	
      	end
	return hash
    end

# parse_load() parses, validates, and executes the load command
# returns true, false for invalid files/ids, :error for invalid format 

    def parse_load(command)

	c_arr = command.scan(/^load (\S+) (\S+)$/)
	if c_arr.empty?
		return :error
	end
	synset_file = c_arr[0][0]
	hypernym_file = c_arr[0][1]

	# 1. check that both files exist
	if (!File.file? synset_file) || (!File.file? hypernym_file)
		return false
	end

	# 2. check the return value of loading into copy objects
	s = Synsets.new
	h = Hypernyms.new
	if (!s.load(synset_file) == nil) && (!h.load(hypernym_file) == nil)
		return false
	end
	
	# 3. check that each hypernym exists in the synset file/object	
	syn_object = @synsets.get_ids
	syn_temp = s.get_ids

	hyp_temp = get_hypernyms(hypernym_file)
	if hyp_temp == nil
		return false
	end
	hyp_temp.each do |id|
		if (!syn_temp.include? id) && (!syn_object.include? id)
			return false
		end
	end
	@synsets.load(synset_file)
	@hypernyms.load(hypernym_file)
	return true	
    end

# get_hypernyms() gets all synset ids from the hypernyms file
# returns an array of ids, or returns nil if any line is invalid

    def get_hypernyms(file)
	arr = Array.new
	File.readlines(file).each do |line|
		ids = line.scan(/^from: (\d+) to: (\S+)$/)
		if ids.empty?
			return nil
		else
			arr.push ids[0][0].to_i
			arr.push ids[0][1].to_i
		end
	end
	return arr
    end

# parse_lookup() parses, validates, and executes the lookup command
# returns result of the lookup, else returns :error for invalid format

    def parse_lookup(command)
	c_arr = command.scan(/^lookup (\d+)$/)
	if c_arr.empty?
		return :error
	end
	result = @synsets.lookup(c_arr[0][0].to_i)
	return result
    end

# parse_find() parses, validates, and executes the find command
# returns result of find, else returns :error for invalid format

    def parse_find(command)
	c_arr = command.scan(/^find (\S+)$/)
	if c_arr.empty?
		return :error
	end
	result = @synsets.findSynsets(c_arr[0][0])
	return result
    end

# parse_findmany() does batch processing for the find command
# returns the result of find(arr), or :error for invalid format/args

    def parse_findmany(command)
	c_arr = command.scan(/^findmany (\S+),*$/)
	if c_arr.empty?
		return :error
	end
	nouns = c_arr[0][0].split(",")
	result = @synsets.findSynsets(nouns)
	return result
    end

# parse_lca() parses, validates, and executes the lca command
# returns the result of lca (no lca possible), :error for invalid format

    def parse_lca(command)
	c_arr = command.scan(/^lca (\d+) (\d+)$/)

	# c_arr will return empty if one/both ids are not ints
	if c_arr.empty?
		return :error
	end
	id1 = c_arr[0][0].to_i
	id2 = c_arr[0][1].to_i
	result = @hypernyms.lca(id1,id2)
	return result
    end
end

