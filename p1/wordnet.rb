require_relative "graph.rb"

# PUT COMMENT FOR SYNSETS CLASS HERE

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

	temp_array = Array.new
	invalid_lines = Array.new	
	load_failed = false;
	count = 1

	File.readlines(synsets_file).each do |line|
		parsed_line = line.scan(/^id: (\d+) synset: (\S+)$/)

		if (parsed_line.empty?)
			invalid_lines.push count
			load_failed = true
		else
			temp_array.push parsed_line
		end
		count = count + 1
	end

	if (load_failed)
		return invalid_lines
	else
		# pull ids and synsets from the array of parsed lines
		# add those to the hashmap, then return nil
		temp_array.each do |line|
			id = line[0][0].to_i
			synset = line[0][1].split(",")
			@hash[id] = synset
		end
	end
	puts @hash
    end 

# addSet() adds a synset id and corresponding array of nouns to a hash
# returns true, returns false for neg id, empty array, or duplicate id
			
    def addSet(synset_id, nouns)
	
	if (synset_id > 0) && (!nouns.empty?) && (!@hash.has_key? synset_id) 
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

	if (@hash.has_key? synset_id)
		return @hash[synset_id]
	else
		return arr
	end
    end

# findSynsets() does...
# blah blah blah

    def findSynsets(to_find)
        
	if (to_find.class == String)
		# iterate through the hash and find every synset 
		# that contains the noun "to_find," return an array of ids

	else if (to_find.class == Array)
		# call findSynsets() on every noun in the received array
		# and find some way of storing the results in a hash
	end	
    end
end

# PUT COMMENT FOR HYPERNYMS CLASS HERE

class Hypernyms
    def initialize
	# create an instance of the graiph
	@graph = Graph.new
    end

    def load(hypernyms_file)
        raise Exception, "Not implemented"
    end

    def addHypernym(source, destination)
        raise Exception, "Not implemented"
    end

    def lca(id1, id2)
        raise Exception, "Not implemented"
    end
end

# PUT COMMENT FOR COMMAND PARSER CLASS HERE

class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

    def parse(command)
        raise Exception, "Not implemented"
    end
end

# interactive portion of the code to test each class

s = Synsets.new
result = s.load("inputs/public_synsets_valid")
print result

search = s.lookup(3)
print search
print "\n"

