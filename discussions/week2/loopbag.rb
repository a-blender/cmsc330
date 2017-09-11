# This is the loopbag class
# discussion week2

class Loopbag

	# constructor for the loopbag class
	def initialize(s)
		@capacity = s
		@a = []
	end

	# adds the given item to loopbag
	def insert(item)
		
		if self.size == @capacity
			@a.pop
		end
		@a.push item
	end

	# returns the number of items in this loopbag
	def size
		@a.size
	end

	# returns true if this loopbag is empty and false otherwise
	# question mark - indicates the method returns a boolean value
	def empty?
		@empty?
	end

	# insert the union of two bags into a separate bag
	def union(lb)
		
		for lb.a.each do |x|
			if (!self.contains x)
				self.insert x
			end
		end	
	end

	# write an iterator to iterate through the bag
	def each

		@a.each do |x|
			yield x
		end
	end
end

