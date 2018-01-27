# Project 6 Security
# By Anna Blendermann (ablender, 114474025)

require "securerandom"
require "sqlite3"

#
# Constants
#

AVATAR_DIR = "./public/avatars/"
SESSION_BITS = 128
DATE_FMT = "%d %h %G at %R"

#
# Helpers
#

# upload_file : String, File -> Boolean
# Uploads a file to the server (failing if it already exists)
# and returns success status.
def upload_file(filename, file)
	path = "#{AVATAR_DIR}/#{filename}"
	return false if File.exists? path

	File.open(path, "wb") { |f| f.write(file.read) }
	true
end

#
# Sessions
#
# The Sessions module handles session identifiers, integers
# which uniquely identify a user on a particular client.
# Once issued, these identifiers will automatically be stored
# in a cookie on the user's browser. We keep track of which
# identifiers belong to whom.
#

module Sessions

	# issue_session : () -> Integer
	# Returns cryptographically secure session identifier.
	def issue_session
		SecureRandom.random_number(2 ** SESSION_BITS)
	end

	# assign_session : String -> Integer
	# Assigns a session identifier to a user and returns it.
	def assign_session(user)
		@sessions[user] = issue_session
	end

	# revoke_session : String -> Integer
	# Revokes session of a user and returns it.
	def revoke_session(user)
		@sessions.delete user
	end
end

#
# Tokens
#
# The Tokens module generates and assigns tokens which are
# attached to user input forms as a hidden field. A new token
# should be assigned to a user on GET requests (where the
# page has a form).
#

module Tokens

	# issue_token : () -> Integer
	# Returns cryptographically secure token.
	def issue_token
		SecureRandom.random_number(2 ** SESSION_BITS)
	end

	# assign_token : String -> Integer
	# Assigns a token identifier to a user and returns it.
	def assign_token(user)
		@tokens[user] = issue_token
	end
	
	# authorize_token : String -> Integer
	# Authorizes that the current user has a token.
	def authorize_token(user, token)
		@tokens[user] == token
	end
end

#
# Access
#
# The Access module handles user authentication and authorization.
# We verify the user's identity by matching up the session identifier
# the user gives us (as a cookie) to the identifier we issued for that
# user at login.
#

module Access

	# authenticate : String, String -> (String or NilClass)
	# If credentials are valid, assigns session identifier to user
	# and returns identifier, otherwise returns nil.
	def authenticate(user, passwd)
		
		# p6 -> get password and salt from the database
		query = %{
			SELECT password,salt
			FROM users
			WHERE user = ?
		}
		result = @db.execute(query,[user])[0]
		
		# p6 -> return nil if user isn't in the database
		if (!result)
			then return nil
		else 
			# p6 -> set vars for original password and salt
			dbpasswd = result[0]
			salt = result[1]
		
			# p6 -> add salt to passwd, hash, and verify
			saltedpwd = passwd + salt
			hashedpwd = Digest::SHA256.hexdigest(saltedpwd)
		end
		
		unless ((not_found user) || (dbpasswd != hashedpwd))
			assign_session user
		end
	end

	# authorize : String, Integer -> Boolean
	# Returns whether user was issued given session identifier.
	def authorize(user, session)
		session == @sessions[user]
	end

	# revoke : String, String -> (Integer or NilClass)
	# Revokes a user's session so long as given session identifier
	# is valid. Returns the session if valid, otherwise nil.
	def revoke(user, session)
		revoke_session user if authorize(user, session)
	end
end

#
# User
#
# The User module handles all user-creating, modifying, and
# data retrieval actions.
#

module User

	# not_found : String -> (String or NilClass)
	# Returns the user's name if not found (for display on the user
	# not found page), otherwise nil.
	def not_found(user)
		
		# p6 -> sanitized user to prevent xss attack
		user = Rack::Utils.escape_html(user)
		display_name = user
		
		# p6 -> changed string var "user" to data variable
		query = %{
		SELECT User
		FROM Users
		WHERE User = ?
		}
		# p6 -> modified execute to prevent sql injection
		@db.execute(query, [user]) { |user| display_name = nil }
		display_name
	end

	# register : String, String, File, String, String -> Boolean
	# Registers a new user if they don't already exist and
	# password and confirm password match. Returns success status.
	def register(user, filename, file, password, confirm)
		return false if password != confirm

		# p6 -> sanitize user and filename for xss attack
		user = Rack::Utils.escape_html(user)
		filename = Rack::Utils.escape_html(filename)
		
		# upload file
		upload_file(filename, file)
		
		# add salt to password and hash the result
		salt = SecureRandom.hex(32)
		passwd2 = password + salt
		passwd3 = Digest::SHA256.hexdigest(passwd2)
		
		# p6 -> changed vars (user, password, filename) to data vars
		query = %{
		INSERT INTO Users(User, Password, Avatar, Salt)
		VALUES (?, ?, ?, ?)
		}
		# p6 -> modified execute to prevent sql injection
		@db.execute(query, [user,passwd3,filename,salt])
		true
	end

	# get_prefs : String -> (Hash or NilClass)
	# Gets preferences of given user or nil if non-existent.
	def get_prefs(user)
		
		# p6 -> changed string var "user" to data variable
		query = %{
		SELECT Avatar, Description
		FROM Users
		WHERE User = ?
		}
		# p6 -> modified to db.prepare to prevent sql injection
		query = @db.prepare "SELECT Avatar, Description
		FROM Users WHERE User = ?"
		
		query.bind_params user
		
		results = query.execute
		results.each do |row|
			return {
				:avatar => row[0],
				:description => row[1]
			}
		end
	end

	# update_prefs : String, Integer, String, String -> Boolean
	# Update preferences of given user returning success status.
	def update_prefs(user, session, description, token)
		
		# p6 -> authorize the user session and token
		if (!authorize(user,session) || !authorize_token(user,token))
			return false
		end
		
		# p6 -> sanitize description for xss attack
		description = Rack::Utils.escape_html(description)
		
		# p6 -> changed vars (user, description) to data vars
		query = %{
		UPDATE Users
		SET Description = ? 
		WHERE User = ?
		}
		# p6 -> modified execute to prevent sql injection
		@db.execute(query, [description,user])
		true
	end
end

#
# Epsilons
#
# The Epsilons module is concerned with creating and retrieving
# epsilons. These are the little messages that make up communication
# on our network.
#

module Epsilons

	# publish_epsilon : String, Integer, String, String -> Boolean
	# Publish epsilon from user with given content. Returns
	# success status.
	def publish_epsilon(user, session, content, token)
		timestamp = Time.now.to_i
		
		# p6 -> authorize the user session and token
		if (!authorize(user,session) || !authorize_token(user,token))
			return false
		end
		
		# p6 -> sanitize content for xss attack
		content = Rack::Utils.escape_html(content)
		
		# p6 -> changed vars (user, content, timestamp) to data
		query = %{
		INSERT INTO Epsilons(User, Content, Date)
		VALUES (?, ?, ?)
		}		
		# p6 -> modified execute to prevent sql injection	
		@db.execute(query, [user,content,timestamp])
		true
	end
	
	# get_epsilons : (String or NilClass) -> Array
	# Returns array of all epsilons from the given user or all epsilons
	# in the system if given nil.
	def get_epsilons(user)
		epsilons = []
		query = %{
		SELECT Epsilons.User, Avatar, Content, Date
		FROM Epsilons
		JOIN Users ON Epsilons.User = Users.User
		#{"WHERE Epsilons.User = \"#{user}\"" if user}
		ORDER BY Epsilons.ID DESC
		}

		@db.execute(query) do |eps|
			date_str = Time.at(eps[3]).strftime(DATE_FMT)
			epsilons << {
				:user => eps[0],
				:avatar => eps[1],
				:content => eps[2],
				:date => date_str
			}
		end

		epsilons
	end

	# all_epsilons : () -> Array
	# Returns all epsilons.
	def all_epsilons
		get_epsilons nil
	end
end

#
# Controller
#
# The Controller class defines a single interface to all the
# previously defined modules. It holds the server-side state
# of the application as well as the database handle.
#

class Controller
	include Sessions
	include Tokens
	include Access
	include User
	include Epsilons

	# Leave @db as attr_accessor for tests
	attr_accessor :db

	def initialize
		@db = SQLite3::Database.new "data.db"
		@sessions = {}
		@tokens = {}
	end
end
