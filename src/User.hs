{-# LANGUAGE MultiParamTypeClasses #-}
module User where
import Database.HDBC
import Data.Convertible
import Data.Char
import Data.List
import Database.HDBC.Sqlite3
import Database.HDBC.SqlValue
import Crypto.PasswordStore
import qualified Data.ByteString.Char8  as BS
--Using Crypto.PasswordStore package for password hashing
--Using Database.HDBC package for abstraction layer between Haskell programs and SQL relational databases

type UserIdentifier = String
data Role = Student Integer | TA Integer Integer | Professor  deriving (Eq,Show,Ord)
data User = User {  identifier::UserIdentifier
                 ,  email     ::String
                 ,  pwdHash   ::String
                 ,  role      ::Role
                 }  deriving (Show)
--Each user is uniquely determined by identifier
--So in instance of Eq for User we only have to check identifier
instance Eq User where
    User id1 _ _ _ == User id2 _ _ _ = id1 == id2


--Because datatype Role is not instance of Convertible we have to write it on out own
--Basicly lot of coverting and problems between ByteString and String
instance Convertible Role SqlValue where
    safeConvert (Student xs) = return(SqlString ("Student "++ show xs))
    safeConvert (TA xs ys )  = return (SqlString("TA "++ show xs ++" " ++show ys ))
    safeConvert Professor  = return (SqlString ("Professor"))






--OUR TABLE IN SQL DATABASE IS CALLED user

--This function creates table od Users in given database in sqlite3 (in mac os x)
createTableInDatabase::String->IO()
createTableInDatabase database =  do
    conn <- connectSqlite3 database
    run conn "create table user (identifier String NOT NULL,email String,pwdhash String,role String,PRIMARY KEY (identifier));" []
    commit conn
    disconnect conn
    return ()


--Takes a user identifier,email,password and role
--Performs password hashing and stores the user into database,
-- returning a filled User.

createUser::UserIdentifier->String->String->Role->IO User
createUser userId  email pass role = do
    --Connecting to my database
    conn <- connectSqlite3 "user.db"
    --placeholder==?
    stmt <- prepare conn "INSERT INTO user VALUES (?,?,?,?)"
    
    --password hashing (17 is the strength of the password)
    passwordHash <- makePassword (BS.pack pass) (17::Int)
    

    let passHash = BS.unpack passwordHash
        newUser = (User userId email passHash role)
    
    --Executing query and inserting one new user
    handleSqlError (execute stmt [toSql userId,toSql email,toSql passHash,toSql role])
    
    --Commiting changes to bas
    commit conn
   
    disconnect conn
    return (newUser)


--Updates a given user.Identifies it by the UserIdentifier in the User
--and overwrites the database entry with the values in the User structure
updateUser::User->IO ()
updateUser (User userId email1 pwdHash1 role1) = do
    conn <- connectSqlite3 "user.db"

    stmt <- prepare conn ("SELECT * FROM user where identifier= ?")

    execute stmt [toSql userId]

    checkIfExist <- fetchRow stmt

    case checkIfExist of 
        Nothing ->  throwSqlError (SqlError "error" 0 "The User doesnt exist" )

        Just _  -> do
            stmt1     <- prepare conn "UPDATE user SET email=?,pwdhash=?,role=? WHERE identifier =?"
            ifChanged <- execute stmt1 [toSql email1 , toSql pwdHash1 , toSql role1,toSql userId]
            if (ifChanged==0) then throwSqlError (SqlError "error" 1 "The User hasnt been modifyed" ) else do

                commit conn 
                disconnect conn 
                return()
            

deleteUser::UserIdentifier->IO ()
deleteUser userId = do
    conn <- connectSqlite3 "user.db"

    stmt <- prepare conn "DELETE FROM user WHERE identifier = ?"

    checkIfDelete <- execute stmt [toSql userId]
    if(checkIfDelete==0)  then throwSqlError (SqlError "error" 0 "User doesnt exist") else do
        commit conn 
        disconnect conn 
        return ()






--Expect string and calculate specific Role type from it
--"Student 2015 "   --- > Student 2015 ::Role
returnRole::String->Role
returnRole xs = case (wt!!0) of 
    "Student"   -> Student (read (wt!!1)::Integer)
    "TA"        -> TA (read (wt!!1)::Integer) (read (wt!!2)::Integer)
    "Professor" -> Professor 
    where 
        wt = words xs

--Takes a list of SqlValue from database user
--This list has specific type and we know how it would lool so we can pattern match
createUserFromRow::[SqlValue]->User
createUserFromRow [SqlInt64 a,SqlByteString b,SqlByteString c,SqlByteString d] = 
    User (show a) (BS.unpack b) (BS.unpack c) (returnRole (BS.unpack d)) 


--List all the user from database user
listUser::IO [User]
listUser = do
    conn <- connectSqlite3 "user.db"
    list <-quickQuery' conn "SELECT * FROM user" []
    disconnect conn
    return(map createUserFromRow list)



--Takes a role and user and anc calculates if the user has given role
checkIfGivenRole::Role->User->Bool
checkIfGivenRole xs (User {role = ts}) = if xs==ts then True else False 

--Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole xs = do
    list <-listUser
    return(filter (checkIfGivenRole xs) list)





--Fetches a single user by identifier
getUser :: UserIdentifier -> IO User
getUser userId =  do
    listOfUser <- listUser
    case (find ((== userId) . identifier) listOfUser) of
        Nothing -> throwSqlError (SqlError "error" 0 "User doesnt exist")

        Just x  -> return(x)


--Checks whether the user has a role of AT LEAST X in a given academic year.
--Function will check all the possible solutions
--Its not very elegant but its effective
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear (User _ _ _ (Student x)) (Student y) year = (x==y) && (y==year)
isRoleInYear (User _ _ _ (Student x)) (TA y1 y2) year  = False
isRoleInYear (User _ _ _ (Student x)) (Professor) year = False
isRoleInYear (User _ _ _ (TA x1 x2)) (Student y) year  = False
isRoleInYear (User _ _ _ (TA x1 x2)) (TA y1 y2) year   = (x1>=year && x2<=year) && (y1>=year && y2<=year)
isRoleInYear (User _ _ _ (TA x1 x2)) Professor year    = False
isRoleInYear (User _ _ _ Professor) (Student y) year   = False 
isRoleInYear (User _ _ _ Professor) (TA y1 y2) year    = False
isRoleInYear (User _ _ _ Professor) Professor  year    = True





