module AssignmentAndSubmission where
import Data.Char
import Data.String
import Data.Time
import System.Directory
import qualified  Data.Text as Txt
import qualified  Data.Text.IO as TxtIO



--An assignment type
type UserIdentifier = String
type Year = Integer
data Type = Homework | Exam | Project deriving Show

--An assignment configuration data structure
data Configuration = Configuration { published    :: UTCTime 
                                   , deadline     :: UTCTime
                                   , lateDeadline :: UTCTime
                                   , files        :: [String]
                                   , minScore     :: Double 
                                   , maxScore     :: Double 
                                   , required     :: Double
                                   }deriving Show

--An assignment descriptor 
data Assignment = Assignment { year  ::   Year
                             , type1 ::   Type                                   
                             , number ::  Int
                             } deriving Show


--Submission data structure
--It has userIdentifier and uploded files
data Submission = Submission { assignment    :: Assignment
                             , userId        :: UserIdentifier
                             , filesUploded  :: [FilePath]
                             } deriving Show


listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions (Assignment year type1 number) = do
    --Setting working directory for an assignment data type we get 
    setCurrentDirectory ("/Users/Filip/Documents/Haskell/AssignmentData/" ++  show year ++"/" ++ show type1 ++"/" ++ show number)

    --Folder are not files,so they are without dot (thats how me made AssignmentData directory)
    currentDir <- getCurrentDirectory
    folders <- listDirectory (currentDir)
    
    --Folders represent userIdentifiers
    return(filter isFolder folders)
    where
        isFolder xs = null $ filter (=='.') xs 

--Views a signle submission in detail
getSubmission :: Assignment -> UserIdentifier ->IO Submission
getSubmission (Assignment year type1 number ) userID = do
    setCurrentDirectory ("/Users/Filip/Documents/Haskell/AssignmentData/" ++  show year ++"/" ++ show type1 ++"/" ++ show number++"/"++userID)

    currentDir <- getCurrentDirectory
    files <- listDirectory currentDir

    --In submission we will include only submitted files and what arewith postfix (.hs)
    return(Submission t userID (filter onlyHs files))
    where
        onlyHs = (=="sh.").take 3 . reverse 
        t = (Assignment year type1 number )


--Creates a new assignment from Assignment,configuration and PDF file
--The PDF file is moved to the proper assignment directory
--Configuration will be stored in Config.txt file 
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment (Assignment year type1 number) (Configuration a b c d e f g) pdfAssign = do
    let workingDirectory = "/Users/Filip/Documents/Haskell/AssignmentData/"
    setCurrentDirectory (workingDirectory)

    --creating directories for assignment we get
    createDirectory (workingDirectory++"/"++show year)
    createDirectory (workingDirectory++"/"++show year++"/"++show type1)
    createDirectory (workingDirectory++"/"++show year++"/"++show type1++"/"++show number)
    
    let assignmentDirectory = workingDirectory++"/"++show year++"/"++show type1++"/"++show number
    --Copying pdf file into proper directory 
    copyFile pdfAssign (workingDirectory++"/"++show year++"/"++show type1++"/"++show number)
    
    --writing configuration file
    setCurrentDirectory (assignmentDirectory)
    writeFile "Config.txt" ((show a)++"\n")
    writeFile "Config.txt" ((show b)++"\n")
    writeFile "Config.txt" ((show c)++"\n")
    return ()





--Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration (Assignment a b c) = do 
    let workingDirectory = "/Users/Filip/Documents/Haskell/AssignmentData/"
        assignmentDirectory = workingDirectory++"/"++show a++"/"++show b++"/"++show c
    setCurrentDirectory (assignmentDirectory)
    content <- readFile "Config.txt"
    let wt=lines content
        a = wt!!0
        b = wt!!1
        c = wt!!2
        d = wt!!3
        e = wt!!4
        f = wt!!5
        g = wt!!6
        published1    = read a :: UTCTime
        deadline1     = read b :: UTCTime
        lateDeadline1 = read c :: UTCTime
        files1        = words d
        minScore1     = read e :: Double
        maxScore1     = read f :: Double
        requiered1      = read g :: Double
    return (Configuration published1 deadline1 lateDeadline1 files1 minScore1 maxScore1 requiered1)


--Adds a solution file to the directory structure of an assignment
--I added one extra parametar for userIdentifier
upload :: Assignment -> Txt.Text -> String -> String -> IO (Maybe Submission)
upload (Assignment a b c) solution file userId = do
    let workingDirectory = "/Users/Filip/Documents/Haskell/AssignmentData/"
        assignmentDirectory = workingDirectory++"/"++show a++"/"++show b++"/"++show c
    setCurrentDirectory (assignmentDirectory)

    --Checking if user already uploded something if not,then the user directory for submission will be created
    let userSolutionDirectory = (assignmentDirectory++"/"++ userId)
    ifExist <- doesDirectoryExist userSolutionDirectory
    case ifExist of
        True  -> return ()
        False -> createDirectory userSolutionDirectory 


    --getting configuration
    config <- getConfiguration (Assignment a b c)
    let listOfPermitedFiles = files config 
    
    --Cheking if the given file is in permited list
    --If not return Nothing
    case file `elem` listOfPermitedFiles of
        False -> return (Nothing)
        True  -> do 
            setCurrentDirectory userSolutionDirectory
            TxtIO.writeFile file solution
            uplodedFiles <- (getDirectoryContents (userSolutionDirectory))
            return ( Just (Submission (Assignment a b c) userId uplodedFiles))




--lists the files contained in a submission
listFiles::Submission -> IO [FilePath]
listFiles (Submission assign userID files) = return files


--Computes a file path for a submission
getSubmissionPath::Submission -> FilePath
getSubmissionPath (Submission (Assignment a b c) userID files) = "/Users/Filip/Documents/Haskell/AssignmentData/"++ show a ++"/" ++show b ++"/"++show c ++"/"++userID



