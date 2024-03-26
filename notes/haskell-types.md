

GADTS
=====

[Simpler and Safer API Design Using GADTs](https://chrispenner.ca/posts/gadt-design)
----------------------------------------

CSVs are either Int indexed or String indexed

You shouldn't be able to ask for the headers of an Int-indexed CSV. Trying to get `[3]` of a header-indexed CSV should fail to type-check

You can do this with a typeclass IsCSV

    class IsCSV c where
        -- A type family to specify the "indexing" type of the CSV
        type Index c :: Type
        -- Try parsing a CSV of the appropriate type
        decode :: String -> Maybe c

        getColumnByIndex  :: Index c -> c -> Maybe [String]
        getColumnByNumber :: Int     -> c -> Maybe [String]
        getRow :: Int -> Row c

But you get type ambiguity errors

    decode input >>= getColumnByIndex ("Name" :: String)
    decode @([String], [[String]]) input >>= getColumnByIndex "Name"

Simple GADTs allow you to avoid impossible branches of an ADT

    data IntOrString a where
        AnInt :: Int -> IntOrString Int
        AString :: String -> IntOrString String

    -- You don't have to specify the other branch!
    toInt :: IntOrString Int -> Int
    toInt (AnInt n) = n

    -- But you CAN choose to implement all of them
    toString :: IntOrString a -> String
    toString (AnInt n) = show n
    toString (AString s) = s

So, now, the CSV Type

    data CSV index where
        NamedCsv    :: [String] -> [[String]] -> CSV String
        NumberedCsv ::             [[String]] -> CSV Int

    -- A side-effect of using GADTs is that we need to use standalone deriving 
    -- for our instances.
    deriving instance Show (CSV i)
    deriving instance Eq   (CSV i)

    data CSVType i where
        Named :: CSVType String
        Numbered :: CSVType Int

    deriving instance Show (CSVType i)
    deriving instance Eq (CSVType i)

And the functions can be implemented accordingly. The compiler knows. 

    decode :: CSVType i -> String -> Maybe (CSV i)
    getColumnByIndex :: i -> CSV i -> Maybe [String]
    getColumnByNumber :: Int -> CSV i -> Maybe [String]
    getHeaders :: CSV String -> [String]
