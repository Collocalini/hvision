
module Global (

eol_char,

sortLt_by_y,
sortGt_by_y,
sortLt_by_x,
sortGt_by_x,

read_file_if_exists,

) where


eol_char = "\n"

{-- ================================================================================================
================================================================================================ --}
sortLt_by_y (_, ly) (_, ry)
  | ly < ry = GT
  | ly > ry = LT
  | ly == ry = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
sortGt_by_y (_, ly) (_, ry)
  | ly < ry = LT
  | ly > ry = GT
  | ly == ry = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
sortLt_by_x (lx, _) (rx, _)
  | lx < rx = GT
  | lx > rx = LT
  | lx == rx = EQ
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
sortGt_by_x (lx, _) (rx, _)
  | lx < rx = LT
  | lx > rx = GT
  | lx == rx = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
read_file_if_exists :: FilePath -> IO String
read_file_if_exists [] = do return ""
read_file_if_exists name  = do
       --handle <- openFile name ReadMode
       --c <- hGetContents handle
      -- hClose handle
       --c <- readFile name
       readFile name
       --return c

-------------------------------------------------------------


--data FileDataType = Text|Image|Video


{-- ================================================================================================
================================================================================================ --}
{--read_file_if_exists_ :: FilePath -> FileDataType -> IO String
read_file_if_exists_ [] = do return ""
read_file_if_exists_ name  = do
       --handle <- openFile name ReadMode
       --c <- hGetContents handle
      -- hClose handle
       --c <- readFile name
       readFile name
       --return c
--}
-------------------------------------------------------------
































