
{--



--test12 = do return ()
   --runEffect $ P.stdinLn >-> (wait_for_piece_of_data []) (>>=) (\x -> (x >->  P.stdoutLn))
   --return ()
   --weed_data_from_input 3
   --runEffect $ for stdinLn (lift . putStrLn . ("-----" ++))
     --runEffect $ P.stdinLn >-> wait_for_piece_of_data



test11 = do
   getArgs >>=
     (\str -> -- $ unwords str


     (data_file' $ data_file $ tag_DMap str) >>= \x -> iterate_all_data (tag_DMap str) $
        {-- map (toStringTable . stack_output . (apply_processors ([(identity_f_dyn)])) .
                                       stringToFloatList_mn_dyn 1 2) $--} weed_data_from_input 3 x
     )


     --(data_file' $ data_file (tag_DMap str)) >>=
     --                       \x -> putStrLn $ show $ weed_data_from_input 3  $
     --                                                              stringToFloatList_mn_dyn 1 2 x)




test10  = do
    putStr $ show $ weed_data_from_input 3
      [" 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n 9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n",
       " 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n",
       " 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n"
      ]





test9 = do
     --putStrLn $ show $ map (\(x, y) -> ((fromDyn x 0::Int), (fromDyn y 0::Int)) )  $ head $
     --      apply_processors [(identity_i_dyn)] [(toDyn (1::Int), toDyn (2::Int))]

     putStrLn ${-- show $ (\x-> map (\(Pd y _) -> y) x)--}toStringTable $ stack_output $
                           apply_processors [(identity_i_dyn),
                                             (identity_f_dyn),
                                             (derivative_i_dyn),
                                             (derivative_f_dyn)
                                            ]                  [(toDyn (1::Int), toDyn (101::Int)),
                                                                (toDyn (2::Int), toDyn (102::Int)),
                                                                (toDyn (3::Int), toDyn (103::Int)),
                                                                (toDyn (4::Int), toDyn (104::Int)),
                                                                (toDyn (5::Int), toDyn (105::Int)),
                                                                (toDyn (6::Int), toDyn (106::Int)),
                                                                (toDyn (7::Int), toDyn (107::Int)),
                                                                (toDyn (8::Int), toDyn (108::Int)),
                                                                (toDyn (9::Int), toDyn (109::Int)),
                                                                (toDyn (10::Int), toDyn (110::Int))
                                                               ]



test8 = do

  getArgs >>=
     (\str -> putStrLn $ show $ tag_DMap str -- $ unwords str
     )

  --getArgs >>=
   --  (\str -> -- $ unwords str

    -- data_process (tag_DMap str) [1..100] identity ioStringToIntList intListToIoString
     --)

  getArgs >>=
     (\str -> -- $ unwords str

     (data_file' $ data_file_range (tag_DMap str) [1..2]) >>=
                            \x -> putStrLn $ show $ map stringToIntList x    )


test7x=do

   --getArgs >>=
  --      (\str -> putStrLn $ show $ tag_DMap str -- $ unwords str
   --     )

 (\(x,y) -> putStrLn ( y) )$  splitAt
                                         ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

   --putStrLn $ (\(x,y) -> [(read x)..(read $ tail y)]) $  splitAt
   --                                     ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

test7=do
  getArgs >>=
    (\str -> putStrLn $ show $ str
    )

  getArgs >>=
    (\str -> putStrLn $ show $ tag_DMap [] -- $ unwords str
    )


--
  getArgs >>=
    (\str -> putStrLn $ show $ list_arguments $
              str
    )

  getArgs >>=
    (\str -> putStrLn $ show $
              (DMap.member "test" $ tag_DMap []) --["test", "test"]--flags
    )

  getArgs >>=
    (\str -> putStrLn $ show $
              take 2 "--data-file"  == "--" && elem (drop 2 "--data-file") options
    )

-- runCommand $ "echo -e " ++ (gnuplot_command ++ x) ++ "|gnuplot -persist"

--}
