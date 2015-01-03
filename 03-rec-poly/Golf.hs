{-# OPTIONS_GHC -Wall #-}

module Golf where

-- First list is the input itself, second list contains only every second
-- element of the input list, etc.
-- > skips "ABCD"        == ["ABCD", "BD", "C", "D"]
-- > skips "hello!"      == ["hello!", "el!", "l", "o", "!"]
-- > skips [1]           == [[1]]
-- > skips [True, False] == [[True,False], [False]]
-- > skips []            == []
skips :: [a] -> [[a]]
skips _ = []
