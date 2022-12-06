-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import FASJSON

main :: IO ()
main = do
  res <- queryFasjson fedoraFASJSON "users/petersen/" noData
  print (res :: Object)
