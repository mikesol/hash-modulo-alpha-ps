module TmpDir where

import Prelude

import Data.JSDate (now, toISOString)
import Effect (Effect)
import Node.FS.Perms as Perms
import Node.FS.Sync as FS
import Node.OS (tmpdir)
import Node.Path as Path

createTempDirectory :: Effect String
createTempDirectory = do
  td <- tmpdir
  date <- now
  parsed <- toISOString date
  let path = Path.concat [ td, "benchmarks", parsed ]
  FS.mkdir' path { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }
  pure path
