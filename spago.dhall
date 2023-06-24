{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut-generic"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "bigints"
  , "console"
  , "control"
  , "debug"
  , "deku"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "gen"
  , "hyrule"
  , "integers"
  , "js-date"
  , "lcg"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-os"
  , "node-path"
  , "node-process"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "random"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
