{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "scrypt"
, dependencies =
  [ "aff", "arraybuffer-types" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
