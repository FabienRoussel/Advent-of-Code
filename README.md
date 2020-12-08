# Scala Starter Kit

Hi ! You'd like to learn Scala and have a clean and working project easily, you're at the right place. 

First, I'll explain Scala project structures. Then, I go through standard libraries, like `pureconfig` or `scalatest`

# Project Structure

## Global structure

```
.
├── README.md                               <-- Casual README 
├── build.sbt                               <-- contains project properties like dependencies (like a `package.json`)
├── project                                 <-- contains everything specific to sbt
│   ├── build.properties                    <-- usually contains sbt version
│   └── project                             <-- inner folder for sbt
│   └── target                              <-- inner folder containing compiled code for sbt
├── src                                     <-- contains your scala sources
└── target                                  <-- contains compiled code from scala compilation
```

## Source structure

```
.
├── main
│  ├── resources
│  │   ├── application.conf    <-- put your application configuration here
│  │   └── reference.conf      <-- put your default configuration here
│  └── scala
│      └── starterkit          <-- package containing all your source code
└── test
    ├── resources
    │   └── application.conf   <-- contain your test config
    └── scala
        └── starterkit         <-- package containing all your test code

```

NB: `application.conf` VS `reference.conf`

in short: 
- application.conf --> use for local conf (dev conf)
- reference.conf --> used for default conf, usually filled by ansible

If you want more infos, check 
- [this website](https://stackoverflow.com/a/49996692)
- [and this website](https://github.com/lightbend/config#standard-behavior)

# How to run this project

I suppose you use IntelliJ IDEA with Scala Plugin.

To run the project, go to [StarterApp](src/main/scala/starterkit/StarterApp.scala) and click the play button.
To run the tests, go to [StarterAppSpec](src/test/scala/starterkit/StarterAppSpec.scala) and repeat the same process.

You can use the `sbt shell` available at the bottom of the UI. 

Inside that shell, use 
- `run` to run the project
- `test` to run the tests
- `clean` to remove `target` folder
- `compile` to compile the projects

# Shipped libraries  

## PureConfig
[PureConfig](https://pureconfig.github.io/) is a cool library to handle conf in Scala.

The idea is to create a Algebraic Data Type (ADT) which is equivalent to the `.conf` file structure.

I invite you to check [ApplicationConfig](src/main/scala/starterkit/ApplicationConfig.scala) for more details.

## Test library

Standard test library is [ScalaTest](https://www.scalatest.org/)