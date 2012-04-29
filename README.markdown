# Code Monitor: Tells you when you write bad code

`codemonitor` is a development tool that automatically runs arbitrary commands
when files change and displays the output of the commands graphically.

`codemonitor` can be used for example to automatically run your tests when you
save a source file.

## Getting started

`codemonitor` is written in Haskell and runs on Linux. (The only Linux
dependency at the moment is inotify.)

It depends on gtk and hinotify haskell packages.

Please contact me if you can't install all dependencies.

When all dependencies are intalled, you should be able to run the program like
this:

    cd ${PATH_TO_CODEMONITOR}
    ./run-app

## Usage

### Specify a configuration

First you need to specify a configuration file that tells `codemonitor` where
to look for changes and what commands to run. The config for code monitor
itself looks like this:

    .
    tests \.hs$ sh run-tests
    lint \.hs$ hlint .

The first line is the path to the directory to watch for file changes.

The rest of the lines are specifications of commands. They have the format
"{id} {regexp} {executable} {arguments}".

The id must be unique among the commands.

The regexp is used to only run this command if the file that was changed
matches the regexp.

### Running it

    ${PATH_TO_CODEMONITOR}/src/Main monitor.config

# Help

Please contact me if you have questions or problems.
