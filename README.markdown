# Code Monitor: Tells you when you write bad code

`codemonitor` is a development tool that automatically runs arbitrary commands
when files change on disk and displays the output of the commands graphically.

`codemonitor` can be used for example to automatically run your tests when you
save a source file.

I have recorded a demo that you can watch here:
http://www.youtube.com/watch?v=oucd1p9cjrI&hd=1

## Getting started

`codemonitor` is written in Haskell and runs on Linux. (The only Linux
dependency though is inotify.)

It depends on gtk2hs and hinotify Haskell packages.

Please contact me if you can't figure out how to install dependencies.

When all dependencies are installed, you should be able to run the program like
this:

    cd ${PATH_TO_CODEMONITOR}
    ./dev-setup
    ./run-tests
    ./run-app

## Usage

### Specify a configuration

First you need to specify a configuration file that tells `codemonitor` in
which directory to look for file changes and what commands to run. The
configuration for `codemonitor` itself looks like this:

    .
    tests \.hs$ sh run-tests
    lint \.hs$ hlint .

The first line is the path to the directory to watch for file changes.

The rest of the lines are specifications of commands. They have the format
"{id} {regexp} {executable} {arguments}".

The id must be unique among the commands.

The regexp is used to only run this command if the file that was changed
matches the regexp. (Files ending with .hs in the example.)

### Running it

After the application has been compiled, you can run it like this, passing the
path to the configuration file:

    ${PATH_TO_CODEMONITOR}/dist/build/codemonitor/codemonitor monitor.config

# Known bugs/limitations

## Parallel jobs

If a file changes while a job is running, a new job is started immediately. The
problem is that the old job is not stopped. So you get two jobs running in
parallel. That is very problematic when compiling code for example since the
parallel jobs touch the same files.

A workaround for this limitation is to create a shell script that runs the
command you want to run, and before it does that, it kills the old version of
itself. It can be done with a snippet like this:

    pidfile=lastpid.txt
    if [ -e $pidfile ]; then
        kill -9 $(cat $pidfile)
        rm $pidfile
    fi
    echo $$ > $pidfile

    ... run your command here ...

    rm -f $pidfile

# Help

Please contact me if you have questions or problems.

[![Build Status](https://secure.travis-ci.org/rickardlindberg/codemonitor.png)](http://travis-ci.org/rickardlindberg/codemonitor)
