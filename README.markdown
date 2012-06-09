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

`codemonitor` is uploaded to
[Hackage](http://hackage.haskell.org/package/codemonitor)
for easy installation. Once you have installed the
[Haskell Platform](http://hackage.haskell.org/platform/) (
`yum/apt-get install haskell-platform`), you can install `codemonitor` like
this:

    # You need to have gtk installed first. Install it with you package manager
    # before you continue. You might need dev libraries as well to be able to
    # install the Haskell bindings.

    # The install script depends on programs in this directory, so we must
    # export it here. It is a good idea to always put this on your path so that
    # cabal always refers to the cabal in ~/.cabal/bin.
    export PATH="~/.cabal/bin:$PATH"

    # Fore some reason, cabal does not install this dependency, so we have to
    # install it manually.
    cabal install gtk2hs-buildtools

    # Then we can install codemonitor.
    cabal install codemonitor

After this, `codemonitor` should be installed in `~/.cabal/bin/codemonitor`.

If you have an old version of `cabal`, you might need to upgrade it. Before you
install `codemonitor` you need to run this:

    # This will install a new cabal in ~/.cabal/bin/, so make sure the path is
    # exported when you try to install codemonitor.
    cabal install cabal-install

To install a new version of `codemonitor`, run these commands:

    cabal update
    cabal install codemonitor

Please contact me if you have trouble installing.

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

After the application has been installed, you can run it like this, passing the
path to the configuration file:

    ~/.cabal/bin/codemonitor monitor.config

### Passing configuration to stdin

You can also pass the configuration directly to stdin. It is useful if you only
want a single file to run codemonitor. It can look something like this:

    #!/bin/sh
    ~/.cabal/bin/codemonitor <<EOF
    .
    tests \.hs$ sh run-tests
    lint \.hs$ hlint .
    EOF

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
